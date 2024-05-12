/*
 * Copyright (c) 2024 Toivo Henningsson
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none

`include "common.vh"

module scheduler #( parameter LOG2_NR=3, REG_BITS=8, NSHIFT=2, PAYLOAD_CYCLES=8 ) (
		input wire clk, reset,

		// Once inst_valid has become 1, it and the instruction parameters must remain stable
		// until inst_done is raised
		input wire inst_valid,
		output wire inst_done,

		output wire reserve_tx,

		input wire wide, wide2,
		input wire [`OP_BITS-1:0] op,
		input wire [`DEST_BITS-1:0] dest,
		input wire [LOG2_NR-1:0] reg_dest,
		input wire src1_from_pc, // Replace src1 with pc?
		input wire [`SRC_BITS-1:0] src,
		input wire [LOG2_NR-1:0] reg_src,
		input wire src_sext2,

		input wire addr_wide2,
		input wire [`ADDR_OP_BITS-1:0] addr_op,
		input wire [LOG2_NR-1:0] addr_reg1,
		input wire [`SRC_BITS-1:0] addr_src,
		input wire [LOG2_NR-1:0] reg_addr_src,
		input wire addr_src_sext2,
		input wire autoincdec, addr_just_reg1,

		input wire update_carry_flags, update_other_flags,

		input wire use_cc,
		input wire [`CC_BITS-1:0] cc,

		input wire [NSHIFT-1:0] imm_data_in,
		output wire next_imm_data,

		// PC control interface
		output wire block_prefetch, write_pc_now, ext_pc_next, // ext_pc_next may only be high when prefetch_idle is
		output wire [$clog2(2*REG_BITS / NSHIFT)-1:0] comp_counter,
		input wire prefetch_idle,
		output wire [NSHIFT-1:0] pc_data_out,
		input wire [NSHIFT-1:0] pc_data_in,

		// TX interface
		output wire tx_reply_wanted,

		output wire tx_command_valid,
		output wire [`TX_CMD_BITS-1:0] tx_command,
		input wire tx_command_started, // only high when starting a scheduler command
		input wire tx_active,

		output wire [NSHIFT-1:0] tx_data,
		input wire tx_data_next,
		input wire [$clog2(PAYLOAD_CYCLES)+1-1:0] tx_counter,
		input wire tx_done,

		// RX interface
		input wire rx_started,
		input wire rx_active,
		input wire [NSHIFT-1:0] rx_sbs,
		input wire rx_sbs_valid,
		input wire rx_data_valid, // only high when receiving scheduler data
		input wire [$clog2(PAYLOAD_CYCLES)+1-1:0] rx_counter,
		input wire rx_done, // only high when receiving scheduler data
		input wire [NSHIFT-1:0] rx_pins
	);
	// Evaluate condition code
	wire flag_c, flag_v, flag_s, flag_z;

	reg cc_ok; // not a register
	wire c_or_v = cc[`CC_BIT_SIGNED_INEQUALITY] ? flag_v : flag_c;
	always @(*) begin
		//if (cc[1:0] == 0) begin
		if ((cc & `CC_MASK_NSPECIAL) == 0) begin
			// Just always for now. TODO: More condition codes
			cc_ok = 1;
		end else begin
			if (cc[`CC_BIT_INEQUALITY]) begin
				if (cc[`CC_BIT_INEQUALITY_A]) cc_ok = c_or_v && !flag_z; // >
				else                          cc_ok = c_or_v;            // >=
			end else begin
				if (cc[`CC_BIT_S]) cc_ok = flag_s;
				else               cc_ok = flag_z;
			end
			if (cc[`CC_BIT_INVERTED]) cc_ok = !cc_ok;
		end
	end

	wire skip = use_cc && !cc_ok;
	wire execute = inst_valid && !skip;

	reg stage;
	reg command_active; // Has the current command been started yet? It's not enough if there is a previous command active.
	wire need_addr = (src == `SRC_MEM) || (dest == `DEST_MEM);
	wire addr_stage = (stage == 0) && need_addr;
	wire data_stage = !addr_stage;

	wire read_pc = data_stage && src1_from_pc;
	wire write_pc = data_stage && (dest == `DEST_PC);
	wire normal_data_stage = data_stage && !(dest == `DEST_PC);
	assign block_prefetch = execute && (read_pc || write_pc);
	assign write_pc_now = execute && write_pc && prefetch_idle;

	wire send_read = addr_stage || write_pc; // When write_pc is high, we do the first prefetch at the same time as writing PC
	wire will_write = (dest == `DEST_MEM);
	assign reserve_tx = execute && will_write;
	wire send_write = data_stage && will_write;
	wire send_command = execute && (send_read || send_write);

	// !((dest == `DEST_MEM) && (op == `OP_MOV)) should detect a mov [addr], reg,
	// but the timing is one cycle off we we set tx_reply_wanted = 0.
	// TODO: Fix the timing issues related to that TX messages have one more cycle of header compared to RX messages,
	// and reenable this.
	assign tx_reply_wanted = 1; //!((dest == `DEST_MEM) && (op == `OP_MOV));

	always @(posedge clk) begin
		if (reset || inst_done) begin
			stage <= 0;
		end else if (op_done) begin
			stage <= 1;
		end

		if (reset || op_done) begin
			command_active <= 0;
		end else if (tx_command_started) begin
			command_active <= 1;
		end
	end

	assign inst_done = (op_done && data_stage) || skip;

	wire [`SRC_BITS-1:0] curr_src = addr_stage ? addr_src : src;
	wire curr_src_imm = curr_src[`SRC_BIT_IMM];

	//wire wait_for_mem = data_stage && (src == `SRC_MEM);
	wire wait_for_mem = data_stage && (src == `SRC_MEM) && tx_reply_wanted;
	wire rmw_command = send_write && wait_for_mem;
	reg addr_lsb;
	wire wait_for_2nd_mem_half = !wide && addr_lsb;

//	wire alu_en = execute && (!send_command || tx_data_next) && (!wait_for_mem || rx_data_valid && (!wait_for_2nd_mem_half || rx_counter[$clog2(PAYLOAD_CYCLES)-1]));
	// send_command: wait for tx_active instead of tx_data_next, which should go high one cycle earlier.
	// Compensates for the added one cycle delay for the tx data through last_data_out, needed to transmit TX header.
	wire alu_en = execute && (!send_command || command_active) && (!wait_for_mem || rx_data_valid && (!wait_for_2nd_mem_half || rx_counter[$clog2(PAYLOAD_CYCLES)-1])) && (!read_pc || prefetch_idle);


	wire op_valid = alu_en;
	wire op_done;

	wire active;
	wire [NSHIFT-1:0] data_out;

	wire [`OP_BITS-1:0] operation = addr_stage ? (addr_op == `ADDR_OP_MOV ? `OP_MOV : `OP_ADD) : op;
	wire pair_op  = addr_stage ? 1'b1 : wide;
	wire pair_op2 = addr_stage ? addr_wide2 : wide2;
	wire sext2 = addr_stage ? addr_src_sext2 : src_sext2;
	wire external_arg2 = curr_src_imm | (curr_src == `SRC_MEM);
	wire [LOG2_NR-1:0] reg1 = addr_stage ? addr_reg1 : reg_dest;
	wire [LOG2_NR-1:0] reg2 = addr_stage ? reg_addr_src : reg_src;
	wire [NSHIFT-1:0] data_in = curr_src_imm ? imm_data_in : rx_pins; // Must wait for rx_pins to contain the right data
	assign next_imm_data = curr_src_imm && active;
	assign ext_pc_next = read_pc && active;
	wire update_reg1 = (addr_stage && autoincdec) || (data_stage && (dest == `DEST_REG));
	wire reverse_args = data_stage && (dest == `DEST_MEM) && !curr_src_imm; // Can we have an immediate source and a memory destination at the same time?

	// Should we double r8 in [r16 + r8] for 16 bit wide operations?
	// SRC_IMM2 case also captures autoincrement/decrement
	// TODO: Which pc operations should we double arg2 for?
	wire double_arg2 = (addr_stage && wide && (addr_src == `SRC_IMM7 || addr_src == `SRC_IMM2)) || write_pc;
	//wire double_arg2 = addr_stage && wide && !addr_wide2; // double addr_arg2 for most address calculations

	wire [2:0] arg2_limit_length = {(addr_src == `SRC_IMM7) && addr_stage, (src == `SRC_IMM6) && data_stage, (addr_src == `SRC_IMM2) && addr_stage};
	wire output_scan_out = addr_stage && addr_just_reg1;
	wire external_arg1 = src1_from_pc;

	ALU #( .LOG2_NR(LOG2_NR), .REG_BITS(REG_BITS), .NSHIFT(NSHIFT)) alu (
		.clk(clk), .reset(reset),
		.op_done(op_done), .op_valid(op_valid),
		.operation(operation),
		.external_arg1(external_arg1), .external_arg2(external_arg2),
		.pair_op(pair_op), .pair_op2(pair_op2), .sext2(sext2), .arg2_limit_length(arg2_limit_length),
		.reg1(reg1), .reg2(reg2), .update_reg1(update_reg1), .reverse_args(reverse_args), .double_arg2(double_arg2),
		.output_scan_out(output_scan_out),
		.update_carry_flags(update_carry_flags && normal_data_stage), .update_other_flags(update_other_flags && normal_data_stage),
		.flag_c(flag_c), .flag_v(flag_v), .flag_s(flag_s), .flag_z(flag_z),
		.active(active), .data_in1(pc_data_in), .data_in2(data_in), .data_out(data_out),
		.counter(comp_counter)
	);

	assign pc_data_out = data_out;

	// Delay output data by one cycle to allow time to start tx header.
	// Would it be better to delay the rx data coming in instead?
	reg [NSHIFT-1:0] last_data_out;
	always @(posedge clk) last_data_out <= data_out;

	// Assumes that we're receiving the right rx message; the one with the data we just asked for
	assign tx_command_valid = send_command && (!rmw_command || rx_started);
	assign tx_command = send_read ? `TX_HEADER_READ_16 : (wide ? `TX_HEADER_WRITE_16 : `TX_HEADER_WRITE_8);
	assign tx_data = last_data_out;

	always @(posedge clk) begin
		if (addr_stage && tx_counter == 0) addr_lsb <= tx_data[0];
	end
endmodule : scheduler
