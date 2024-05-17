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
		input wire update_dest,

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

		output wire load_imm16,
		input wire imm16_loaded,
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

	// Imm16 loading
	// -------------
	reg imm16_available;
	wire need_addr;

	wire need_imm16 = (src == `SRC_IMM16) || (need_addr && addr_src == `SRC_IMM16);
	assign load_imm16 = need_imm16 && !imm16_available;
	wire wait_for_imm16 = load_imm16;

	always @(posedge clk) begin
		if (reset || inst_done) begin
			imm16_available <= 0;
		end else if (imm16_loaded) begin
			imm16_available <= 1;
		end
	end

	// Evaluate condition code
	// -----------------------
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
	wire execute = inst_valid && !wait_for_imm16 && !skip;

	// Stage and command progression
	// -----------------------------
	wire op_done;
	reg stage;
	reg command_active; // Has the current command been started yet? It's not enough if there is a previous command active.

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

	assign inst_done = (op_done && data_stage) || (skip && !wait_for_imm16);

	assign need_addr = (src == `SRC_MEM) || (dest == `DEST_MEM); // Assume need_addr means that we send a read during the address stage
	wire addr_stage = (stage == 0) && need_addr;
	wire data_stage = !addr_stage;

	// Stage properties
	// ----------------
	wire read_pc  = data_stage && src1_from_pc;
	wire write_pc = data_stage && (dest == `DEST_PC);
	wire access_pc = read_pc || write_pc;

	wire block_flag_updates = !data_stage || (dest == `DEST_PC);

	wire send_read = addr_stage || (data_stage && write_pc); // When write_pc is high, we do the first prefetch at the same time as writing PC
	wire will_write = (dest == `DEST_MEM);
	// If we will make a write during the data stage, the address will be the last address sent, so we can't let in any transactions between the address and data stages
	wire send_write = data_stage && will_write;
	wire send_command = send_read || send_write;

	wire [`SRC_BITS-1:0] curr_src = addr_stage ? addr_src : src;
	wire curr_src_imm = curr_src[`SRC_BIT_IMM];

	// Stage control outputs
	// ---------------------
	assign block_prefetch = execute && access_pc;
	assign write_pc_now   = execute && write_pc && prefetch_idle;

	assign reserve_tx     = execute && will_write;

	// tx_reply_wanted should be constant between the address and data stages so that the data stage knows if it should wait for data.
	// !((dest == `DEST_MEM) && (op == `OP_MOV)) should detect a mov [addr], reg,
	// but the timing is one cycle off we we set tx_reply_wanted = 0.
	// TODO: Fix the timing issues related to that TX messages have one more cycle of header compared to RX messages,
	// and reenable this.
	assign tx_reply_wanted = !((dest == `DEST_MEM) && (op == `OP_MOV));

	assign tx_command = send_read ? `TX_HEADER_READ_16 : (wide ? `TX_HEADER_WRITE_16 : `TX_HEADER_WRITE_8);

	// Wait to start
	// -------------
	wire wait_for_mem = data_stage && need_addr && tx_reply_wanted;

	// Wait conditions shared by command start and alu enable. If no command should be sent this stage, the ALU needs to wait for them anyway.
	wire both_wait = access_pc && !prefetch_idle;
	// These conditions assume that we're receiving the right RX message; the one with the data we just asked for.
	// Which we do since the scheduler will only have one outstanding read that it waits to respond to.
	wire command_wait = both_wait || (wait_for_mem && !rx_started);
	wire alu_wait     = both_wait || (wait_for_mem && !rx_data_valid) || (send_command && !(command_active && tx_data_next)); // Shouldn't need to check that this is the right RX data, no other can be outstanding

	// We might still wait for the command to be started when tx_command_valid is high. But hopefully not if it is a response to incoming data (then reserve_tx will have been held high since the read was sent).
	assign tx_command_valid = execute && !command_wait && send_command;
	wire alu_en             = execute && !alu_wait;

	// Advance external data sources and feed data sinks
	// -------------------------------------------------
	wire active;
	wire [NSHIFT-1:0] data_out;

	assign next_imm_data = curr_src_imm && active;
	assign ext_pc_next = access_pc && active;

	assign pc_data_out = data_out;
	assign tx_data     = data_out;

	// Invoke ALU
	// ----------
	wire [`OP_BITS-1:0] operation = addr_stage ? (addr_op == `ADDR_OP_MOV ? `OP_MOV : `OP_ADD) : op;
	wire pair_op  = addr_stage ? 1'b1 : wide;
	wire pair_op2 = addr_stage ? addr_wide2 : wide2;
	wire sext2 = addr_stage ? addr_src_sext2 : src_sext2;
	wire external_arg2 = curr_src_imm | (curr_src == `SRC_MEM);
	wire [LOG2_NR-1:0] reg1 = addr_stage ? addr_reg1 : reg_dest;
	wire [LOG2_NR-1:0] reg2 = addr_stage ? reg_addr_src : reg_src;
	wire [NSHIFT-1:0] data_in = curr_src_imm ? imm_data_in : rx_pins; // Must wait for rx_pins to contain the right data
	wire update_reg1 = (addr_stage && autoincdec) || (data_stage && (dest == `DEST_REG) && update_dest);
	wire reverse_args = data_stage && (dest == `DEST_MEM) && !curr_src_imm; // Can we have an immediate source and a memory destination at the same time?

	// Should we double r8 in [r16 + r8] for 16 bit wide operations?
	// SRC_IMM2 case also captures autoincrement/decrement
	// TODO: Which pc operations should we double arg2 for?
	wire double_arg2 = (addr_stage && wide && (addr_src == `SRC_IMM7 || addr_src == `SRC_IMM2)) || (write_pc && curr_src_imm && (curr_src != `SRC_IMM16));
	//wire double_arg2 = addr_stage && wide && !addr_wide2; // double addr_arg2 for most address calculations

	wire [2:0] arg2_limit_length = {(addr_src == `SRC_IMM7) && addr_stage, (src == `SRC_IMM6) && data_stage, (addr_src == `SRC_IMM2) && addr_stage};
	wire output_scan_out = addr_stage && addr_just_reg1;
	wire external_arg1 = src1_from_pc;

	ALU #( .LOG2_NR(LOG2_NR), .REG_BITS(REG_BITS), .NSHIFT(NSHIFT)) alu (
		.clk(clk), .reset(reset),
		.op_done(op_done), .op_valid(alu_en),
		.operation(operation),
		.external_arg1(external_arg1), .external_arg2(external_arg2),
		.pair_op(pair_op), .pair_op2(pair_op2), .sext2(sext2), .arg2_limit_length(arg2_limit_length),
		.reg1(reg1), .reg2(reg2), .update_reg1(update_reg1), .reverse_args(reverse_args), .double_arg2(double_arg2),
		.output_scan_out(output_scan_out),
		.update_carry_flags(update_carry_flags && !block_flag_updates), .update_other_flags(update_other_flags && !block_flag_updates),
		.flag_c(flag_c), .flag_v(flag_v), .flag_s(flag_s), .flag_z(flag_z),
		.active(active), .data_in1(pc_data_in), .data_in2(data_in), .data_out(data_out),
		.counter(comp_counter)
	);
endmodule : scheduler