/*
 * Copyright (c) 2024 Toivo Henningsson
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none

`include "common.vh"

module scheduler #( parameter LOG2_NR=4, REG_BITS=8, NSHIFT=2, PAYLOAD_CYCLES=8 ) (
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
		input wire src1_zero,
		input wire [`SRC_BITS-1:0] src,
		input wire [LOG2_NR-1:0] reg_src,
		input wire src_sext2,
		input wire double_src2,
		input wire update_dest,
		input wire force_reverse_args,
		input wire invert_src2,

		input wire addr_wide2,
		input wire [`ADDR_OP_BITS-1:0] addr_op,
		input wire [LOG2_NR-1:0] addr_reg1,
		input wire [`SRC_BITS-1:0] addr_src,
		input wire [LOG2_NR-1:0] reg_addr_src,
		input wire addr_src_sext2,
		input wire autoincdec, addr_just_reg1,

		input wire update_carry_flags, update_other_flags, no_flag_updates,

		input wire use_cc,
		input wire [`CC_BITS-1:0] cc,

		input wire use_rotate, rotate_only, use_shr, use_sar, use_shl,
		input wire [$clog2(REG_BITS*2)-1:0] rotate_count,

		input wire do_swap,

		input wire any_prefetched,
		output wire load_imm16,
		input wire imm16_loaded,
		input wire [NSHIFT-1:0] imm_data_in,
		output wire next_imm_data,

		output wire [NSHIFT-1:0] imm8_data_out,
		input wire [2*REG_BITS-1:0] imm_full,
`ifdef USE_MULTIPLIER
		output wire set_imm_top,
		output wire [REG_BITS-1:0] next_imm_top_data,

		input wire use_mul, mul_only, extended_mul,
`endif

		output wire addr_stage, data_stage,
		input block_tx_reply,

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
	localparam ROTATE_COUNT_BITS = $clog2(REG_BITS*2);

`ifndef USE_MULTIPLIER
	wire use_mul = 0;
	wire mul_only = 0;
	wire extended_mul = 0;
`endif

	wire use_rot_or_mul = use_rotate || use_mul;

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

	wire no_op;
	wire skip = use_cc && !cc_ok || no_op;
	wire skip_stage;
	wire execute = inst_valid && !wait_for_imm16 && !skip && !skip_stage;

	// Stage and command progression
	// -----------------------------
	localparam NUM_STAGES = 3;
	localparam STAGE_BITS = $clog2(NUM_STAGES);
	localparam STAGE0 = 0;
	localparam STAGE1 = 1;
	localparam STAGE_ROR1 = 2;
	localparam STAGE_ROTATE = 3;
	localparam STAGE_BIT_ANY_ROTATE = 1; // as long this bit is set, it is a rotate stage

	wire any_rotate_stage;
	wire do_ror1;
	wire ror1_stage;

	wire op_done;
	reg [STAGE_BITS-1:0] stage;
	reg last_ror1;
	reg command_active; // Has the current command been started yet? It's not enough if there is a previous command active.

	always @(posedge clk) begin
		if (reset || inst_done) begin
			stage <= STAGE0;
			last_ror1 <= 0;
		end else if ((op_done && data_stage) || skip_stage && !any_rotate_stage) begin
			stage <= STAGE_ROR1;
			last_ror1 <= 0;
		end else if (op_done || skip_stage) begin
			if (ror1_stage && do_ror1 && !last_ror1) begin
				last_ror1 <= 1;
			end else begin
				stage <= stage + 1;
				last_ror1 <= 0;
			end
		end

		if (reset || op_done) begin // Should reset at skip_stage too, but the rotate stage does not use command_active. TODO: Change if third stage needs command_active
			command_active <= 0;
		end else if (tx_command_started) begin
			command_active <= 1;
		end
	end

	assign need_addr = (src == `SRC_MEM) || (dest == `DEST_MEM); // Assume need_addr means that we send a read during the address stage
	assign addr_stage = (stage == STAGE0) && need_addr;

	wire _any_rotate_stage  = stage[STAGE_BIT_ANY_ROTATE];
	wire _ror1_stage = stage == STAGE_ROR1;
	wire _rotate_stage = stage == STAGE_ROTATE;

`ifdef USE_MULTIPLIER
	assign any_rotate_stage = _any_rotate_stage && use_rotate;
	assign ror1_stage = _ror1_stage   && use_rotate;
	wire rotate_stage = _rotate_stage && use_rotate;

	wire any_mul_stage = _any_rotate_stage && use_mul;
	wire mul1_stage = _ror1_stage    && use_mul;
	wire mul2_stage  = _rotate_stage && use_mul;
`else
	assign any_rotate_stage = _any_rotate_stage;
	assign ror1_stage = _ror1_stage;
	wire rotate_stage = _rotate_stage;

	wire any_mul_stage = 0;
	wire mul1_stage    = 0;
	wire mul2_stage    = 0;
`endif


	assign data_stage = !_any_rotate_stage && !addr_stage;

	//assign inst_done = (op_done && (use_rotate ? rotate_stage : data_stage)) || (skip && !wait_for_imm16);
	assign inst_done = (op_done && (_rotate_stage || (use_mul && !extended_mul && _ror1_stage) || (!use_rot_or_mul && data_stage))) || (skip && !wait_for_imm16);

	// rotate_count = 0 for a shift is always a no-op
	assign no_op = (rotate_count[ROTATE_COUNT_BITS-1:1] == '0) && (rotate_stage || (any_rotate_stage && rotate_count[0] == 0));
	assign skip_stage = inst_valid && ((rotate_only || mul_only) && !_any_rotate_stage) || (ror1_stage && (rotate_count[0] == 0) && !use_sar && !use_shl);

	// Stage properties
	// ----------------
	wire read_pc  = data_stage && src1_from_pc;
	wire write_pc = data_stage && (dest == `DEST_PC);
	wire access_pc = read_pc || write_pc;

	wire block_flag_updates = !data_stage || (dest == `DEST_PC) || use_rot_or_mul || no_flag_updates; // TODO: might update flags during the rotate stage

	wire send_read = addr_stage || (data_stage && write_pc); // When write_pc is high, we do the first prefetch at the same time as writing PC
	wire will_write = (dest == `DEST_MEM);
	// If we will make a write during the data stage, the address will be the last address sent, so we can't let in any transactions between the address and data stages
	wire send_write = data_stage && will_write;
	wire send_command = send_read || send_write;

	wire [`SRC_BITS-1:0] curr_src = addr_stage ? addr_src : src;
	wire curr_src_imm = curr_src[`SRC_BIT_IMM] && !rotate_stage;

	// Stage control outputs
	// ---------------------
 	// If we will write to pc then there is no point in prefetching beyond one imm16 possibly needed by this instruction, save time by blocking
	assign block_prefetch = (execute && access_pc) || (inst_valid && (dest == `DEST_PC) && any_prefetched);
	assign write_pc_now   = execute && write_pc && prefetch_idle;

	assign reserve_tx     = execute && will_write && !do_swap; // Swaps never wait to start a transaction after initial access, so don't need to reserve

	// tx_reply_wanted should be constant between the address and data stages so that the data stage knows if it should wait for data.
	// !((dest == `DEST_MEM) && (op == `OP_MOV)) should detect a mov [addr], reg,
	// but the timing is one cycle off we we set tx_reply_wanted = 0.
	// TODO: Fix the timing issues related to that TX messages have one more cycle of header compared to RX messages,
	// and reenable this.
	assign tx_reply_wanted = !((dest == `DEST_MEM) && (op == `OP_MOV)) && !block_tx_reply || write_pc || do_swap;

	assign tx_command = send_read ? `TX_HEADER_READ_16 : (wide ? `TX_HEADER_WRITE_16 : `TX_HEADER_WRITE_8);

	// Wait to start
	// -------------
	wire wait_for_mem = data_stage && need_addr && tx_reply_wanted;
	wire do_swap_data = do_swap && data_stage;

	// Wait conditions shared by command start and alu enable. If no command should be sent this stage, the ALU needs to wait for them anyway.
	wire both_wait = access_pc && !prefetch_idle;
	wire tx_data_wait = (send_command && !(command_active && tx_data_next));
	// These conditions assume that we're receiving the right RX message; the one with the data we just asked for.
	// Which we do since the scheduler will only have one outstanding read that it waits to respond to.
	wire command_wait = both_wait || (wait_for_mem && !rx_started && !do_swap_data);
	wire alu_wait     = both_wait || (wait_for_mem && !rx_data_valid) || (tx_data_wait && !do_swap_data); // Shouldn't need to check that this is the right RX data, no other can be outstanding
	wire regfile_wait = do_swap_data ? tx_data_wait : alu_wait;

	// We might still wait for the command to be started when tx_command_valid is high. But hopefully not if it is a response to incoming data (then reserve_tx will have been held high since the read was sent).
	assign tx_command_valid = execute && !command_wait && send_command && !command_active;
	wire alu_en             = execute && !alu_wait;
	wire regfile_en         = execute && (!regfile_wait || !alu_wait); // regfile_en should be high whenever alu_en is

	// Advance external data sources and feed data sinks
	// -------------------------------------------------
	wire active;
	wire [NSHIFT-1:0] data_out;

	assign next_imm_data = (curr_src_imm || (use_rot_or_mul && data_stage)) && active && !(any_rotate_stage || any_mul_stage);
	assign ext_pc_next = access_pc && active;

	assign pc_data_out = data_out;
	assign tx_data     = data_out;
	assign imm8_data_out = data_out;

	// Invoke ALU
	// ----------
	wire [`OP_BITS-1:0] operation = addr_stage ? (addr_op == `ADDR_OP_MOV ? `OP_MOV : `OP_ADD) : op;
	wire force_narrow = (data_stage && use_rot_or_mul) || mul2_stage;
	wire pair_op  = addr_stage ? 1'b1       : (force_narrow ? 1'b0 : wide);
	wire pair_op2 = addr_stage ? addr_wide2 : (force_narrow ? 1'b0 : wide2);
	wire sext2 = addr_stage ? addr_src_sext2 : src_sext2;
	wire external_arg2 = curr_src_imm | (curr_src == `SRC_MEM);
	wire [LOG2_NR-1:0] reg1 = addr_stage ? addr_reg1 : (mul2_stage ? `REG_INDEX_MUL_MSB_DEST : reg_dest);
	wire [LOG2_NR-1:0] reg2 = addr_stage ? reg_addr_src : reg_src;
	wire [NSHIFT-1:0] data_in = curr_src_imm ? imm_data_in : rx_pins; // Must wait for rx_pins to contain the right data
	wire update_reg1 = (addr_stage && autoincdec) || (data_stage && ((dest == `DEST_REG) || do_swap) && update_dest) || any_mul_stage;
	wire reverse_args = data_stage && ((dest == `DEST_MEM) && !curr_src_imm) || force_reverse_args; // Can we have an immediate source and a memory destination at the same time?

	// Should we double r8 in [r16 + r8] for 16 bit wide operations?
	// SRC_IMM2 case also captures autoincrement/decrement
	// TODO: Which pc operations should we double arg2 for?
	wire double_arg2 = (addr_stage && wide && !use_rot_or_mul && (addr_src == `SRC_IMM7 || addr_src == `SRC_IMM2)) || (write_pc && curr_src_imm && (curr_src != `SRC_IMM16)) || (data_stage && double_src2);
	//wire double_arg2 = addr_stage && wide && !addr_wide2; // double addr_arg2 for most address calculations

	wire [2:0] arg2_limit_length = {(addr_src == `SRC_IMM7) && addr_stage, (src == `SRC_IMM6) && data_stage, (addr_src == `SRC_IMM2) && addr_stage || (src == `SRC_IMM2 && data_stage)};
	wire output_scan_out = addr_stage && addr_just_reg1;
	wire external_arg1 = data_stage && (src1_from_pc || src1_zero);

	wire do_swap_reg =              !need_addr && do_swap;
	wire do_swap_mem = data_stage && need_addr && do_swap;

	wire rotate = any_rotate_stage || do_swap_mem;
	wire timed_rotate = rotate_stage;
	wire do_shr = (rotate_stage || last_ror1) && use_shr; // Has effect only when rotate is true
	wire do_sar = (rotate_stage || last_ror1) && use_sar; // Has effect only when rotate is true
	wire do_shl = ror1_stage && use_shl;

	assign do_ror1 = ror1_stage && (rotate_count[0] == 1);

`ifdef USE_MULTIPLIER
	wire do_mul = any_mul_stage;
	wire continue_mul = mul2_stage;
	wire zero_mul_input = mul2_stage;
`endif

	ALU #( .LOG2_NR(LOG2_NR), .REG_BITS(REG_BITS), .NSHIFT(NSHIFT)) alu (
		.clk(clk), .reset(reset),
		.op_done(op_done), .regfile_en(regfile_en), .advance(alu_en),
		.operation(operation),
		.external_arg1(external_arg1), .external_arg2(external_arg2),
		.pair_op(pair_op), .pair_op2(pair_op2), .sext2(sext2), .arg2_limit_length(arg2_limit_length),
		.reg1(reg1), .reg2(reg2), .update_reg1(update_reg1), .reverse_args(reverse_args), .double_arg2(double_arg2), .invert_src2(data_stage && invert_src2),
		.output_scan_out(output_scan_out),
		.update_carry_flags(update_carry_flags && !block_flag_updates), .update_other_flags(update_other_flags && !block_flag_updates),
		.rotate(rotate), .timed_rotate(timed_rotate), .do_shr(do_shr), .do_sar(do_sar), .do_shl(do_shl), .do_ror1(do_ror1), .last_ror1(last_ror1), .rotate_count(rotate_count[ROTATE_COUNT_BITS-1:1]),
		.do_swap_reg(do_swap_reg), .do_swap_mem(do_swap_mem),
		.imm_full(imm_full),
`ifdef USE_MULTIPLIER
		.set_imm_top(set_imm_top), .next_imm_top_data(next_imm_top_data),
		.do_mul(do_mul), .continue_mul(continue_mul), .zero_mul_input(zero_mul_input),
`endif
		.flag_c(flag_c), .flag_v(flag_v), .flag_s(flag_s), .flag_z(flag_z),
		.active(active), .data_in1(src1_zero ? '0 : pc_data_in), .data_in2(data_in), .data_out(data_out),
		.counter(comp_counter)
	);
endmodule : scheduler
