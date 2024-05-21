/*
 * Copyright (c) 2024 Toivo Henningsson
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none

`include "common.vh"

module ALU #( parameter LOG2_NR=3, REG_BITS=8, NSHIFT=2, OP_BITS=`OP_BITS ) (
		input wire clk, reset,

		// Raised on last cycle of op: Must get a new op on next cycle or get op_valid = 0
		output wire op_done,

		// Normally, both regfile_en and advance should be high together to advance the ALU operation
		input wire regfile_en, advance,
		input wire [OP_BITS-1:0] operation, // which operation to perform, according to what?
		input wire external_arg1, external_arg2, // if true, arg1/arg2 is taken from data_in, otherwise from regfile[reg2]
		input wire pair_op, pair_op2, sext2, // sign/zero extend if pair_op && !pair_op2
		input wire [2:0] arg2_limit_length,
		input wire [LOG2_NR-1:0] reg1, reg2,
		input wire update_reg1, reverse_args, double_arg2, invert_src2, // invert_src2 doesn't combine with `OP_SUB because not needed
		input wire output_scan_out,
		input wire update_carry_flags, update_other_flags,

		input wire rotate, timed_rotate, do_shr, do_sar, do_shl, do_ror1, last_ror1,
		input wire [$clog2(REG_BITS*2/NSHIFT)-1:0] rotate_count,

		input wire do_swap_reg, do_swap_mem,

		output wire active, // When high, data in shifted on data_in and data_out (if used by the operation)
		input wire [NSHIFT-1:0] data_in1, data_in2,
		output wire [NSHIFT-1:0] data_out,

		output reg flag_c, flag_v, flag_s, flag_z,

		output wire [$clog2(2*REG_BITS/NSHIFT)-1:0] counter
	);
	wire arg2_7bit;
	wire arg2_6bit;
	wire arg2_2bit;
	assign {arg2_7bit, arg2_6bit, arg2_2bit} = arg2_limit_length;

	localparam NUM_STATES = 2*REG_BITS / NSHIFT; // must divide evenly
	localparam STATE_BITS = $clog2(NUM_STATES);
	localparam ROTATE_COUNT_BITS = STATE_BITS;

	localparam NUM_STATES_PAIR = NUM_STATES;
	localparam STATE_BITS_PAIR = STATE_BITS;

	localparam NUM_STATES_SINGLE = REG_BITS / NSHIFT; // must divide evenly
	localparam STATE_BITS_SINGLE = $clog2(NUM_STATES_SINGLE);


	localparam OP_ADD = `OP_ADD;
	localparam OP_SUB = `OP_SUB;
	localparam OP_ADC = `OP_ADC;
	localparam OP_SBC = `OP_SBC;
	localparam OP_AND = `OP_AND;
	localparam OP_OR  = `OP_OR;
	localparam OP_XOR = `OP_XOR;
	localparam OP_MOV = `OP_MOV;


	assign active = advance;

	reg [STATE_BITS-1:0] state; // Counts through the steps of an operation
	wire [STATE_BITS+1-1:0] inc_state = state + active;
	// TODO: truncate rotate_count when doing 8 bit rotations?
	wire last_op_cycle = timed_rotate ? (inc_state[STATE_BITS_PAIR-1:0] == rotate_count) :
		(pair_op ? inc_state[STATE_BITS_PAIR] : inc_state[STATE_BITS_SINGLE]) || last_ror1;
	//wire [STATE_BITS-1:0] next_state = last_op_cycle ? 0 : inc_state[STATE_BITS-1:0];

	wire second = state[STATE_BITS_PAIR-1]; // are we on the second byte in the pair?
	assign counter = state;

	//reg flag_c, flag_v, flag_s, flag_z;


	assign op_done = last_op_cycle;

	always @(posedge clk) begin
		if (reset ||last_op_cycle) begin
			state <= 0;
		end else begin
			//state <= next_state;
			state <= inc_state[STATE_BITS-1:0];
		end
	end

	wire [LOG2_NR-1:0] reg_index, reg_index2;
	wire [NSHIFT-1:0] scan_in, scan_in2, scan_out, scan_out2;

	wire block_carry = !operation[`OP_BIT_WCARRY]; // Set to zero for adc
	wire do_sub = (operation == `OP_SUB) || (operation == `OP_SBC);

	// Is the operation using just one register operand?
	//wire single_reg = (operation == OP_MOV) || external_arg2;
	wire single_reg = external_arg2;

	wire do_scan  = regfile_en;
	wire do_scan2 = regfile_en && (!single_reg || rotate);

	wire [NSHIFT-1:0] arg1, arg2_0, arg2;
	//assign arg1 = scan_out;
	assign arg1 = external_arg1 ? data_in1 : scan_out;
	assign arg2_0 = external_arg2 ? data_in2 : scan_out2;

	// Highest bit of arg2 seen so far. Sign extension will preserve it once the valid input bits have passed.
	reg sign2;

	// Zero/sign extend arg2?
	 // Assumes REG_BITS=8, NSHIFT=2
	wire extend_arg2 = (second && !pair_op2) || (arg2_6bit && state == 3) || (arg2_2bit && state != 0);
	wire [NSHIFT-1:0] arg2_01 = extend_arg2 ? ((sext2 && sign2) ? '1 : '0) : ((arg2_7bit && state == 3) ? arg2_0 & 2'b01 : arg2_0);

	reg old_arg2_bit; // CONSIDER: Is this the same as sign2?
	assign arg2 = double_arg2 ? {arg2_01[NSHIFT-1-1:0], (state == 0) ? 1'b0 : old_arg2_bit} : arg2_01;
	always @(posedge clk) old_arg2_bit <= arg2_01[NSHIFT-1];

	reg carry;
	//wire carry_in = (state == 0 && block_carry) ? do_sub : carry;
	wire carry_in = (state == 0) ? (block_carry ? do_sub : flag_c) : carry;

	wire invert_arg2 = do_sub | invert_src2;

	wire [NSHIFT-1:0] arg1_s = reverse_args ? arg2 : arg1;
	wire [NSHIFT-1:0] arg2_s = reverse_args ? arg1 : arg2;
	wire [NSHIFT-1:0] arg2_si = (arg2_s ^ (invert_arg2 ? {NSHIFT{1'b1}} : '0));

	wire [NSHIFT+1-1:0] sum = arg1_s + arg2_si + carry_in; // TODO: better way to express the adder?

	// Calculate signed overflow. TODO: better way, share more logic with the normal sum
	wire [NSHIFT-1:0] sb = {1'b1, {(NSHIFT-1){1'b0}}};
	wire [NSHIFT+1-1:0] sum_v = (arg1_s ^ sb) + ((arg2_s ^ (invert_arg2 ? {NSHIFT{1'b1}} : '0)) ^ sb) + carry_in;

	// not a register
	reg [NSHIFT-1:0] result;
	always @(*) begin
		case (operation)
			OP_ADD, OP_SUB, OP_ADC, OP_SBC: result = sum;
			OP_AND: result = arg1_s & arg2_si;
			OP_OR:  result = arg1_s | arg2_si;
			OP_XOR: result = arg1_s ^ arg2_si;
			OP_MOV: result = arg2_si;
		endcase
	end



	wire [NSHIFT-1:0] scan_in_regular = update_reg1 ? (do_swap_mem ? data_in2 : result) : scan_out;

	wire shift_in_zeros = do_shr || (do_shl && (last_ror1 || (state > rotate_count) || ((state == rotate_count) && !do_ror1)));

	wire [NSHIFT-1:0] scan_in_rotate_regular = do_sar ? {NSHIFT{carry}} : (shift_in_zeros ? 2'b0 : scan_out2);
	wire scan_in_ror1_msb = last_ror1 ? (do_sar ? carry : (do_shr ? 1'b0 : scan_out2[NSHIFT-1:1])) : scan_out2[NSHIFT-1-1:0];

	wire [NSHIFT-1:0] scan_in_ror1 = {(!(shift_in_zeros && do_shl) || last_ror1) && scan_in_ror1_msb, !(shift_in_zeros && do_shl) && carry};

	wire [NSHIFT-1:0] scan_in_rotate = do_ror1 ? scan_in_ror1 : scan_in_rotate_regular;
	assign scan_in = (rotate && !do_swap_mem) ? scan_in_rotate : scan_in_regular;

	assign scan_in2 = rotate || do_swap_reg ? scan_out : scan_out2;

	wire [LOG2_NR-1:0] pre_reg_index = reg1;
	//wire [LOG2_NR-1:0] pre_reg_index2 = single_reg ? reg1 : reg2;
	//wire [LOG2_NR-1:0] pre_reg_index2 = reg2;
	wire [LOG2_NR-1:0] pre_reg_index2 = rotate ? reg1 : reg2;

	// If pair_op is true, replace bottom register index bit with state[STATE_BITS_PAIR-1] (zero for first register in pair, then one).
	// For rotate, make reg_index the top, so that it can scan in zeros when do_shr is true.
	assign reg_index  = {pre_reg_index[ LOG2_NR-1:1], pair_op ?  (rotate ? 1'b1 : second) : pre_reg_index[0]};
	// Will scan second register for both bytes even if just using the lower one
	assign reg_index2 = {pre_reg_index2[LOG2_NR-1:1], pair_op2 ? (rotate ? 1'b0 : second) : pre_reg_index2[0]};

	wire next_carry = rotate ? scan_out2[NSHIFT-1] : sum[NSHIFT];
	wire next_scarry = sum_v[NSHIFT];
	always @(posedge clk) begin
		if (!(rotate && (timed_rotate || last_ror1))) carry <= next_carry;
		sign2 <= arg2_01[NSHIFT-1];

		if (reset) begin
			flag_c <= 0;
			flag_v <= 0;
			flag_s <= 0;
			flag_z <= 0;
		end else if (active) begin
			if (update_carry_flags) begin
				flag_c <= next_carry;
				flag_v <= next_scarry;
			end
			if (update_other_flags) begin
				flag_s <= result[NSHIFT-1];
				flag_z <= (result == '0) && (flag_z || (state == 0));
			end
		end
	end

	regfile #(.LOG2_NR(LOG2_NR), .REG_BITS(REG_BITS), .NSHIFT(NSHIFT)) registers(
		.clk(clk), .reset(reset),
		.reg_index(reg_index), .reg_index2(reg_index2),
		.do_scan(do_scan), .do_scan2(do_scan2),
		.scan_in(scan_in), .scan_in2(scan_in2),
		.scan_out(scan_out), .scan_out2(scan_out2)
	);

	// data_out also makes it possible to observe the register file, make sure that it stays that way
	// so that it doesn't get optimized away.
	assign data_out = output_scan_out ? scan_out : (do_swap_mem ? scan_out2 : result);
endmodule : ALU
