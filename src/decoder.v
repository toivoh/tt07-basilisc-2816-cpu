/*
 * Copyright (c) 2024 Toivo Henningsson
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none

`include "common.vh"

module decoder #( parameter LOG2_NR=4, REG_BITS=8, NSHIFT=2, PAYLOAD_CYCLES=8 ) (
		input wire clk, reset,

		input wire inst_valid,
		input wire [15:0] inst,
		output wire inst_done,

		input wire [2*REG_BITS-1:0] imm_full,
		output wire feed_imm8,
		output wire [NSHIFT-1:0] imm8_data_out,
`ifdef USE_MULTIPLIER
		output wire set_imm_top,
		output wire [REG_BITS-1:0] next_imm_top_data,
`endif

		output wire reserve_tx,

		input wire any_prefetched,
		output wire load_imm16,
		input wire imm16_loaded,
		input wire [NSHIFT-1:0] imm_data_in,
		output wire next_imm_data,

		// PC control interface
		output wire block_prefetch, write_pc, ext_pc_next, // ext_pc_next may only be high when prefetch_idle is
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

	wire addr_stage, data_stage;

	// Decoder stage progression
	// =========================

	reg stage;
	reg need_pre_stage; // not a register

	wire pre_stage = need_pre_stage && (stage == 0);
	wire normal_stage = !pre_stage;

	wire sc_inst_done;
	assign inst_done = normal_stage && sc_inst_done;

	always @(posedge clk) begin
		if (reset || inst_done) stage <= 0;
		else if (sc_inst_done) stage <= 1;
	end


	// Decoder
	// =======

	wire b8, e, m, d, z;
	wire [2:0] aaa;
	wire [1:0] rr;
	wire [5:0] imm6;

	// 1eaaamrrdziiiiii		2^15 r8:  8x binop/shift
	assign {b8, e, aaa, m, rr, d, z, imm6} = inst;

	wire [3:0] cccc = inst[11:8];
	wire g = aaa[1];

	wire [2:0] mdz = {m, d, z};

	localparam CLASS_BITS = 3;
	localparam CLASS_ALU = 0;
	localparam CLASS_MOV = 1; // Also used for jumps
	localparam CLASS_SWAP = 2;
	localparam CLASS_SHIFT = 3;
	//localparam CLASS_INCDECZERO = 4;
	localparam CLASS_MUL = 4;

	wire [3:0] cc = cccc;
	//wire use_imm6 = (mdz == 3'b101);
	//wire [1:0] idz_op = rr;

	wire push_pc_plus_n = pre_stage; // only thing that pre_stage is used for so far.

/*
	111111
	5432109876543210
	1eaaamrrdziiiiii
	000100000ziiiiii	2^7 	r16, rr  = 0, 	mov pc, [zp]
	0010000000iiiiii	2^6 	r8,  rrg = 0 	mov pc, src
*/
	//wire jump_zp = (inst[15:8] == '9b000100000);
	//wire jump_src = (inst[15:7] == '10b0010000000);

	// not registers
	reg [CLASS_BITS-1:0] cls;
	reg wide;
	reg [2:0] r;
	reg [2:0] shift_op;
	reg use_zp;
	reg use_imm8; // overrides d, z when true: it has to be a mov r, imm8
	reg branch;
	reg src1_from_pc;
	reg jump;
	reg cmptest;
	reg alt_op_available;
	reg use_mul;
	//reg [NSHIFT-1:0] plus_pc_words;
	reg effective_d;
	always @(*) begin
		cls = 'X;
		//cls = CLASS_MOV;
		wide = 1'bX;
		r = 'X;
		shift_op = 'X;
		use_zp = (m == 0);
		use_imm8 = 0;
		branch = 0;
		src1_from_pc = 0;
		jump = 0;
		need_pre_stage = 0;
		cmptest = 0;
		alt_op_available = 0;
		//plus_pc_words = 'X;
		use_mul = 0;
		effective_d = d;

//				111111
//				5432109876543210
		if (b8 == 1 || e == 1) begin
//				1eaaamrrdziiiiii	2^15 r8:  8x binop/shift
//				01aaamrrdziiiiii	2^14 r16: 8x binop/shift
			wide = (b8 == 0);
//		100 	101 		110 	111 		00x 		01x
//		(r, s)	(r, imm6)	(d, r)	(d, r)b		(r, zp)x2	(zp, r)x2
//	8x	<-------- binop --------->	shift r, s	<------ binop ------>
			cls = (mdz == 3'b111) ? CLASS_SHIFT : CLASS_ALU;
			if (mdz == 3'b111) effective_d = 0;
			r = {rr, e & !wide}; // Would have been easier if r[0] could be 1. Can r[0] be ignored when wide=1?
			shift_op = aaa;
			cmptest = (aaa == 3'b111);
			alt_op_available = !cmptest && !(mdz == 3'b111);
			if (cmptest) effective_d = 0;
		end else if (aaa[2] == 1 || aaa[1] == 1) begin
//				001gomrrdziiiiii	2^13 r8:  mov/shift/swap/inc/dec/zero
//				0001omrrdziiiiii	2^12 r16: mov/shift/swap/inc/dec/zero
			wide = aaa[2] == 0;
			cls = CLASS_MOV; // may be overridden below
			r = {rr, g & !wide};
			//shift_op = imm6[5:3];
			shift_op = {imm6[5:4], 1'b0}; // imm6[3:0] is used for shift count
			//if (wide) shift_op[0] = m; // imm6[3] is needed as part of the the shift amount

			if (aaa[0] == 0) begin
//		100 	101 		110 	111 		00x 		01x
//		(r, s)	(r, imm6)	(d, r)	(d, r)b		(r, zp)x2	(zp, r)x2
//	1x	<---------- mov r, imm8 ---------->		inc/dec/zero zp	swap
				if (m == 1) begin
					use_imm8 = 1;
					effective_d = 0;
				end
				if ({m, d} == 2'b00) begin
					if (rr == 0) begin
						jump = normal_stage;
						cls = CLASS_MOV;
						if (wide) begin
//								111111
//								5432109876543210
//								000100000ziiiiii	jump [zp] (mov pc, [zp])
							// Should be all set?
						end else begin
//								111111
//								5432109876543210
//								0010000000iiiiii	jump src (mov pc, src)
//								0010000001iiiiii	call src
							wide = 1;
							use_zp = 0;
							need_pre_stage = z;
							//plus_pc_words = 2'd2;

							// needed for call
							src1_from_pc = pre_stage;
						end
					end else begin
						//cls = CLASS_INCDECZERO;

`ifdef USE_MULTIPLIER
						// Use all this encoding space for mul r, imm7 for now. Regs 0 and 1 are taken by the jump/call instructions above.
						cls = CLASS_MUL;
						use_mul = 1;
						use_imm8 = (z == 0);
						use_zp = 0;
`endif
					end
				end
				if ({m, d} == 2'b01) cls = CLASS_SWAP;
				/*
				use_imm8 = 1;
				if (m == 0) begin
					cls = CLASS_ALU;
					src1_from_pc = 1;
				end
				*/
			end else begin
//		100 	101 		110 	111 		00x 		01x
//		(r, s)	(r, imm6)	(d, r)	(d, r)b		(r, zp)x2	(zp, r)x2
//	1x	mov		shift		mov		swap		<------- mov ------->
				if (mdz == 3'b101) cls = CLASS_SHIFT;
				if (mdz == 3'b111) cls = CLASS_SWAP;
				alt_op_available = (cls == CLASS_MOV);
				//if (wide && mdz == 3'b001) cls = CLASS_SHIFT; // collides with mov r16, zp?
			end
		end else begin
//				0000ccccbbbbbbbb	2^12 branch
			cls = CLASS_MOV;
			wide = 1;
			branch = 1;
			src1_from_pc = 1;
			use_imm8 = 1;
			effective_d = 'X;
			need_pre_stage = (cccc == `CC_CALL);
			//plus_pc_words = 2'd1;
		end
	end

	/*
	reg [2:0] binop;
	reg update_dest;
	always @(*) begin
		binop = aaa;
		update_dest = 1;
		//if (cls == CLASS_ALU && binop == `OP_MOV) begin
		//	// cmp
		//	binop = `OP_SUB;
		//	update_dest = 0;
		//end
	end
*/
	wire [2:0] binop = aaa;


	wire [5:0] arg2_enc = imm6;

	// not registers
	reg [`SRC_BITS-1:0] arg2_src;
	reg [LOG2_NR-1:0] addr_reg1;
	reg [`SRC_BITS-1:0] addr_src;
	reg wide2;
	reg src_sext2;
	reg [`ADDR_OP_BITS-1:0] addr_op;
	reg autoincdec, autoincdec_dir, autoincdec_update;
	reg addr_src_sext2;
	reg arg2_pure_reg;
	reg [LOG2_NR-1:0] arg2_reg;
	reg no_flag_updates;
	always @(*) begin
		arg2_src = 'X;
		addr_reg1 = 'X;
		//addr_reg2 = 'X;
		addr_src = 'X;
		wide2 = wide;
		src_sext2 = 0;
		addr_op = `ADDR_OP_ADD;
		autoincdec = 0;
		autoincdec_dir = 'X;
		autoincdec_update = 0;
		addr_src_sext2 = 0;
		arg2_pure_reg = 0;
		arg2_reg = arg2_enc[2:0];
		no_flag_updates = 0;

		if (use_imm8 && !push_pc_plus_n) begin
			arg2_src = `SRC_IMM8;
			wide2 = 0;
			src_sext2 = 1;
		end else if (use_zp && !push_pc_plus_n) begin
			// zp
			arg2_src = `SRC_MEM;
			addr_op = `ADDR_OP_MOV;
			addr_src = `SRC_IMM7;
		end else if (mdz == 3'b101 && !push_pc_plus_n) begin
			// (r, imm6)
			arg2_src = `SRC_IMM6;
			wide2 = 0;
			src_sext2 = 1;
		end else begin
			// Regular dest/src
			if (arg2_enc[5] == 1 || push_pc_plus_n) begin
				// 1RRrrr	[r16 + r8]
					// TODO: special cases when r16 + r8 overlap, maybe imm16 + r8 case too
				arg2_src = `SRC_MEM;
				addr_reg1 = push_pc_plus_n ? `REG_INDEX_SP : {arg2_enc[4:3], 1'b0};
				//addr_src = `SRC_REG;

				if (push_pc_plus_n) arg2_src = `SRC_IMM2; // override
				autoincdec = (addr_reg1[2:1] == addr_reg2[2:1]) || push_pc_plus_n;
				autoincdec_update = autoincdec;
				autoincdec_dir = arg2_enc[0] || push_pc_plus_n; // 1 = negative

				addr_src = autoincdec ? `SRC_IMM2 : `SRC_REG;
				addr_src_sext2 = autoincdec;
			end else if (arg2_enc[4] == 1) begin
				// 01RRii	[r16 + imm2]
				arg2_src = `SRC_MEM;
				addr_reg1 = {arg2_enc[3:2], 1'b0};
				addr_src = `SRC_IMM2;
			end else if (arg2_enc[3] == 1 || (arg2_enc[0] == 0)) begin
				arg2_src = `SRC_REG;
				if (!wide && arg2_enc[3] == 0) begin
					// 000rr0 special register when wide=0
					// 000000 sp (duplicate)
					// 000010 flags
					//arg2_reg = `REG_INDEX_FLAGS;
					arg2_reg[LOG2_NR-1] = 1; // allows access to even numbered special registers
					no_flag_updates = effective_d; // block flag updates when writing, at least when writing flags
				end else begin
					// 001rrr	r8
					// 000rr0	r16 when wide=1
					wide2 = !arg2_enc[3];
					src_sext2 = !d || cmptest; // d selects sext or zext
					arg2_pure_reg = !wide || !arg2_enc[3];
				end
			end else begin
				// 000xy1
				if (arg2_enc[2:1] == 0) begin
					// imm16
					arg2_src = `SRC_IMM16;
				end else if (arg2_enc[2:1] == 1) begin
					// [imm16]
					arg2_src = `SRC_MEM;
					addr_src = `SRC_IMM16;
					addr_op = `ADDR_OP_MOV;
				end else if (arg2_enc[2:1] == 2) begin
					// [push/pop/sp]
					arg2_src = `SRC_MEM;
					addr_src = `SRC_IMM2;
					addr_reg1 = `REG_INDEX_SP;
					addr_src_sext2 = 1;
					autoincdec = 1;
					// Turn off actual update sp for RMW operations, and set autoincdec_dir=0. This makes us just operate on [sp].
					autoincdec_update = (cls == CLASS_MOV) || !effective_d;
					autoincdec_dir = effective_d && autoincdec_update; // TODO correct?
				end else if (arg2_enc[2:1] == 3) begin
					// sp
					arg2_src = `SRC_REG;
					arg2_reg = `REG_INDEX_SP;
				end
			end
		end
		if (use_rotate) wide2 = wide;
	end
	wire long_inst = !branch && (arg2_enc[5:2] == '0 && arg2_enc[0] == 1); // Matches arg2 = imm16 or [imm16]
	// Need to take the imm16 into account here since when we push pc + 2*plus_pc_words, it has not been consumed yet.
	wire [NSHIFT-1:0] plus_pc_words = long_inst ? 2'd2 : 2'd1;

	wire special_reg2 = arg2_reg[LOG2_NR-1];

	wire [LOG2_NR-1:0] arg1_reg = r;

	wire [LOG2_NR-1:0] addr_reg2 = arg2_enc[2:0];

	wire cmp = cmptest && !d;
	wire test = cmptest && d;
	wire update_dest = !cmptest;

	wire use_alt_op = alt_op_available && !special_reg2 && arg2_pure_reg && d;
	//wire sub_or_sbc = (cls == CLASS_ALU) && (binop == `OP_SUB || binop == `OP_SBC);
	wire add_or_sub = (cls == CLASS_ALU && !binop[`OP_BIT_NADD]);
	wire sub_or_sbc = add_or_sub && (binop[0] == 1);
	wire add_or_adc = add_or_sub && (binop[0] == 0);

	wire invert_src2 = use_alt_op && !sub_or_sbc;
	// Replace sub r, imm6 with revsub r, imm6, also use revsub as alternate op for sub
	wire force_reverse_args = sub_or_sbc && ((mdz == 3'b101) || use_alt_op);


	wire [`OP_BITS-1:0] op = (branch || src1_from_pc) ? `OP_ADD : (use_rol || cmp ? `OP_SUB : (test ? `OP_AND : ((cls == CLASS_ALU) ? binop : `OP_MOV)));

	// TODO: Is this the right conditions for updating flags? Should shifts update c, and v?
	wire update_other_flags = (cls == CLASS_ALU) || use_alt_op;
	wire update_carry_flags = (cls == CLASS_ALU && (cmp || !binop[`OP_BIT_NADD]));

	wire [`DEST_BITS-1:0] dest = ((branch || jump) && normal_stage) ? `DEST_PC : ((use_rotate || use_mul) ? `DEST_IMM8 : ( ((effective_d && (arg2_src == `SRC_MEM)) || push_pc_plus_n) ? `DEST_MEM : `DEST_REG ));
	wire [`SRC_BITS-1:0] src = arg2_src;
	// swap_arg1_arg2 is only high when both args are registers
	wire swap_arg1_arg2 = special_reg2 && (effective_d && (cls != CLASS_SHIFT) && (cls != CLASS_MUL));
	wire [LOG2_NR-1:0] reg_dest = swap_arg1_arg2 ? arg2_reg : arg1_reg;
	wire [LOG2_NR-1:0] reg_src  = swap_arg1_arg2 ? arg1_reg : arg2_reg;
	wire addr_wide2 = (addr_src == `SRC_IMM16);

	wire double_src2 = push_pc_plus_n; // There are other cases when the scheduler sets double_arg2 based on its inputs.

	wire sc_next_imm_data;
	assign next_imm_data = sc_next_imm_data && normal_stage; // Don't consume imm data during pre-stage

	wire [NSHIFT-1:0] imm_data_in2 = (autoincdec && addr_stage) ? {autoincdec_dir, 1'b1} : (push_pc_plus_n ? plus_pc_words : imm_data_in);
	wire addr_just_reg1 = autoincdec && (autoincdec_dir == 0);

	wire use_cc = branch;

	wire block_tx_reply = push_pc_plus_n;

	wire use_rotate = (cls == CLASS_SHIFT);
	wire rotate_only = use_rotate & (d == 0); // Differentias between the two shift forms
	//wire use_shr = shift_op[2];
	//wire use_sar = shift_op[1];
	wire use_shr = shift_op[2:1] == 2'b10;
	wire use_sar = shift_op[2:1] == 2'b01;
	wire use_shl = (shift_op == 3'b110);
	wire use_rol = use_rotate && (shift_op[0] || use_shl); // should be active also for shl

	wire rcount_msb_mask = !(use_rotate && !wide && !(use_sar || use_shr)); // sar and shr can shift a byte > 7 steps to yield the sign/zero.
	wire [ROTATE_COUNT_BITS-1:0] rotate_count = {imm_full[ROTATE_COUNT_BITS-1] & rcount_msb_mask, imm_full[ROTATE_COUNT_BITS-1-1:0]};

	wire src1_zero = use_rol || (use_alt_op && add_or_adc);

	assign feed_imm8 = (cls == CLASS_SHIFT || use_mul) && data_stage;

	wire do_swap = (cls == CLASS_SWAP);

`ifdef USE_MULTIPLIER
	wire mul_only = use_mul && use_imm8;
	// Do an extended mul unless the destination register overlaps with `REG_INDEX_MUL_MSB_DEST
	wire extended_mul = !(reg_dest[2:1] == (`REG_INDEX_MUL_MSB_DEST>>1) && (wide || reg_dest[0] == (`REG_INDEX_MUL_MSB_DEST&1)));
`endif

	scheduler #( .LOG2_NR(4), .REG_BITS(REG_BITS), .NSHIFT(NSHIFT), .PAYLOAD_CYCLES(PAYLOAD_CYCLES) ) sched(
		.clk(clk), .reset(reset),
		.inst_valid(inst_valid), .inst_done(sc_inst_done),
		.wide(wide), .wide2(wide2),
		.op(op), .dest(dest), .reg_dest(reg_dest), .src1_from_pc(src1_from_pc), .src1_zero(src1_zero),
		.src(src), .reg_src(reg_src), .src_sext2(src_sext2), .double_src2(double_src2),
		.addr_wide2(addr_wide2), .addr_op(addr_op), .addr_reg1(addr_reg1), .addr_src(addr_src), .reg_addr_src(addr_reg2),
		.addr_src_sext2(addr_src_sext2), .update_dest(update_dest),
		.force_reverse_args(force_reverse_args), .invert_src2(invert_src2),
		.autoincdec(autoincdec_update), .addr_just_reg1(addr_just_reg1),
		.update_carry_flags(update_carry_flags), .update_other_flags(update_other_flags), .no_flag_updates(no_flag_updates),
		.use_cc(use_cc), .cc(cc),
		.use_rotate(use_rotate), .rotate_only(rotate_only), .use_shr(use_shr), .use_sar(use_sar), .use_shl(use_shl), .rotate_count(rotate_count),
		.do_swap(do_swap),
		.any_prefetched(any_prefetched), .load_imm16(load_imm16), .imm16_loaded(imm16_loaded),
		.imm_data_in(imm_data_in2), .next_imm_data(sc_next_imm_data),
		.imm8_data_out(imm8_data_out),
		.imm_full(imm_full),
`ifdef USE_MULTIPLIER
		.set_imm_top(set_imm_top), .next_imm_top_data(next_imm_top_data),
		.use_mul(use_mul), .mul_only(mul_only), .extended_mul(extended_mul),
`endif
		.reserve_tx(reserve_tx),
		.addr_stage(addr_stage), .data_stage(data_stage), .block_tx_reply(block_tx_reply),

		.block_prefetch(block_prefetch), .write_pc_now(write_pc), .ext_pc_next(ext_pc_next), .comp_counter(comp_counter),
		.prefetch_idle(prefetch_idle), .pc_data_in(pc_data_in), .pc_data_out(pc_data_out),

		.tx_reply_wanted(tx_reply_wanted),
		.tx_command_valid(tx_command_valid), .tx_command(tx_command), .tx_command_started(tx_command_started), .tx_active(tx_active),
		.tx_data(tx_data), .tx_data_next(tx_data_next), .tx_done(tx_done),
		.tx_counter(tx_counter),
		.rx_active(rx_active), .rx_sbs(rx_sbs), .rx_sbs_valid(rx_sbs_valid),
		.rx_started(rx_started), .rx_data_valid(rx_data_valid), .rx_done(rx_done), .rx_counter(rx_counter),
		.rx_pins(rx_pins)
	);
endmodule : decoder
