/*
 * Copyright (c) 2024 Toivo Henningsson
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none

`include "common.vh"

module prefetcher #( parameter IO_BITS=2, PAYLOAD_CYCLES=8, PREFETCH_DEPTH=1, IMM_BITS=16 ) (
		input wire clk, reset,

		output wire [15:0] inst,
		output wire inst_valid,
		input wire inst_done,

		output reg [IMM_BITS-1:0] imm_reg,
		input wire feed_imm8,
		input wire [IO_BITS-1:0] imm8_data_in,
`ifdef USE_MULTIPLIER
		input wire set_imm_top,
		input wire [IMM_BITS-8-1:0] next_imm_top_data,
`endif

		output wire any_prefetched,
		input wire load_imm16,
		output wire imm16_loaded,
		output wire [IO_BITS-1:0] imm_data_out,
		input wire next_imm_data,

		// PC control interface
		input wire write_pc, // Must be held high during entire transaction, actual write happens when ext_pc_next is high at the same time
		input wire ext_pc_next,
		input wire [$clog2(PAYLOAD_CYCLES)-1:0] comp_counter,
		input wire [IO_BITS-1:0] pc_data_in,
		output wire [IO_BITS-1:0] pc_data_out,

		// TX interface
		output wire tx_command_valid,
		output wire [`TX_CMD_BITS-1:0] tx_command,
		input wire tx_command_started, // only high when starting a prefetch command
		input wire tx_active,

		output wire [IO_BITS-1:0] tx_data,
		input wire tx_data_next,
		input wire [$clog2(PAYLOAD_CYCLES)+1-1:0] tx_counter,
		input wire tx_done,

		// RX interface
		input wire rx_started,
		input wire rx_active,
		input wire [IO_BITS-1:0] rx_sbs,
		input wire rx_sbs_valid,
		input wire rx_data_valid, // only high when receiving prefetch data
		input wire [$clog2(PAYLOAD_CYCLES)+1-1:0] rx_counter,
		input wire rx_done, // only high when receiving prefetch data
		input wire [IO_BITS-1:0] rx_pins
	);
	localparam INST_BITS = PAYLOAD_CYCLES*IO_BITS;
	// pc is scanned around and updated until tx command is finished, better have PC_BITS = PAYLOAD_BITS
	localparam PC_BITS = PAYLOAD_CYCLES*IO_BITS;
	//localparam NE_BITS = $clog2(PREFETCH_DEPTH+1);
	localparam NP_BITS = $clog2(PREFETCH_DEPTH+2);

	wire compensate = ext_pc_next;
	wire update_pc  = !ext_pc_next || write_pc;

	// Prefetch queue
	// ==============

	wire add, remove;
	wire [INST_BITS-1:0] new_entry;

	wire [INST_BITS-1:0] last_entry;
	wire can_add, last_valid;
	//wire [NE_BITS-1:0] num_entries;

	SRFIFO #( .DEPTH(PREFETCH_DEPTH), .BITS(INST_BITS) ) fifo (
		.clk(clk), .reset(reset),
		.add(add), .remove(remove), .new_entry(new_entry),
		.last_entry(last_entry),
		.can_add(can_add), .last_valid(last_valid)
		//.num_entries(num_entries),
	);

	reg [INST_BITS-1:0] sreg;
	reg sreg_full;
	always @(posedge clk) begin
		if (reset) begin
			sreg_full <= 0;
		end else begin
			if (rx_done) sreg_full <= 1;
			else if (can_add) sreg_full <= 0;
		end

		if (rx_data_valid) sreg <= {rx_pins, sreg[INST_BITS-1:IO_BITS]};
	end

	assign new_entry = sreg;
	assign add = sreg_full && can_add;

	reg [INST_BITS-1:0] inst_reg;
	//reg [IMM_BITS-1:0] imm_reg;
	reg inst_reg_valid;

	assign inst_valid = inst_reg_valid;
	assign inst = inst_reg;
	assign imm_data_out = imm_reg[IO_BITS-1:0];

	// An instruction word leaves the prefetch stage when it enters the instruction register or is flushed, which happens when remove is high.
	wire pre_discard = (num_flushed != 0);
	assign remove = last_valid && (pre_discard || !inst_valid || load_imm16);
	wire discard =        remove && pre_discard;
	wire load_inst_reg =  remove && !pre_discard && !inst_valid;
	assign imm16_loaded = remove && !pre_discard && inst_valid;

	always @(posedge clk) begin
		if (reset) begin
			inst_reg_valid <= 0;
		end else if (load_inst_reg) begin
			inst_reg_valid <= 1;
			inst_reg <= last_entry;
			imm_reg <= last_entry[IMM_BITS-1:0];
		end else if (imm16_loaded) begin
			imm_reg <= last_entry[IMM_BITS-1:0];
		end else begin
			if (inst_done) inst_reg_valid <= 0;

			//if (next_imm_data) imm_reg <= imm_reg >> IO_BITS;
`ifdef USE_MULTIPLIER
			if (set_imm_top)        imm_reg[IMM_BITS-1:8] <= next_imm_top_data;
			else if (next_imm_data) imm_reg[IMM_BITS-1:8] <= imm_reg[IMM_BITS-1:8] >> IO_BITS;

			if (next_imm_data) imm_reg[IMM_BITS-8-1:0] <= {feed_imm8 ? imm8_data_in : imm_reg[7+IO_BITS -: IO_BITS], imm_reg[7:IO_BITS]};
`else
			if (next_imm_data) imm_reg <= {imm_reg[IMM_BITS-1:8]>>IO_BITS, feed_imm8 ? imm8_data_in : imm_reg[7+IO_BITS -: IO_BITS], imm_reg[7:IO_BITS]};
`endif
		end
	end

	//assign inst = last_entry;
	//assign inst_valid = last_valid;

	// Prefetcher
	// ==========
	wire pc_start;
	wire pc_done;

	// How many instruction words have we sent a read message for but not yet executed?
	// How many that we don't plan to execute?
	reg [NP_BITS-1:0] num_prefetched, num_flushed;
	wire [NP_BITS+IO_BITS-1:0] num_prefetched_ext = {{(IO_BITS-1){1'b0}}, num_prefetched, 1'b1}; // Put a one in the LSB since we will invert it.
	always @(posedge clk) begin
		if (reset) begin
			num_prefetched <= 0;
			num_flushed <= 0;
		end else begin
			if (write_pc && pc_done) num_prefetched <= 1;
			else num_prefetched <= num_prefetched + ({{(NP_BITS-1){1'b0}}, pc_done && update_pc} - {{(NP_BITS-1){1'b0}}, load_inst_reg || imm16_loaded});

			// Set num_flushed <= num_prefetched only once per pc write, before inst_done goes high.
			if (write_pc && pc_start) num_flushed <= num_prefetched; // Assume that num_flushed = 0 when this happens; we have a valid instruction.
			else num_flushed <= num_flushed - discard;
		end
	end

	reg carry;
	reg [PC_BITS-1:0] pc;

	assign tx_command = `TX_HEADER_READ_16;
	assign tx_command_valid = num_prefetched < PREFETCH_DEPTH + 1; // sreg holds extra prefetch space

	// not a register
	reg [IO_BITS-1:0] delta_pc_bits;
	always @(*) begin
		if (compensate) begin
			if (comp_counter*IO_BITS < NP_BITS+1) delta_pc_bits = ~num_prefetched_ext[IO_BITS-1 + comp_counter*IO_BITS -: IO_BITS];
			else                                  delta_pc_bits = '1;
		end else begin
			delta_pc_bits = 0; //(tx_counter == 0) ? 2'd2 : 0;
		end
	end

	wire counter_at_zero = compensate ? (comp_counter == 0) : (tx_counter == 0);
	assign pc_start = pc_next && counter_at_zero;

	wire [1:0] carry_in = counter_at_zero ? 2'd2 : {1'b0, carry};
	// TODO: better way to express the adder?
	wire [IO_BITS-1:0] pc_term_bits = pc[IO_BITS-1:0];
	wire [IO_BITS+1-1:0] sum = {1'b0, pc_term_bits} + {1'b0, delta_pc_bits} + {{(IO_BITS-1){1'b0}}, carry_in};

	//assign tx_data = pc[IO_BITS-1:0];
	assign tx_data = write_pc ? pc_data_in : sum[IO_BITS-1:0];
	assign pc_data_out = sum[IO_BITS-1:0];

	wire pc_next = tx_data_next || ext_pc_next;
	always @(posedge clk) begin
		if (reset) begin
			// We will start to fetch from pc+2.
			pc <= 16'hfffa; // Start fetching from 'hfffc, just enough for a jump to anywhere before wrapping around to zero.
			//pc <= '0;
		end else begin
			//if (pc_next) pc <= {write_pc ? pc_data_in : (update_pc ? sum[IO_BITS-1:0] : pc[IO_BITS-1:0]), pc[PC_BITS-1:IO_BITS]};
			if (pc_next) pc <= {update_pc ? tx_data : pc[IO_BITS-1:0], pc[PC_BITS-1:IO_BITS]};
		end

		carry <= sum[IO_BITS];
	end

	assign pc_done = pc_next && (compensate ? (comp_counter == PAYLOAD_CYCLES-1) : (tx_counter == PAYLOAD_CYCLES-1));
	assign any_prefetched = (num_prefetched != 0);
endmodule : prefetcher
