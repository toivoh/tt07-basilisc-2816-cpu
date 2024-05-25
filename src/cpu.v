/*
 * Copyright (c) 2024 Toivo Henningsson
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none

`include "common.vh"

// Currently, must have PREFETCH_QUEUE >= 2
module CPU #( parameter LOG2_NR=4, REG_BITS=8, IO_BITS=2, PAYLOAD_CYCLES=8, PREFETCH_QUEUE=2 ) (
		input wire clk, reset,

		output wire tx_fetch, // High when the message on tx_pins is for fetching instructions
		output wire tx_jump,  // High when the message on tx_pins is for fetching the first instruction after a jump
		output wire [IO_BITS-1:0] tx_pins,
		input wire [IO_BITS-1:0] rx_pins
	);
	localparam NSHIFT = IO_BITS;
	localparam MAX_OUTSTANDING = PREFETCH_QUEUE + 1; // scheduler can only have one outstanding read

	// PC control interface
	wire block_prefetch, write_pc, ext_pc_next;
	wire [$clog2(2*REG_BITS / NSHIFT)-1:0] comp_counter;
	wire prefetch_idle;
	wire [NSHIFT-1:0] pc_data_to_decoder, pc_data_to_prefetcher;

	// Memory interface
	// ================

	wire pf_tx_command_valid, sc_tx_command_valid;
	wire [`TX_CMD_BITS-1:0] pf_tx_command, sc_tx_command;
	wire [NSHIFT-1:0] pf_tx_data, sc_tx_data;

	// Prefer scheduler if both want to send a command, or if scheduler reserves the TX channel (write will follow read, same address implied).
	wire sc_reserve_tx;
	wire sc_tx_wanted = sc_tx_command_valid || sc_reserve_tx || block_prefetch;
	reg curr_sc_tx;
	always @(posedge clk) begin
		if (!tx_active) curr_sc_tx <= sc_tx_wanted;
	end

	wire sc_tx = tx_active ? curr_sc_tx : sc_tx_wanted;
	assign prefetch_idle = tx_active ? curr_sc_tx : block_prefetch;

	wire pf_tx = !sc_tx;
	assign tx_fetch = pf_tx || write_pc;
	assign tx_jump = write_pc;

	wire track_tx_command = (tx_command == `TX_HEADER_READ_16);

	// Choose TX source
	wire tx_command_valid              = (sc_tx ? sc_tx_command_valid : pf_tx_command_valid) && (!full || !track_tx_command);
	wire [`TX_CMD_BITS-1:0] tx_command =  sc_tx ? sc_tx_command       : pf_tx_command;
	wire [NSHIFT-1:0]       tx_data    =  sc_tx ? sc_tx_data          : pf_tx_data;

	wire tx_command_started, tx_active;
	wire tx_data_next, tx_done;
	wire [NSHIFT-1:0] rx_sbs;
	wire [$clog2(PAYLOAD_CYCLES)+1-1:0] rx_counter;
	wire rx_started, rx_active, rx_sbs_valid, rx_data_valid, rx_done;

	wire [$clog2(PAYLOAD_CYCLES)+1-1:0] tx_counter;

	memory_interface #( .IO_BITS(IO_BITS), .PAYLOAD_CYCLES(PAYLOAD_CYCLES) ) mem_if (
		.clk(clk), .reset(reset),
		.tx_command_valid(tx_command_valid), .tx_command(tx_command), .tx_command_started(tx_command_started), .tx_active(tx_active),
		.tx_data(tx_data), .tx_data_next(tx_data_next), .tx_done(tx_done),
		.tx_counter(tx_counter),
		.rx_started(rx_started), .rx_active(rx_active), .rx_sbs(rx_sbs), .rx_sbs_valid(rx_sbs_valid),
		.rx_data_valid(rx_data_valid), .rx_done(rx_done), .rx_counter(rx_counter),
		.tx_pins(tx_pins), .rx_pins(rx_pins)
	);

	// Transaction type FIFO
	// ---------------------

	localparam FIFO_BITS = 2;

	// Dont queue up write commands, since they don't get a response.
	// TODO: Update if the set of commands that get a response changes.
	wire add = tx_command_started && track_tx_command;
	wire sc_tx_reply_wanted;
	wire tx_reply_wanted = sc_tx ? sc_tx_reply_wanted : 1;
	// If write_pc is high, the transaction is counted as a prefetch
	wire [FIFO_BITS-1:0] new_entry = tx_reply_wanted ? {1'b1, sc_tx && !write_pc} : '0;
	wire remove = rx_done; // TODO: could remove already at rx_started, but then last_entry will not keep track of the current receiver.
	wire [FIFO_BITS-1:0] last_entry;
	wire empty, full;
	FIFO #( .BITS(FIFO_BITS), .DEPTH(MAX_OUTSTANDING) ) fifo (
		.clk(clk), .reset(reset),
		.add(add), .remove(remove), .new_entry(new_entry),
		.last_entry(last_entry), .empty(empty), .full(full)
	);
	wire sc_rx = ( last_entry[0]) && last_entry[1];
	wire pf_rx = (!last_entry[0]) && last_entry[1];


	// Prefetcher
	// ==========
	wire [15:0] inst;
	wire inst_valid, inst_done;

	localparam IMM_BITS = 2*REG_BITS;
	wire [IMM_BITS-1:0] imm_full;
	wire feed_imm8;
	wire [IO_BITS-1:0] imm8_data_to_pf;

	wire any_prefetched, load_imm16, imm16_loaded;
	wire [IO_BITS-1:0] imm_data;
	wire next_imm_data;

`ifdef USE_MULTIPLIER
	wire set_imm_top;
	wire [REG_BITS-1:0] next_imm_top_data;
`endif

	prefetcher #( .IO_BITS(IO_BITS), .PAYLOAD_CYCLES(PAYLOAD_CYCLES), .PREFETCH_DEPTH(PREFETCH_QUEUE - 1) ) pref (
		.clk(clk), .reset(reset),
		.inst(inst), .inst_valid(inst_valid), .inst_done(inst_done),
		.imm_reg(imm_full), .feed_imm8(feed_imm8), .imm8_data_in(imm8_data_to_pf),
`ifdef USE_MULTIPLIER
		.set_imm_top(set_imm_top), .next_imm_top_data(next_imm_top_data),
`endif
		.any_prefetched(any_prefetched), .load_imm16(load_imm16), .imm16_loaded(imm16_loaded),
		.imm_data_out(imm_data), .next_imm_data(next_imm_data),

		.write_pc(write_pc), .ext_pc_next(ext_pc_next), .comp_counter(comp_counter),
		.pc_data_out(pc_data_to_decoder), .pc_data_in(pc_data_to_prefetcher),

		.tx_command_valid(pf_tx_command_valid), .tx_command(pf_tx_command),
		.tx_command_started(pf_tx && tx_command_started), .tx_active(pf_tx && tx_active),
		.tx_data(pf_tx_data), .tx_data_next(pf_tx && tx_data_next), .tx_done(pf_tx && tx_done), .tx_counter(tx_counter),
		.rx_active(pf_rx && rx_active), .rx_sbs(rx_sbs), .rx_sbs_valid(pf_rx && rx_sbs_valid),
		.rx_started(pf_rx && rx_started), .rx_data_valid(pf_rx && rx_data_valid), .rx_done(pf_rx && rx_done), .rx_counter(rx_counter),
		.rx_pins(rx_pins)
	);


	// Decoder
	// =======
	decoder #( .LOG2_NR(LOG2_NR), .REG_BITS(REG_BITS), .NSHIFT(NSHIFT), .PAYLOAD_CYCLES(PAYLOAD_CYCLES) ) dec(
		.clk(clk), .reset(reset),
		.inst(inst), .inst_valid(inst_valid), .inst_done(inst_done),
		.imm_full(imm_full), .feed_imm8(feed_imm8), .imm8_data_out(imm8_data_to_pf),
`ifdef USE_MULTIPLIER
		.set_imm_top(set_imm_top), .next_imm_top_data(next_imm_top_data),
`endif
		.any_prefetched(any_prefetched), .load_imm16(load_imm16), .imm16_loaded(imm16_loaded),
		.imm_data_in(imm_data), .next_imm_data(next_imm_data),
		.reserve_tx(sc_reserve_tx),

		.block_prefetch(block_prefetch), .write_pc(write_pc), .ext_pc_next(ext_pc_next), .comp_counter(comp_counter),
		.prefetch_idle(prefetch_idle), .pc_data_in(pc_data_to_decoder), .pc_data_out(pc_data_to_prefetcher),

		.tx_reply_wanted(sc_tx_reply_wanted),
		.tx_command_valid(sc_tx_command_valid), .tx_command(sc_tx_command),
		.tx_command_started(sc_tx && tx_command_started), .tx_active(sc_tx && tx_active),
		.tx_data(sc_tx_data), .tx_data_next(sc_tx && tx_data_next), .tx_done(sc_tx && tx_done), .tx_counter(tx_counter),
		.rx_active(sc_rx && rx_active), .rx_sbs(rx_sbs), .rx_sbs_valid(sc_rx && rx_sbs_valid),
		.rx_started(sc_rx && rx_started), .rx_data_valid(sc_rx && rx_data_valid), .rx_done(sc_rx && rx_done), .rx_counter(rx_counter),
		.rx_pins(rx_pins)
	);
endmodule : CPU
