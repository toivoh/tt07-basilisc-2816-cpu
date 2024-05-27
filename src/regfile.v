/*
 * Copyright (c) 2024 Toivo Henningsson
 * SPDX-License-Identifier: Apache-2.0
 */

`default_nettype none

`include "common.vh"

/*
Bit serial register file with 2 read/write ports.
When scanning a register, scan out the bottom NSHIFT bits each cycle, and scan in the top NSHIFT bits.
*/
module regfile #( parameter LOG2_NR=3, REG_BITS=8, NSHIFT=2 ) (
		input wire clk, reset,

		input wire [LOG2_NR-1:0] reg_index, reg_index2,

		input wire do_scan, do_scan2,
		// If both indices are the same and scanning, scan_in is used over scan_in2
		input wire [NSHIFT-1:0] scan_in, scan_in2,
		output wire [NSHIFT-1:0] scan_out, scan_out2
	);

	localparam NUM_REGS = 2**LOG2_NR;

	genvar i;

	reg [REG_BITS-1:0] regs[NUM_REGS];

	assign scan_out  = regs[reg_index ][NSHIFT-1:0];
	assign scan_out2 = regs[reg_index2][NSHIFT-1:0];

	generate
		for (i = 0; i < NUM_REGS; i++) begin
			wire active_reg  = (i == reg_index)  & do_scan;
			wire active_reg2 = (i == reg_index2) & do_scan2;
			// Prefer scan_in over scan_in2
			wire [NSHIFT-1:0] reg_scan_in = active_reg ? scan_in : scan_in2;
			wire reg_scan = active_reg | active_reg2;

			always @(posedge clk) begin
				if (reg_scan) regs[i] <= {reg_scan_in, regs[i][REG_BITS-1:NSHIFT]};
			end
		end
	endgenerate
endmodule


/*
Like regfile but just a single register
*/
module regfile_single #( parameter REG_BITS=8, NSHIFT=2 ) (
		input wire clk, reset,

		input wire do_scan, //do_scan2,
		// If both indices are the same and scanning, scan_in is used over scan_in2
		input wire [NSHIFT-1:0] scan_in, //scan_in2,
		output wire [NSHIFT-1:0] scan_out
	);

	reg [REG_BITS-1:0] regs;

	assign scan_out  = regs[NSHIFT-1:0];

	wire active_reg  = do_scan;
	//wire active_reg2 = do_scan2;
	// Prefer scan_in over scan_in2
	//wire [NSHIFT-1:0] reg_scan_in = active_reg ? scan_in : scan_in2;
	//wire reg_scan = active_reg | active_reg2;

	wire [NSHIFT-1:0] reg_scan_in = scan_in;
	wire reg_scan = active_reg;

	always @(posedge clk) begin
		if (reg_scan) regs <= {reg_scan_in, regs[REG_BITS-1:NSHIFT]};
	end
endmodule


/*
Bit serial register file with 2 read/write ports.
When scanning a register, scan out the bottom NSHIFT bits each cycle, and scan in the top NSHIFT bits.
Compound register file with 8 general purpose registers and some special purpose ones in the upper half.
	*/
module regfile_top #( parameter LOG2_NR=4, REG_BITS=8, NSHIFT=2 ) (
		input wire clk, reset,
		input wire [$clog2(REG_BITS*2/NSHIFT)-1:0] bit_index,

		input wire [LOG2_NR-1:0] reg_index, reg_index2,

		input wire [REG_BITS-1:0] flags,
		//output wire write_flags,
		output wire [NSHIFT-1:0] flags_data_scan_in,

		input wire do_scan, do_scan2,
		// If both indices are the same and scanning, scan_in is used over scan_in2
		input wire [NSHIFT-1:0] scan_in, scan_in2,
		output wire [NSHIFT-1:0] scan_out, scan_out2
	);
	localparam GR_LOG2_NR = 3;

	// General registers
	// =================

	wire gr_match  = (reg_index[ LOG2_NR-1:GR_LOG2_NR] == 0);
	wire gr_match2 = (reg_index2[LOG2_NR-1:GR_LOG2_NR] == 0);

	wire gr_do_scan  = do_scan  && gr_match;
	wire gr_do_scan2 = do_scan2 && gr_match2;
	wire [NSHIFT-1:0] gr_scan_out, gr_scan_out2;
	regfile #(.LOG2_NR(GR_LOG2_NR), .REG_BITS(REG_BITS), .NSHIFT(NSHIFT)) general_registers(
		.clk(clk), .reset(reset),
		.reg_index(reg_index[GR_LOG2_NR-1:0]), .reg_index2(reg_index2[GR_LOG2_NR-1:0]),
		.do_scan(gr_do_scan), .do_scan2(gr_do_scan2),
		.scan_in(scan_in), .scan_in2(scan_in2),
		.scan_out(gr_scan_out), .scan_out2(gr_scan_out2)
	);

	// Special registers
	// =================

	wire sr_match  = (reg_index[ LOG2_NR-1:GR_LOG2_NR] == 1);
	wire sr_match2 = (reg_index2[LOG2_NR-1:GR_LOG2_NR] == 1);

	// Assume that only one of the ports can match at any given time
	wire [LOG2_NR-1:0] reg_index_sr = sr_match ? reg_index : reg_index2;
	wire [NSHIFT-1:0]  sr_scan_in   = sr_match ? scan_in   : scan_in2;

	wire sr_do_scan  = (do_scan && sr_match) || (do_scan2 && sr_match2);

	// Stack pointer sp
	// ----------------
	wire sp_match    = (reg_index_sr[1:0] == 0);
	wire sp_do_scan  = sr_do_scan && sp_match;

	wire [NSHIFT-1:0] sp_scan_out;
	regfile_single #(.REG_BITS(REG_BITS), .NSHIFT(NSHIFT)) sp_register(
		.clk(clk), .reset(reset),
		.do_scan(sp_do_scan),
		.scan_in(sr_scan_in),
		.scan_out(sp_scan_out)
	);

	// Stack pointer top
	wire [NSHIFT-1:0] sp_top_scan_out = (bit_index[$clog2(REG_BITS/NSHIFT)-1:0] == 0) ? 2'd1 : '0;

	// Flags register
	//wire flags_match = (reg_index_sr[1] == 1); // Matches on both 2 and 3
	//assign write_flags = flags_match && sr_do_scan;
	assign flags_data_scan_in = sr_scan_in;

	// Special output mux
	//wire [NSHIFT-1:0] sr_scan_out = sp_match ? sp_scan_out : sp_top_scan_out;
	reg [NSHIFT-1:0] sr_scan_out;
	always @(*) begin
		sr_scan_out = 'X;
		case (reg_index_sr[1:0])
			0: sr_scan_out = sp_scan_out;
			1: sr_scan_out = sp_top_scan_out;
			2, 3: begin
				if      (bit_index[1:0] == 0) sr_scan_out = flags[1:0];
				else if (bit_index[1:0] == 1) sr_scan_out = flags[3:2];
				else sr_scan_out = 0; // Matches on both 2 and 3
			end
		endcase
	end

	wire [NSHIFT-1:0] sr_scan_out2 = sr_scan_out;

	// Output mux
	// ==========
	assign scan_out  = gr_match  ? gr_scan_out  : sr_scan_out;
	assign scan_out2 = gr_match2 ? gr_scan_out2 : sr_scan_out2;
endmodule
