`default_nettype none
`timescale 1ns / 1ps

`include "../src/common.vh"

module tb ();

	// Dump the signals to a VCD file. You can view it with gtkwave.
	initial begin
		$dumpfile("tb.vcd");
		$dumpvars(0, tb);
		#1;
	end

	// Wire up the inputs and outputs:
	reg clk;
	reg rst_n;
	reg ena;
	reg [7:0] ui_in;
	reg [7:0] uio_in;
	wire [7:0] uo_out;
	wire [7:0] uio_out;
	wire [7:0] uio_oe;

	tt_um_toivoh_basilisc_2816 top (

		// Include power ports for the Gate Level test:
`ifdef GL_TEST
		.VPWR(1'b1),
		.VGND(1'b0),
`endif

		.ui_in  (ui_in),    // Dedicated inputs
		.uo_out (uo_out),   // Dedicated outputs
		.uio_in (uio_in),   // IOs: Input path
		.uio_out(uio_out),  // IOs: Output path
		.uio_oe (uio_oe),   // IOs: Enable path (active high: 0=input, 1=output)
		.ena    (ena),      // enable - goes high when design is selected
		.clk    (clk),      // clock
		.rst_n  (rst_n)     // not reset
	);

`ifndef GL_TEST
	//wire [15:0] entry0 = top.cpu.pref.fifo.entries[0];

	localparam MSG_TYPE_BITS = 2; //top.cpu.fifo.BITS;
	wire [MSG_TYPE_BITS-1:0] msg_entry1 = top.cpu.fifo.entries[1];
	wire [MSG_TYPE_BITS-1:0] msg_entry2 = top.cpu.fifo.entries[2];
	wire [MSG_TYPE_BITS-1:0] msg_entry3 = top.cpu.fifo.entries[3];

	localparam REG_BITS = 8;
	wire [REG_BITS-1:0] r0 = top.cpu.dec.sched.alu.registers.general_registers.regs[0];
	wire [REG_BITS-1:0] r1 = top.cpu.dec.sched.alu.registers.general_registers.regs[1];
	wire [REG_BITS-1:0] r2 = top.cpu.dec.sched.alu.registers.general_registers.regs[2];
	wire [REG_BITS-1:0] r3 = top.cpu.dec.sched.alu.registers.general_registers.regs[3];
	wire [REG_BITS-1:0] r4 = top.cpu.dec.sched.alu.registers.general_registers.regs[4];
	wire [REG_BITS-1:0] r5 = top.cpu.dec.sched.alu.registers.general_registers.regs[5];
	wire [REG_BITS-1:0] r6 = top.cpu.dec.sched.alu.registers.general_registers.regs[6];
	wire [REG_BITS-1:0] r7 = top.cpu.dec.sched.alu.registers.general_registers.regs[7];
	wire [REG_BITS-1:0] sp = top.cpu.dec.sched.alu.registers.sp_register.regs;
`endif

`ifdef USE_MULTIPLIER
	localparam MUL_SUPPORTED = 1;
`else
	localparam MUL_SUPPORTED = 0;
`endif

	localparam CC_ALWAYS = `CC_ALWAYS;
	localparam CC_Z = `CC_Z;
	localparam CC_NZ = `CC_NZ;
	localparam CC_S = `CC_S;
	localparam CC_NS = `CC_NS;
	localparam CC_C = `CC_C;
	localparam CC_NC = `CC_NC;
	localparam CC_A = `CC_A;
	localparam CC_NA = `CC_NA;
	localparam CC_V = `CC_V;
	localparam CC_NV = `CC_NV;
	localparam CC_G = `CC_G;
	localparam CC_NG = `CC_NG;
	localparam CC_AE = `CC_AE;
	localparam CC_NB = `CC_NB;
	localparam CC_NAE = `CC_NAE;
	localparam CC_B = `CC_B;
	localparam CC_NBE = `CC_NBE;
	localparam CC_BE = `CC_BE;
	localparam CC_GE = `CC_GE;
	localparam CC_NL = `CC_NL;
	localparam CC_NGE = `CC_NGE;
	localparam CC_L = `CC_L;
	localparam CC_NLE = `CC_NLE;
	localparam CC_LE = `CC_LE;

endmodule
