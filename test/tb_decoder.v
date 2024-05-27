`default_nettype none

`include "common.vh"

module tb_decoder #( parameter LOG2_NR=4, REG_BITS=8, NSHIFT=2 ) ();
	localparam IO_BITS = NSHIFT;
	localparam IMM_BITS = 16;

	// this part dumps the trace to a vcd file that can be viewed with GTKWave
	initial begin
		$dumpfile ("tb_decoder.vcd");
		$dumpvars (0, tb_decoder);
		#1;
	end


	// wire up the inputs and outputs
	reg clk;
	reg rst_n;

	wire reset = !rst_n;


`ifdef USE_MULTIPLIER
	localparam MUL_SUPPORTED = 1;
`else
	localparam MUL_SUPPORTED = 0;
`endif


	reg inst_valid = 0;

	reg [NSHIFT-1:0] rx_pins = 0;

	wire inst_done;
	wire [NSHIFT-1:0] tx_pins;

	reg [15:0] inst;

	reg [15:0] imm_data;
	wire feed_imm8;
	wire [IO_BITS-1:0] imm8_data_in;

`ifdef USE_MULTIPLIER
	wire set_imm_top;
	wire [REG_BITS-1:0] next_imm_top_data;
`endif

	//always @(posedge clk) if (next_imm_data) imm_data <= imm_data >> NSHIFT;
	always @(posedge clk) begin
`ifdef USE_MULTIPLIER
		if (set_imm_top)        imm_data[IMM_BITS-1:8] <= next_imm_top_data;
		else if (next_imm_data) imm_data[IMM_BITS-1:8] <= imm_data[IMM_BITS-1:8] >> IO_BITS;

		if (next_imm_data) imm_data[IMM_BITS-8-1:0] <= {feed_imm8 ? imm8_data_in : imm_data[7+IO_BITS -: IO_BITS], imm_data[7:IO_BITS]};
`else
		if (next_imm_data) imm_data <= {imm_data[IMM_BITS-1:8]>>IO_BITS, feed_imm8 ? imm8_data_in : imm_data[7+IO_BITS -: IO_BITS], imm_data[7:IO_BITS]};
`endif
	end

	wire [NSHIFT-1:0] imm_data_in = imm_data[NSHIFT-1:0];
	wire next_imm_data;

	localparam PAYLOAD_CYCLES = 16/NSHIFT;

	wire tx_command_valid;
	wire [`TX_CMD_BITS-1:0] tx_command;
	wire [NSHIFT-1:0] tx_data;

	wire tx_command_started, tx_active;
	wire tx_data_next, tx_done;
	wire [NSHIFT-1:0] rx_sbs;
	wire [$clog2(PAYLOAD_CYCLES)+1-1:0] rx_counter;
	wire rx_active, rx_sbs_valid;
	wire rx_started, rx_data_valid, rx_done;

	wire [$clog2(PAYLOAD_CYCLES)+1-1:0] tx_counter;

	wire load_imm16;
	wire imm16_loaded = load_imm16;
	decoder #( .LOG2_NR(LOG2_NR), .REG_BITS(REG_BITS), .NSHIFT(NSHIFT), .PAYLOAD_CYCLES(PAYLOAD_CYCLES) ) dec (
		.clk(clk), .reset(reset),
		.inst_valid(inst_valid), .inst(inst), .inst_done(inst_done), .imm_full(imm_data),
`ifdef USE_MULTIPLIER
		.set_imm_top(set_imm_top), .next_imm_top_data(next_imm_top_data),
`endif
		.load_imm16(load_imm16), .imm16_loaded(imm16_loaded),
		.next_imm_data(next_imm_data), .imm_data_in(imm_data_in),
		.feed_imm8(feed_imm8), .imm8_data_out(imm8_data_in),

		.tx_command_valid(tx_command_valid), .tx_command(tx_command), .tx_command_started(tx_command_started), .tx_active(tx_active),
		.tx_data(tx_data), .tx_data_next(tx_data_next), .tx_done(tx_done),
		.tx_counter(tx_counter),
		.rx_active(rx_active), .rx_sbs(rx_sbs), .rx_sbs_valid(rx_sbs_valid),
		.rx_started(rx_started), .rx_data_valid(rx_data_valid), .rx_done(rx_done), .rx_counter(rx_counter),
		.rx_pins(rx_pins)
	);

	memory_interface #( .IO_BITS(NSHIFT), .PAYLOAD_CYCLES(PAYLOAD_CYCLES) ) mem_if (
		.clk(clk), .reset(reset),
		.tx_command_valid(tx_command_valid), .tx_command(tx_command), .tx_command_started(tx_command_started), .tx_active(tx_active),
		.tx_data(tx_data), .tx_data_next(tx_data_next), .tx_done(tx_done),
		.tx_counter(tx_counter),
		.rx_started(rx_started),
		.rx_active(rx_active), .rx_sbs(rx_sbs), .rx_sbs_valid(rx_sbs_valid),
		.rx_data_valid(rx_data_valid), .rx_done(rx_done), .rx_counter(rx_counter),
		.tx_pins(tx_pins), .rx_pins(rx_pins)
	);

	always @(posedge clk) begin
		if (inst_done) inst_valid <= 0;
	end

	wire [REG_BITS-1:0] r0 = dec.sched.alu.registers.general_registers.regs[0];
	wire [REG_BITS-1:0] r1 = dec.sched.alu.registers.general_registers.regs[1];
	wire [REG_BITS-1:0] r2 = dec.sched.alu.registers.general_registers.regs[2];
	wire [REG_BITS-1:0] r3 = dec.sched.alu.registers.general_registers.regs[3];
	wire [REG_BITS-1:0] r4 = dec.sched.alu.registers.general_registers.regs[4];
	wire [REG_BITS-1:0] r5 = dec.sched.alu.registers.general_registers.regs[5];
	wire [REG_BITS-1:0] r6 = dec.sched.alu.registers.general_registers.regs[6];
	wire [REG_BITS-1:0] r7 = dec.sched.alu.registers.general_registers.regs[7];
	wire [REG_BITS-1:0] sp = dec.sched.alu.registers.sp_register.regs;

	localparam OP_BITS = `OP_BITS;
	localparam OP_BIT_NADD = `OP_BIT_NADD;
	localparam OP_BIT_SUB = `OP_BIT_SUB;
	localparam OP_BIT_WCARRY = `OP_BIT_WCARRY;
	localparam OP_ADD = `OP_ADD;
	localparam OP_SUB = `OP_SUB;
	localparam OP_ADC = `OP_ADC;
	localparam OP_SBC = `OP_SBC;
	localparam OP_AND = `OP_AND;
	localparam OP_OR = `OP_OR;
	localparam OP_XOR = `OP_XOR;
	localparam OP_MOV = `OP_MOV;
	localparam ADDR_OP_BITS = `ADDR_OP_BITS;
	localparam ADDR_OP_ADD = `ADDR_OP_ADD;
	localparam ADDR_OP_MOV = `ADDR_OP_MOV;
	localparam DEST_BITS = `DEST_BITS;
	localparam DEST_REG = `DEST_REG;
	localparam DEST_MEM = `DEST_MEM;
	localparam SRC_BITS = `SRC_BITS;
	localparam SRC_BIT_IMM = `SRC_BIT_IMM;
	localparam SRC_REG = `SRC_REG;
	localparam SRC_MEM = `SRC_MEM;
	//localparam SRC_REG_ZEXT = `SRC_REG_ZEXT;
	//localparam SRC_REG_SEXT = `SRC_REG_SEXT;
	localparam SRC_IMM2 = `SRC_IMM2;
	localparam SRC_IMM6 = `SRC_IMM6;
	localparam SRC_IMM7 = `SRC_IMM7;
	localparam SRC_IMM8 = `SRC_IMM8;
	localparam TX_CMD_BITS = `TX_CMD_BITS;
	localparam TX_HEADER_READ_16 = `TX_HEADER_READ_16;
	localparam TX_HEADER_WRITE_8 = `TX_HEADER_WRITE_8;
	localparam TX_HEADER_WRITE_16 = `TX_HEADER_WRITE_16;
	localparam RX_SB_READ_16 = `RX_SB_READ_16;
endmodule
