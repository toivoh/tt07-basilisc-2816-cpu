
`define USE_MULTIPLIER

`define OP_BITS 3
`define OP_BIT_NADD 2
`define OP_BIT_SUB 0
`define OP_BIT_WCARRY 1

`define OP_ADD 3'd0
`define OP_SUB 3'd1 // Interpeted as revsub when arg2 is an imm6
`define OP_ADC 3'd2
`define OP_SBC 3'd3
`define OP_AND 3'd4
`define OP_OR  3'd5
`define OP_XOR 3'd6
`define OP_MOV 3'd7


`define ADDR_OP_BITS 1
`define ADDR_OP_ADD 1'd0
`define ADDR_OP_MOV 1'd1


`define DEST_BITS 2
`define DEST_REG  2'd0
`define DEST_MEM  2'd1
`define DEST_PC   2'd2
`define DEST_IMM8 2'd3

`define SRC_BITS 4
`define SRC_BIT_IMM 3
//`define SRC_REG      3'd0
//`define SRC_MEM      3'd1
//`define SRC_REG_ZEXT 3'd2
//`define SRC_REG_SEXT 3'd3
//`define SRC_IMM2     3'd4
//`define SRC_IMM6     3'd5
//`define SRC_IMM7     3'd6
//`define SRC_IMM8     3'd7
`define SRC_REG      4'd0
`define SRC_MEM      4'd1
`define SRC_IMM2     4'd8
`define SRC_IMM6     4'd9
`define SRC_IMM7     4'd10
`define SRC_IMM8     4'd11
`define SRC_IMM16    4'd12


`define CC_BITS 4
`define CC_BIT_INVERTED          0
`define CC_BIT_INEQUALITY        3
`define CC_BIT_SIGNED_INEQUALITY 2
`define CC_BIT_INEQUALITY_A      1
`define CC_BIT_S                 1
`define CC_MASK_NSPECIAL         4'b1100

`define CC_ALWAYS 4'b0000
`define CC_CALL   4'b0001
//`define CC_DJNZ0  4'b0010
//`define CC_DJNZ0  4'b0011

`define CC_Z      4'b0100
`define CC_NZ     4'b0101
`define CC_S      4'b0110
`define CC_NS     4'b0111

`define CC_C      4'b1000
`define CC_NC     4'b1001
`define CC_A      4'b1010 // c && nz
`define CC_NA     4'b1011
`define CC_V      4'b1100
`define CC_NV     4'b1101
`define CC_G      4'b1110
`define CC_NG     4'b1111

`define CC_AE  `CC_C
`define CC_NB  `CC_C
`define CC_NAE `CC_NC
`define CC_B   `CC_NC

`define CC_NBE `CC_A
`define CC_BE  `CC_NA

`define CC_GE  `CC_V
`define CC_NL  `CC_V
`define CC_NGE `CC_NV
`define CC_L   `CC_NV

`define CC_NLE `CC_G
`define CC_LE  `CC_NG


`define REG_INDEX_MUL_MSB_DEST  7
`define REG_INDEX_SP            8
`define REG_INDEX_FLAGS        10


// CPU module assumes that only TX_HEADER_READ_16 gets a response
`define TX_CMD_BITS 2
`define TX_HEADER_READ_16  2'd0
`define TX_HEADER_WRITE_8  2'd2
`define TX_HEADER_WRITE_16 2'd3

`define RX_SB_READ_16 2'd1
