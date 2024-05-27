<!---

This file is used to generate your project datasheet. Please fill in the information below and delete any unused
sections.

You can also include images in this folder and reference them in the markdown. Each image must be less than
512 kb in size, and the combined size of all images must be less than 1 MB.
-->

## Overview

Basilisc-2816 v0.1 is a small 2-bit serial 2/8/16 bit processor that fits into one Tiny Tapeout tile.
The CPU has been designed around the constraints of

- small area,
- 4 pin serial memory interface to a RAM emulator implemented in a RP2040 microcontroller, which can be supported by the RP2040 microcontroller on the Tiny Tapeout 06 Demo Board.

and a later version is intended to be included in the next version of https://github.com/toivoh/tt06-retro-console (motivating the constraints).

Features:

- 2-bit serial execution:
	- ALU operations etc are calculated 2 bits/cycle
	- 2-bit-serial register file with two read/write ports realized as a bank of shift registers
	- Saves area compared to processing 8/16 bits per cycle
	- No point in calculating faster than the memory interface allows
- 8x 8-bit registers that can be paired into 4x 16-bit registers
- 16 bits/instruction (some instructions might take an additional 16 bit immediate)
- 64 kB address space
- Quite regular and orthogonal instruction encoding
- Many addressing modes
- variable shift instructions
- 8x8 and 8x16 bit multiply instructions, producing 2 result bits per cycle like everything else
- 13 branch conditions + relative call
- instruction prefetch queue

Contents:

- Programmer's view
- Execution timing
- Interface
- How it works

## Programmer's view
### Registers

The CPU has the following registers:
- 8 general purpose registers 8 bit registers `a - h`,
	- which can be grouped into four general purpose 16 bit register pairs `ba / dc / fe / hg`.
- 16 bit program counter `pc`, keeping the adress of the current instruction.
- Stack pointer register pair `sp`:
	- bottom half `p` is an 8 bit register,
	- top half `s` always reads as 1, making all stack operations work on the address range `0x100 - 0x1ff` (actually, `0xff - 0x1ff`).
- Flags `flags`:
	- zero flag `z`, (bit 0)
	- sign flag `s`, (bit 1)
	- signed carry flag `v`, (bit 2)
	- carry flag `c` (bit 3).

At reset `pc` starts at `0xfffc`, which is just enough to fit a `jump imm16` instruction before the top of the address space.
All other registers are uninitialized at reset.

### Instructions
Most instructions operate on one general purpose register and one additional operand.
The `branch, jump, call, ret` instructions are always 16 bit; all others can be 8 bit or 16 bit.
8 bit instructions operate on 8 bit registers and 16 bit instructions operate on register pairs.

In the descriptions below,
- `reg` stands for a general purpose register for 8 bit instructions or general purpose register pair for 16 bit instructions.
- `dest/src` can be a general purpose register (or pair), a memory location, or some other things (described under addressing modes below).
- `imm4/imm6/imm8` is a 4/6/8 bit immediate value, usually sign extended.
- `[zp]` is a _zero page memory location_ (described under addressing modes below).

The following groups of instructions are supported:

#### `mov dest, src`
Copy the value from `src` to `dest`. Supported forms:

	mov reg, src
	mov dest, reg
	mov reg, imm8  // imm8 is signed
	mov reg, [zp]
	mov [zp], reg

#### `swap dest, reg`
Swap the value of `dest` and `reg`. Supported forms:

	swap dest, reg
	swap [zp], reg

#### `binop dest, src`
Perform a binary operation on `dest` and `src`, update `dest` with the result, and update `flags` to reflect the result.
The binary operation can be

	add dest, src  // dest = dest + src
	sub dest, src  // dest = dest - src
	adc dest, src  // dest = dest + src + carry
	sbc dest, src  // dest = dest - src + carry - 1
	and dest, src  // dest = dest & src
	or  dest, src  // dest = dest | src
	xor dest, src  // dest = dest ^ src

The following froms are supported:

	binop reg, src
	binop dest, reg
	binop reg, imm6  // imm6 is signed
	binop reg, [zp]
	binop [zp], reg

There some additional binary operations

	revsub dest, src  // dest = src - dest
	revsbc dest, src  // dest = src - dest + carry - 1
	cmp    dest, src  // Update flags according to dest - src
	test   dest, src  // Update flags according to dest & src

where `cmp` and `test` don't update the destination,
which support fewer forms:

	revsub reg, imm6  // replaces sub dest, imm6
	revsub reg1, reg2

	revsbc reg, imm6  // replaces sbc dest, imm6
	revsbc reg1, reg2

	cmp reg, src
	cmp reg, imm6
	cmp reg, [zp]

	test reg, src
	test reg, [zp]

To get a non-reverse `sub reg, imm6`m use `add reg, -imm6` instead.

Additionally, the `alt_binop reg1, reg2` form is supported for the operations

	neg     dest, src  // dest = -src
	revsub  dest, src  // dest = src - dest
	negc    dest, src  // dest = -src + carry - 1
	revsbc  dest, src  // dest = src - dest + carry - 1
	and_not dest, src  // dest = dest & ~src
	or_not  dest, src  // dest = dest | ~src
	xor_not dest, src  // dest = dest ^ ~src
	not     dest, src, // dest = ~src

Binary operations that include addition or subtraction update the `c, v` flags. All binary operations update the `s, z` flags.

#### `shift reg, src8`
Perform a shift operation on `reg` using a shift count from `src`.
The shift operation can be

	shl reg, src8  // reg = reg << src
	shr reg, src8  // reg = reg >> src, unsigned shift
	sar reg, src8  // reg = reg >> src, signed shift
	rol reg, src8  // rotate reg left
	ror reg, src8  // rotate reg right

Supported forms:

	shift reg, src8
	shift reg, imm4

The `src8` argument is always taken to be 8 bit even for 16 bit shifts.
16 bit shifts always use the bottom 4 bits of the shift count.
`shr` and `sar` use the bottom 4 bits also for 8 bit shifts, while the others use the bottom 3 bits. (Timing wise, right shifts use the bottom 4 bits and left shifts use the bottom 3 for 8 bit shifts).

#### `mul reg, src`
Unsigned multiply of 8/16 bit `reg` and 8 bit `src`, producing a 16/24 bit result.
Store the bottom part of the result in `reg`. Store the top 8 bits in `h`, unless `reg` is `h` or `hg`. (Instruction takes 4 cycles less if top 8 bits are not stored).
Supported forms:

	mul reg, src8
	mul reg, imm6  // imm6 is unsigned

`reg` can not be `a`, `b`, or `ba`.

#### `branch cc, imm8`
Relative conditional branch: if the specified condition is true, jump `imm8` instruction words ahead of the current instruction. `imm8` is signed.
`branch cc, 0` jumps to itself.

Supported conditions:

	always
	call          // like always, but push address of next instruction before branching

	z / e         // zero / equal
	nz / ne       // not zero / not equal
	s             // signed
	ns            // not signed

	// unsigned comparisons:
	c / ae / nb   // carry / above equal / not below
	nc / nae / b  // not carry / not above equal / below
	a / nbe       // above / not below
	na / be       // not above / below

	// signed comparisons:
	v / ge / nl   // signed carry / greater equal / not less
	nv / nge / l  // not signed carry / not greater equal / less
	g / nle       // greater / not less
	ng / le       // not greater / less

#### `jump src16`, `call src16`, `ret`
Abslute unconditional jump to `pc = src16`.

Supported forms:

	jump src16
	jump [zp]

	call src16

`call` is like `jump`, but push address of next instruction before jumping.

`ret` is a pseudoinstruction for `jump [pop]`, which pops a `pc` value from the stack and jumps to it.

### Addressing modes
For instructions that use a `dest` operand, `dest` can be one of

	reg               
	flags             // Only for 8 bit operations. Not for shift/mul.
	sp                
	[r16 + zext(r8)]  // r16 and r8 can not overlap.
	[r16++]           // Postincrement: increase r16 after calculating
	                     the address. Increase by 2 for 16 bit operands.
	[--r16]           // Predecrement: decrease r16 after calculating
	                     the address. Decrease by 2 for 16 bit operands.
	[r16 + zext(imm2)]// imm2  is multiplied by 2 for 16 bit operands.
	[imm16]           // imm16 follows in the next instruction word.
	[push] / [--sp]   // Push the result onto the stack.
	                     Only for operations that don't depend on
	                     the value of dest.
	                     Decrease `sp` by 2 for 16 bit operands.
	[sp]              // Only for operations that depend on
	                     the value of dest.

For instructions that use a `src` operand, `src` can be anything that `dest` can be except `[pop], [sp]`, and can additionally be

	sext(r8)        // Sign extend r8, only for 16 bit operations.
	zext(r8)        // Zero extend r8, only for 16 bit operations,
	                   not for cmp and test.
	imm16           // imm16 follows in the next instruction word.
	[pop] / [sp++]  // Pop the source from the stack.
	                   Increase `sp` by 2 for 16 bit operands.

Some instructions use a `[zp]` operand, indicating a 7 bit memory adress `[imm7]`, which allows reaching the first 128 bytes in the address space.
For 16 bit operands, `[2*imm7]` is used instead, which can reach the first 256 bytes in the address space, as aligned 16 bit words.

### Instruction encoding
Each instruction is encoded into one of five major forms `a, A, b, B, M`:

	111111
	5432109876543210
	1eaaamrrdziiiiii    a: 8 bit binop/shift
	01aaamrrdziiiiii    A: 16 bit binop/shift
	001gomrrdziiiiii    m: 8 bit others (including shift)
	0001omrrdziiiiii    M: 16 bit others (including shift)
	0000ccccbbbbbbbb    B: branch

where

- `a/A` are mostly used for 8/16 bit binops
- `m/M` are used for 8/16 bit moves and other things
- `B` is used for branches.

The `mdz` bits are used togther with the major form to choose instruction:

	mdz     100     101         110     111         00x         01x
	        (r, s)  (r, imm6)   (d, r)  (d, r)      (r, zp)	(zp, r)

	a/A     <-------- binop --------->  shift r, s  <---- binop --->
	m0/M0   <-------- mov reg, imm8 ------------->  mul/jumps   swap
	m1/M1   mov     shift       mov     swap        <--- mov ------>

where
- The form of the instruction is decided by the `mdz` value except for `mov reg, imm8`, `shift r, s` (`shift reg, src8`), and `mul/jumps`.
- The `o` bit chooses between the `m0/M0` (`o = 0`) and `m1/M1` (`o = 1`) columns.
- `A` and `M` form instructions are 16 bit while `a` and `m` form instructions are 8 bit except for `jumps` which are all 16 bit.

Most instructions encode one general purpose register in `rr/rre/rrg`, and a source/destination in `iiiiii`.
The interpretation of the `iiiiii` bits depends on the form of the instruction, as does whether it is a source or a destination.

The registers `a - h` are represented with the numbers 0 - 7; `ba - hg` with the numbers 0 - 3.
For `A/M` instructions, `reg` is given by `rr`.
For `a` instructions, `reg` is given by `rre`, while for `m` instructions, it is given by `rrg`.

The `aaa` field in the `a/A` forms sets the binary operation:

	aaa              alternate

	  0   add        neg
	  1   sub        revsub
	  2   adc        negc
	  3   sbc        revsbv
	  4   and        and_not
	  5   or         or_not
	  6   xor        xor_not
	  7   cmp/test   

The alternate form is used in for `binop` instructions with `mdz = 110` and two register operands (use `mdz = 100`) for the regular form.
In the same way, `not` is the alternate form of `mov`, and is used under the same circumstances.
`sub reg, imm6` is replaced by `revsub reg, imm6` (use `add reg, -imm6` for `sub reg, imm6`).
`cmp` is replaced by `test` when `d=1`, but the form is kept as `cmp reg, src`.

The `aaa` field is also used to specify the shift operation in `shift reg, src8`:

	aaa

	  0   ror
	  2   sar
	  4   shr
	  6   shl
	  7   rol

For `shift reg, imm4`, the top two bits of `aaa` are stored in the top two bits of `iiiiii`, while the bottom 4 bits store the shift count.
`rol reg, imm4` must be encoded as

	ror reg, 16 - imm4       // for 16 bit reg
	ror reg, (8 - imm4) & 7  // for  8 bit reg

Multiplication instructions are encoded in the `mul/jump` case with `rr != 0`.
When `rr = 0`, absolute jumps and calls are encoded instead:

	111111
	5432109876543210
	0010000000iiiiii	jump src16
	0010000001iiiiii	call src16
	000100000ziiiiii	jump [zp]

For branches, `bbbbbbbb` is the `imm8` offset, while `cccc` selects the branch condition according to

	cccc   branch condition

	   0   always
	   1   call          // like always, but push address of next instruction before branching

	   4   z / e         // zero / equal
	   5   nz / ne       // not zero / not equal
	   6   s             // signed
	   7   ns            // not signed

	   8   c / ae / nb   // carry / above equal / not below
	   9   nc / nae / b  // not carry / not above equal / below
	  10   a / nbe       // above / not below
	  11   na / be       // not above / below

	  12   v / ge / nl   // signed carry / greater equal / not less
	  13   nv / nge / l  // not signed carry / not greater equal / less
	  14   g / nle       // greater / not less
	  15   ng / le       // not greater / less

When `dest/src` is encoded into `iiiiii` according to

	543210
	1RRrrr   [r16 + r8]          RR != rrr[2:1]
	1RRrr0   [r16++]
	1RRrr1   [--r16]
	01RRii   [r16 + imm2]
	001rrr   r8                  for 8 bit operations
	001rrr   sext(r8)/zext(r8)   for 16 bit operations
	000010   flags               for 8 bit operations
	000RR0   r16                 for 16 bit operations
	000001   imm16
	000011   [imm16]
	000101   [push/pop/sp]       interpretation depends on instruction
	000111   sp

where RR specifies `r16`, `rrr` specifies r8, and `ii` specifies `imm2`.
For the `imm16` and `imm16` cases, the `imm16` value follows the instruction word. For 8 bit instructions, `imm16` still takes 16 bits, but only the lower 8 bits are used by the instruction.

## How it works

	memory    <-> memory  <-> scheduler <-> ALU <-> register
	interface     arbiter         ^                 file
	                 ^            |
	                 |            |
	                 V            V
	            prefetcher <-> decoder

The CPU is divided into a number of parts:

- The memory interface communicates with the external memory (RAM emulator) to send and receive serial messages.
- The memory arbiter
	- arbitrates memory command transmission between the prefetcher and scheduler, and
	- keeps track of outstanding commands and returns incoming responses to the appropriate recepient.
- The prefetcher
	- keeps track of and updates the program counter (PC) register,
	- tries to fetch instruction words a few steps ahead of the currently executing instruction,
	- buffers prefetched instruction words in the prefetch queue, and
	- keeps track of the current instruction and immediate value in the instruction and immediate registers.
- The decoder
	- recieves the current instruction from the prefetcher/instruction register, and
	- decodes it into control signals for the scheduler.
- The scheduler
	- divides instructions into stages,
	- runs the stages needed for each instruction in order, and
	- sets control and data signals as needed during each stage.
- The ALU (arithmetic logic unit)
	- calculates ALU operations (`add/sub/adc/sbc/and/or/xor`...), shifts, and multiplies,
	- breaks calculations down into bit serial steps,
	- breaks down operations on 16 bit register pairs by accessing the appropriate 8 bit register in each cycle,
	- updates flags: carry (`c`), signed carry (`v`), sign (`s`), zero (`v`),
	- reads external inputs and produces an external result when needed, and
	- reads and updates the register file.
- The register file
	- holds most of the CPU registers: general purpose registers `a-h`, `sp`, and
	- mediates access to the flags as a register.

## How to test

TODO

## External hardware

Requires the RP2040 microcontroller on the Tiny Tapeout 07 demo board or similar to serve as RAM emulator, adhering to the memory interface protocol described above.
