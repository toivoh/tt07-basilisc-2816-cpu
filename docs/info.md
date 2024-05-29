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

and a later version is intended to be included in the next version of the AnemoneGrafx-8 retro console https://github.com/toivoh/tt06-retro-console (motivating the constraints).

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
- Variable shift instructions
- 8x8 and 8x16 bit multiply instructions, producing 2 result bits per cycle like everything else
- 13 branch conditions + relative call
- Instruction prefetch queue

Contents:

- Interface / pins
- Programmer's view
- Execution timing
- How it works

## Interface / pins

The TX/RX interface is used to send commands to the RAM/RAM emulator, and to receive read data.
It uses start bits to allow each side to initiate a message when appropriate; subsequent bits are sent on subsequent clock cycles.
The `tx_out` and `rx_in` pins must remain low when no messages are sent.

The `tx_out[1:0]` pins are used for messages from the CPU:

- a message is initiated with one cycle of `tx_out[1:0] = 1` (low bit set, high bit clear),
- during the next cycle, `tx_out[1:0]` contains the 2 bit _TX header_, specifying the message type,
- during the following 8 cycles, a 16 bit payload is sent through `tx_out[1:0]`, from lowest bits to highest.

The `rx_in[1:0]` pins are used for messages to the console:

- a message is initiated with one cycle when `rx_in[1:0] != 0`, specifying the _RX header_, i e, the message type,
- the value of `rx_in` during the next cycle is ignored,
- during the following 8 cycles, a 16 bit payload is sent through `rx_in[1:0]`, from lowest bits to highest.

The RX message has been prolonged to the same length as the TX message so that the CPU can respond to an incoming RX message with an outgoing TX message without any delay.

TX message types:

- 0: Read 16 bit data. Payload is the byte address (can be uneven).
- 2: Write 8 bit data: Write bottom 8 bits of payload to last read address.
- 3: Write 16 bit data: Write payload to last read address.

There is only one RX message types: 1: Read response. Payload is 16 bit data.

Two additional output pins give additional information about the current TX message:

- `tx_fetch` is high when the current TX message is a read of an instruction word,
- `tx_jump` is high when `tx_fetch` is high and the current fetch is for the destination of a jump.

The RAM emulator does not need to use these pin values to operate correctly, but they give additional information about what the CPU is doing, and are used by the gate level test.

## Programmer's view
### Registers

The CPU has the following registers:
- 8 general purpose registers 8 bit registers `a - h`,
	- which can be grouped into four general purpose 16 bit register pairs `ba / dc / fe / hg`.
- 16 bit program counter `pc`, keeping the adress of the current instruction.
- Stack pointer register pair `sp`:
	- bottom half `p` is an 8 bit register,
	- top half `s` always reads as 1, making all stack operations work on the address range `0x100 - 0x1ff` (actually, `0xfe - 0x1ff`).
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

The following forms are supported:

	binop reg, src
	binop dest, reg
	binop reg, imm6  // imm6 is signed
	binop reg, [zp]
	binop [zp], reg

There some additional binary operations

	revsub dest, src  // dest = src - dest
	revsbc dest, src  // dest = src - dest + carry - 1
	cmp    dest, src  // Update flags according to dest - src,
                         don't update dest
	test   dest, src  // Update flags according to dest & src
                         don't update dest

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
	call          // like always, but push address
	                 of next instruction before branching

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

	reg               // 8/16 bit general purpose register,
	                     depending on size of the instruction.
	sp/p              // Stack pointer.
	                     Read/write LSB p for 8 bit instructions.
	flags             // Only for 8 bit operations. Not for shift/mul.
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
	[sp]              // Only for operations that depend
	                     on the value of dest.

For instructions that use a `src` operand, `src` can be anything that `dest` can be except `[pop], [sp]`, and can additionally be

	sext(r8)        // Sign extend r8, only for 16 bit operations.
	zext(r8)        // Zero extend r8, only for 16 bit operations,
	                   not for cmp and test.
	imm16           // imm16 follows in the next instruction word.
	[pop] / [sp++]  // Pop the source from the stack.
	                   Increase `sp` by 2 for 16 bit operands.

The `[push]` and `[pop]` operands do not play well when `sp` wraps around.
It is recommended to initialize `sp` to `0x1ff` or `0x1fe` when the stack is empty (by writing `0xff` or `0xfe` to the `p` register).
The `[push]` operand will write a byte to `[sp - 1]` or two bytes to `[sp - 2], [sp - 1]`, which can reach as low as `0xfe`. 8-bit wraparound will only affect the updated value of `sp` after the `[push]` operation.

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
	   1   call          // like always, but push address
	                        of next instruction before branching

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

When `dest/src` is used in an instruction, it is encoded into `iiiiii` according to

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
The `d` bit chooses between `sext(r8)` (`d=0`) and `zext(r8)` (`d=1`), except that `test` uses `sext(r8)` even though it is encoded with `d=1`.
For the `imm16` and `imm16` cases, the `imm16` value follows the instruction word. For 8 bit instructions, `imm16` still takes 16 bits, but only the lower 8 bits are used by the instruction.

### Execution timing

Instruction timing is influenced by 3 factors:

- the execution cycles needed for a given instruction,
- cycles needed to wait for access to send memory read/write commands, and to wait for memory read data,
- availability of new instructions to execute from the prefetch queue, which is flushed by jumps.

See the Inferface / Pins section for details if the memory interface.
The memory interface is shared between

- the _prefetcher_, which tries to fetch new instruction words a few steps ahead of the current `pc`, and
- the _scheduler_, which executes instructions, including any memore accesses that they need.

The scheduler has priority if both want to initiate a new TX message and the TX channel is idle.

#### Instruction stages

The scheduler divides each instruction into 1-4 stages:

	                               ror1 stage -> rotate stage
	address stage -> data stage -> 
	                               mul1 stage -> mul2 stage

where

- the _address stage_ is used by instructions that need to send a memory address to the RAM,
- the _data stage_ is the main stage and is used by most instructions to calculate and store their result,
- the _ror1_ and _rotate stages_ are used by `shift` instructions, and
- the _mul1_ and _mul2_ stages are used by `mul` instructions.

Stages that are not needed for the current instruction are skipped, and when the last stage that is needed is done, the instruction finishes.
No extra cycles are needed to skip the address stage or to finish an instruction, but in all other cases it takes one cycle to skip an unused stage.

The address stage always sends a read message on the TX channel, and takes 10 cycles starting from when the scheduler gains access to the TX channel.

The data stage takes a different number of cycles depending on the instruction:

- It needs 4 cycles to calculate/store an 8 bit results and 8 cycles for a 16 bit result.
- If the instruction reads memory, the data stage needs to wait for the data before it can start.
	- It does not need to wait for the read message to finish (when reading 16 bits but only needing 8).
- If the instruction writes memory, the data stage needs 10 cycles to send a write message on the TX channel.
- If the instruction is read-modify-write, the write data message will be initiated in the same cycle that the start bit for the read response message corresponding to the read sent during the address stage is received.
- If the data stage reads or writes the `pc` register, (calls read to push the address of the next instruction, jumps write)
  it needs to wait for the prefetcher to be idle first. The prefetcher is blocked while reading from `pc`, while writing to `pc` is combined with sending the first read message to start prefetching from the jump destination.

Read-modify-write instructions send a read command on the TX channel, and reply with a write command when the read response is received, updating the data at the address just read. Since they need to be able to respond with a TX message as soon as an RX message comes in, and because the destination address for the write is given by the last read address, read-modify-write instructions block the prefetcher from iniating reads while they wait for a read response.

`mov` and `swap` instructions are not considered read-modify-write. They send a read message to specify the write address, immediately followed by a write message to write the data. `swap` instructions need to wait for the read response before they can finish, but the prefetcher is allowed read access during the wait.

`shift` and `mul` instructions with immediate second operand skip the address and data stages completely.
Other `shift` and `mul` use the data stage (and address stage if needed) to load the shift count / factor from the second operand, which is always treated as 8 bit for the timing.

The ror1 stage is used by most shifts. It rotates the destination right by a single bit if needed, inserts zeros for `shl`, and finds the sign bit for `sar`.
It takes 4/8 cycles for 8/16 bit shifts, but is skipped if

- the shift count is zero, or (the shift instruction is finished immediately from the ror1 stage)
- the shift count is even, except for `sar` and `shl`.

The rotate stage is used by all shifts except when the shift count is zero. It rotates the destination right in steps of 2, taking one cycle for each such step needed to produce the result. It also feeds in zero/sign bits for `shr` and `sar`.

The mul1 stage is used by all `mul` instructions. It calculates the part of the result that is stored into the first operand, and takes 4/8 cycles for 8/16 bit operands.
The mul2 stage is used to write the 8 top bits of the multiplication result to the `h` register. It takes 4 cycles, unless the `h` register overlaps with the destination, in which case the instruction is finished after the mul1 stage.

#### The prefetcher

The prefetcher has a prefetch queue of 2 - 3 instruction words (2 for the https://github.com/toivoh/tt07-basilisc-2816-cpu configuration, which supports the `mul` instruction, and 3 for the https://github.com/toivoh/tt07-basilisc-2816-cpu-OL2 configuration, which doesn't).

When the current instruction is finished, the decoder tries to load the next instruction from the head of the prefetch queue to start executing it.
Instructions that use an `imm16` operand wait for and load an additional instruction word from the prefetch queue before they can start. This takes at least one cycle.

As long as the prefetch queue is not full, the prefetcher tries to send new read messages to fill it.
A read enters the prefetch queue as soon as the read message is sent.
When the read response arrives, the instruction data is stored in the 2 - 3 read buffers (same number as the size of the prefetch queue) waiting to be consumed by the decoder.
An important motivation for the prefetch queue is that it might take some time between a read message is sent and a read response is received.

#### Jumps

Jumps have special effects on the prefetcher.
Whenever a jump is taken, the current contents of the prefetch queue are flushed, meaning that they will not be used (including read responses to read messages that have already been sent, but where the response has not arrived yet).

Jumps update the `pc` register during the data stage. At the same time, they send the first read meassage to prefetch from the jump destination.

Calls are special in that they are scheduled like two instructions in sequence: `push pc+2 / push pc+4` followed by the corresponding `jump` instruction.

#### Performance considerations

Performance characteristics of different types of instructions:

- Instruction that operate only on registers and immediates are generally fast, don't need to wait for memory access, and allow the prefetcher to work in parallel.
	- `mov/swap/binop` with registers (including `sp/p/flags/sext(r8)/zext(r8)` and immediates generally take 4/8 cycles for 8/16 bit operations.
	- `shift` and `mul` instructions usually take more cycles (see above), but still allow the prefetcher to work, especially when the shift count/factor does not need to be read from memory.
		- `shift` and `mul` instructions with immediate second operand are faster since they don't need to spend time on loading the shift count/factor.
		- For `shift` instructions, the ror1 stage can be skipped under certain circumstances, and the length of the rotate stage depends on the shift count: small right shifts / big left shifts are faster (see above).
	- Consider that `imm16` operands require an extra instruction word to be prefetched (and require one additional cycle to load).
	  They still allow the data to be prefetched, unlike with reads initiated by the instruction.
- `mov/binop reg, mem` instructions need to send a read message and wait for the response, but allow the prefetcher to work while doing so. Likely, the prefetcher has a chance to get ahead while waiting.
- `mov mem, reg` instructions need to send a read message and a write message, but don't need to wait for response.
- `swap mem, reg` instructions need to send a read message and a write message, and wait for the response, but allow the prefetcher to run while waiting.
- Read-modify-write instructions `binop mem, reg` need to send a read message and a write message, and block the prefetcher while waiting for the response.
- (Taken) jumps flush the prefetch queue, causing execution to stall until new instructions can arrive at the decoder.

The things to be most careful with performance wise are probably read-modify-write instructions and jumps. On the other hand, read-modify-write can help relieve register pressure, and jumps are often needed and sometimes save more time than they cost.

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
	- calculates ALU operations (binary operations `add/sub/adc/sbc/and/or/xor`...), shifts, and multiplies,
	- breaks down operations on 16 bit register pairs by accessing the appropriate 8 bit register in each cycle,
	- breaks calculations down into bit serial steps,
	- updates `flags` based on the results of a computation,
	- reads external inputs and produces an external result when needed, and
	- reads and updates the register file.
- The register file
	- holds most of the CPUs registers: general purpose registers `a-h` and `sp`, and
	- mediates access to the flags as a the `flags` register.

### 2-Bit serial operation
The CPU uses 2-bit-serial operation wherever it can. This means that instead of operating on all bits of a value in parallel, it operates on 2 bits per cycle.
Since the memory interface can only send/reveive 2 bits/cycle, there is little point in making anything else operate faster. Serial operation saves area compared to parallel operation for both computions and registers.

An important example is addition. Consider adding two values that are stored separate registers. Addition starts from the least significant two bits (lsbs), and proceeds upwards, two bits per cycle. To facilitate this, the registers are organized as shift registers, which can be right-shifted two bits per cycle. At the same time, two new bits are fed into the top.

The two lsbs from each register can have a value between 0 and 3, and the sum has a value between 0 and 6. With a carry bit coming in, the sum can be between 
 and 7. The bottom 2 bits of the sum is used as the result bits at the current 2-bit position, while the top bit is used as carry into the next 2-bit position. A flip-flop is needed to store the carry from one cycle to the next. At each step, the registers are shifted right by two steps so that the 2 bits to be operated on are always at the bottom of each register.

The addition can be performed in place, replacing the value in one of the registers with the result: The result bits are just shifted into the top of the register. If a register should keep its value after the operation, the lsb bits shifted out are shifted back into the top at the same time.
When the register contents have been shifted around to the beginning, the addition is completed.

### Memory arbiter
The memory arbiter keeps track of which block is currently using the TX channel, if any.
If both prefetcher and scheduler try to start a transaction at the same time, the scheduler gets priority. The scheduler has been designed under the assumption that it will get priority whenever it raises the `reserve_tx` signal, which also blocks the prefetcher from sending any new messages.
This is needed for read-modify-write instructions, since the write address is specified in the read message from the scheduler that precedes its write message.

The memory arbiter also has a FIFO to keep track of outstading transactions (reads), which have been started but have not yet finished.
The FIFO keeps track of whether the read response should be handled by the prefetcher or the scheduler, or if it should be ignored completely.
The ignore feature is used for write instructions (`mov mem, reg`, `push pc+2/4` during `call`s), where the read message is just used to set the write address. This allows the scheduler to move on without waiting for a read response.
The scheduler is designed to have only one outstanding (non-ignored) read, so that when it receives a read response, there is no doubt about what the data is for.

### Prefetcher

The prefetcher holds the 16 bit program counter register `pc`, and updates it using 2-bit serial operations, which can add a delta or set a new value.
The least significant bit of `pc` is always expected to be zero, but the storage space is still needed since the 2-bit serial addition causes the `pc` value to rotate around through the `pc` bits.

Except when jumping, the `pc` register is incremented by 2 for each prefetch.
When the prefetcher sends a read message on the TX channel, it scans through the `pc` register and increments it at the same time, sending the incremented `pc` value as the read address.

The prefetcher also keeps track of the number of prefetched and flushed instruction words.
The `pc` register does not store the address of the currently executing instruction, instead it stores the address of the last instruction that was prefetched `curr_inst_addr + 2*num_prefetched`.

The prefetcher also holds the instruction and immediate registers. When loading a new value into the instruction register, the same value is loaded into the immediate register as well. For instructions that use an `imm16` or `[imm16]` argument, the immediate register is loaded with the next value from the prefetch queue.

The instruction register is used as input to the decoder. The immediate register is used to feed the ALU with an immediate value, typically shifting out 2 bits per cycle. This works since the immediate field is always placed in the least significant bits of an instruction.
The bottom half of the immediate register is also used to store the shift count of `shift` instructions and 2nd operand of `mul` instructions. `shift/mul` instructions that don't have these stored as immediates shift the needed data into the immediate register during the data stage.
The top half of the immediate register is also used as scratch space by the `mul` instruction.

When reading out the `pc` (for branches and calls), the adder that is normally used to increment it is used to subtract `2*num_prefetched` to compute the address of the current instruction. The prefetcher must be blocked from starting new prefetch transactions to allow reading out the `pc`, but in the case of branches, the `pc` is read out, adjusted, updated, and used to send a prefetch transaction for the jump destination, all at the same time:
The adjusted `pc` value is sent to the ALU, which adds the offset from the branch, and sends the branch target `pc` value to be sent as prefetch address and stored in the `pc` register. Jumps are similar to branches, but don't need to read the current value of the `pc`, just set it to the value output from the ALU.

The reason that the prefetcher outputs the incremented `pc` value as read address is so that the same target address can be stored into the `pc` register and output in a prefetch transaction when jumping.

When jumping, the prefetcher also sets `num_prefetched = 1`, to reflect that the jump is started with a single prefetch.
Before that, `num_flushed` is set to `num_prefetched`, to flush all instruction words prefetched after the the jump instruction that was executed.
As long as `num_flushed` is nonzero, instruction words that arrive at the output of the prefetch queue are discarded immediately, instead of feeding the instruction or immediate registers, or waiting to be consumed.

### Decoder

The decoder decodes instruction words according to instruction, instruction form, and addressing mode, and instructs the scheduler which operations need to be performed: which operation, sources, and destination to use for the data stage, which operation and sources to use for the address stage (if any source or destination in the data stage needs one), whether to update flags, activate special features like swapping, condition codes, shifting, multiplication, etc.

The decoder also schedules a `push pc + 2` or `push pc + 4` before executing a `call` (including `branch call`) instruction, which is otherwise executed just like a `jump/branch`, calculating the size of the instruction already at the push stage.

### Register file

The register file is indexed using 4 bit register indices.
The first 8 indices are connected to the general register file, while additional indices are used for `s, p`, and `flags`.
The general register file is implemented using its own module; the additional registers are implemented with some extra glue logic and storage.

The general register file has two read/write ports, with 3 bit register indices, separate enable signals, inputs and outputs.
It is implemented as 8 addressable 8 bit shift registers. Registers that are addressed through a read/write port that is enabled are shifted, shifting in the input value for the appropriate port. The input from port 1 has priority.

If a value should be read out without being modified, the user of the register file needs to recirculate the bits that were read out from the port back to its input.
Each port is generally expected to be enabled 4 times in sequence with the same register index, so that values can circulate back to their original bit positions. The exception is shift operations, which vary the number of enable cycles to achieve different bit shifts.

### Addressing modes

When sending a read/write address using a read message on the TX channel, the address bits need to be retrieved/computed two bits at a time.
The ALU and register file have nothing else to do at this time, and are used to calculate the address.
Either the ALU's second argument is used as the result (just like in a `mov` instruction, which ignores the destination's value), or an addition is used, sometimes exploiting the ALU's feature to multiply the second argument by two. (Used only for address computations, branches, and calls.)
The ALU can also sign/zero extend the second argument to different lengths, which is used by various addressing modes.

Postincrement/predecrement addressing modes (including `[pop]` and `[push]`) write the result of the computation back to the ALU's first register argument, just like instructions that write to registers. For the postincrement case, the ALU result is written to the register, while the value read out from it is used as the address.

There are more options that make sense for `src` operands than `dest` operands.
This fact is used to differentiate between the `sext(r8)` and `zext(r8)` source forms; `zext(r8)` is `sext(r8)` with the `d` bit set to 1, which would normally mark it as a destination. (Except for the `test` instruction, which also uses the `d` bit to distinguish it from the `cmp` instruction, but retains `sext(r8)`.)

### `swap` instructions

`swap reg1, reg2` instructions work like `mov reg1, reg2` instructions, but feed the second register file port's input with the output from the first.

`swap mem, reg` instructions use a special trick to be able to write out the data immediately and shift in the new data later.
First, for 16 bit swap instructions, the register indices are kept static and the register file ports are connected to the top and bottom of the target register, forming a 16 bit shift register (instead of swapping between upper and lower halves as the ALU usually does in a 16 bit operation).

The write transaction is started as soon as possible after sending the read, and the ALU is allowed to shift out bits from the register file to feed as write payload, but the ALU does not start counting cycles to finish the operation. The bits shifted into the target register at this point may be garbage.
When the read response payload arrives, the ALU starts counting cycles. The payload bits are shifted into the target register and the instruction is finished.

This setup allows the register file to take care of read response data without sending a write message in response, which means that the scheduler doesn't have to block the prefetcher while waiting for the read response data.

### `shift` instructions

`shift` instructions rely on the register file's ability to shift/rotate values in place, rotating right 2 bits per cycle.
For this reason, all shift operations operate in place on register values.
Some complications arise since
- the only supported shift step is 2 (how to handle odd shift counts?),
- the only supported shift direction is right shift,
- the sign bit (top bit) of the registers in the register file is not readily available, but needed by `sar`.

For the 16 bit case, both register file ports are connected together to form a 16 bit shift register.

To work witin the restrictions:
- Odd shift amounts are handled by the ror1 stage, which needs 5/9 cycles to rotate an 8/16 bit value right one step by rotating it right 5/9 cycles in steps of 2 bits, through a 1 bit delay.
- `sar` records the sign bit at the end of the ror1 stage.
- Left shifts and rotates are implemented using right rotation with `8 - shift_count` or `16 - shift_count` number of steps.

Also:
- `shr` and `sar` replace the topmost bits with zeros/the sign during the final rotate stage (and the final ror1 cycle, for odd shifts).
- `shl` replaces bits with zeros during the `ror1` stage - the bit positions to be zeroed are exactly those that will not be rotated the lsb during the final rotate stage.
- The shift count for `rol` and `shl` is negated while loading it into the immediate register during the data stage - `rol reg, imm4` must be encoded as `ror reg, -imm4`.

### `mul` instructions

`mul` instructions use the immediate register for two purposes:

- The bottom half, which we will call the `factor` register in this context, holds an 8 bit unsigned factor.
- The top half, which we will call the `partial` register in this context, holds a partial sum.

For `mul reg, imm6` instructions, the `imm6` factor is loaded into the `factor` register when loading the instruction.
For `mul reg, src` instructions, the data stage loads the `factor` register, much like `shift reg, src` instructions load the same register with the shift count.

A `mul` instruction multiplies an 8/16 bit destination register with the 8 bit `factor` register, producing a 16/24 bit result.
Every cycle, it consumes 2 bits from the original value of the destination register and produces two bits of the result.

Consider multiplying an 8 bit factor `dcba` with an 8 bit value `hgfe`, where each letter is two bits.
The needed computation that is

	 dcba    * h
	  dcba   * g
	   dcba  * f
	+   dcba * e
	--------

The basic algorithm starts by forming `dcba * e`, the first term in the sum, our current partial sum. This is the only term that affects the bottom 2 bits of the result, which can be output as the first 2 result bits. The remaining bits are shifted 2 steps right to align with the next level.
In the second step `dcba * f` is added to the partial sum from the last step, producing a new partial sum, and everything proceeds as before.

The basic algorithm still requires the multiplication of an 8 bit factor with a 2 bit number every cycle.
Multiplication by 0 and 1 are easy, and 2 is just a left shift, but multiplication by 3 would require an extra adder.
Instead, the multiplier multiplies by -1. This produces the same result for the lowest 2 bits. At the same time, it sets a carry to add one to the incoming 2 bit value during the next cycle. This results in the need to multiply by a number between 0 and 4, but 4 can be handled much like 3: multiply by zero, set a carry for the next step.

In total, the multiplier requires
- 9 bits of scratch space for the partial sum (the `partial` register and a sign bit, reusing the ALUs temporary sign bit (not the sign flag)),
- an 11 bit adder,
- a one step shifter, and provisions to multiply the factor by zero or invert its bits.

## How to test

TODO

## External hardware

Requires the RP2040 microcontroller on the Tiny Tapeout 07 demo board or similar to serve as RAM emulator, adhering to the memory interface protocol described above.
