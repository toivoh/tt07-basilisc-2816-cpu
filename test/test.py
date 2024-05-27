# SPDX-FileCopyrightText: © 2024 Toivo Henningsson
# SPDX-License-Identifier: MIT

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer, ClockCycles
from random import randrange, getrandbits, choice

from ram_emulator import *
from isa import *


#@cocotb.test()
async def test_cpu0(dut):
	dut._log.info("start")
	clock = Clock(dut.clk, 2, units="us")
	cocotb.start_soon(clock.start())

	preserved = True
	top = dut.top
	cpu = top.cpu
	try:
		decoder = cpu.dec
	except AttributeError:
		preserved = False

	if preserved:
		sched = decoder.sched
		alu = sched.alu
		regfile = alu.registers
		regs = regfile.general_registers.regs

		#for i in range(NREGS): regs[i].value = i**2

	# reset
	dut._log.info("reset")
	dut.rst_n.value = 0
	dut.ui_in.value = 0
	await ClockCycles(dut.clk, 10)
	dut.rst_n.value = 1
	await ClockCycles(dut.clk, 1) # Seems to be needed to start the RAM emulator at the first start bit

	if preserved:
		mem = [0 for i in range(32768)]
		pc = 0xfffc >> 1

		def encode(inst):
			nonlocal pc
			encoded = inst.encode()
			print(encoded)
			for w in encoded:
				mem[pc] = w
				pc = (pc + 1) & 0x7fff

		if False:
			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0x1233)))
			encode(Binop(BinopNum.CMP, ArgReg(False, 0), ArgReg(False, 1)))
			encode(Binop(BinopNum.MOV, ArgReg(False, 2), ArgRegFlags()))
			encode(Binop(BinopNum.MOV, ArgRegFlags(), ArgReg(False, 0)))

		if False:
			encode(Binop(BinopNum.MOV, ArgReg(True, 4), ArgImm16(True, 0x0706)))
			encode(Binop(BinopNum.MOV, ArgReg(False, 1), ArgImm8(False, 0x06)))
			encode(Mul(ArgReg(True, 4), ArgReg(False, 1)))
			#encode(Mul(ArgReg(False, 5), ArgReg(False, 0)))

		if False:
			rd = 4
			encode(Binop(BinopNum.MOV, ArgReg(True, rd&6), ArgImm16(True, 0x7060)))
			encode(Mul(ArgReg(True, rd), ArgImm6(False, 0x1f)))
			#encode(Mul(ArgReg(False, rd), ArgImm6(False, 0x1f)))

			#encode(Binop(BinopNum.MOV, ArgReg(True, rd), ArgImm16(True, 0x7775)))
			#encode(Mul(ArgReg(True, rd), ArgImm6(False, 0x24 - 64)))
		if False:
			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0x1234)))

			encode(Binop(BinopNum.MOV, ArgRegSP(False), ArgReg(False, 1)))
			encode(Binop(BinopNum.MOV, ArgReg(False, 3), ArgRegSP(False)))
			encode(Binop(BinopNum.MOV, ArgReg(True, 4), ArgRegSP(True)))

		if False:
			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0x1234)))

			encode(Binop(BinopNum.MOV, ArgRegSP(False), ArgReg(False, 0)))
			encode(Binop(BinopNum.MOV, ArgMemPushPopTop(False, 1), ArgReg(False, 1)))
			encode(Binop(BinopNum.MOV, ArgReg(False, 3), ArgMemPushPopTop(False, -1)))

		if False:
			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0x1234)))

			#encode(Binop(BinopNum.MOV, ArgReg(True, 2), ArgImm16(True, 0x3456-2)))
			#mem[0x3456 >> 1] = 0xabcd
			#encode(Swap(ArgReg(True, 0), ArgMemR16PlusImm2(True, 2, 2)))

			encode(Swap(ArgReg(True, 0), ArgMemZP(True, 250)))
			mem[250>>1] = 0xabcd

			#encode(Swap(ArgReg(False, 0), ArgReg(False, 1)))

		if False:
			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0x8a5f)))
			encode(Shift(ShiftopNum.SAR, ArgReg(True, 0), ArgImm6(False, 5)))

			#encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0xe469)))
			#encode(Shift(ShiftopNum.SAR, ArgReg(True, 0), ArgImm6(False, 2)))

		if False:
			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0x0a5f)))
			#encode(Shift(ShiftopNum.ROR, ArgReg(True, 0), ArgImm6(False, 1)))
			encode(Shift(ShiftopNum.SHR, ArgReg(True, 0), ArgImm6(False, 5)))

		if False:
			#encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0x1234)))
			#encode(Shift(ShiftopNum.ROR, ArgReg(True, 0), ArgImm6(False, 12)))
			#encode(Shift(ShiftopNum.SHR, ArgReg(True, 0), ArgImm6(False, 4)))
			#encode(Shift(ShiftopNum.SHL, ArgReg(True, 0), ArgImm6(False, 4)))

			#encode(Binop(BinopNum.MOV, ArgReg(False, 2), ArgImm8(False, 12)))
			#encode(Shift(ShiftopNum.ROR, ArgReg(True, 0), ArgReg(False, 2)))
			#encode(Shift(ShiftopNum.ROL, ArgReg(True, 0), ArgReg(False, 2)))

			#encode(Binop(BinopNum.MOV, ArgReg(False, 2), ArgImm8(False, 4)))
			#encode(Shift(ShiftopNum.SHL, ArgReg(True, 0), ArgReg(False, 2)))
			#encode(Shift(ShiftopNum.SHL, ArgReg(False, 0), ArgReg(False, 2)))

			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm8(True, -1)))
			encode(Shift(ShiftopNum.SHL, ArgReg(True, 0), ArgImm6(False, 5)))

		if False:
			#encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm16(True, 0xe4a5)))
			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgMemImm16(True, 0xbcde)))
			mem[0xbcde>>1] = 0xe4a5
		if False:
			#encode(Binop(BinopNum.MOV, ArgReg(True, 6), ArgImm8(True, -128))) # set sp old style

			encode(Binop(BinopNum.MOV, ArgReg(False, 0), ArgImm8(True, 0xff)))
			encode(Binop(BinopNum.MOV, ArgRegSP(False), ArgReg(False, 0))) # set up sp

			target_pc = pc + 8
			#encode(Jump(ArgImm16(True, 2*target_pc)))
			encode(Jump(ArgImm16(True, 2*target_pc), call=True))
			while pc < target_pc:
				mem[pc] = 2048+pc; pc += 1
			print("target_pc = ", target_pc)
			print("pc = ", pc)

		if False:
			encode(Binop(BinopNum.MOV, ArgReg(False, 0), ArgImm8(False, 0xff)))
			encode(Binop(BinopNum.MOV, ArgRegSP(False), ArgReg(False, 0))) # set up sp

			target_pc = pc + 5
			encode(Jump(ArgImm16(True, 2*target_pc))) # jump over function

			# function
			func_pc = pc
			encode(Binop(BinopNum.MOV, ArgReg(False, 0), ArgImm8(False, 0xab))) # executed
			encode(Jump(ArgMemPushPopTop(True))) # ret
			encode(Binop(BinopNum.MOV, ArgReg(False, 0), ArgImm8(False, 0xcd))) # not executed

			while pc < target_pc:
				mem[pc] = 0xf00+pc; pc += 1

			#encode(Jump(ArgImm16(True, 2*func_pc), call=True)) # call function
			encode(Branch(func_pc - pc, cc=CCNum.CALL.value)) # call function
			print("target_pc = ", target_pc)
			print("func_pc = ", func_pc)
			print("pc = ", pc)


		# Sequence of instructions to make a prefetch be ongoing when the PC read starts
		encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm8(True, 0)))
		encode(Binop(BinopNum.SUB, ArgMemR16PlusImm2(True, 0, 0), ArgReg(True, 0)))
		encode(Binop(BinopNum.MOV, ArgReg(False, 0), ArgReg(False, 0)))
		# PC read
		#encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgPCPlusImm8(True, -128)))
		# Branch
		encode(Branch(4+1, cc=dut.CC_NZ.value))
		for i in range(4): mem[pc] = 256+i; pc += 1
		# Untaken branch
		encode(Branch(4+1, cc=dut.CC_Z.value))

		if True:
			# Indirect jump: jump pc, sext(r8)
			target_pc = pc + 8
			encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm8(True, 2*target_pc)))
			encode(Jump(ArgSextReg(True, 0)))
			while pc < target_pc:
				mem[pc] = 512+pc; pc += 1
		if False:
			# Indirect jump: jump pc, [zp]
			target_pc = pc + 8
			encode(Jump(ArgMemZP(True, 2*(pc+1))))
			mem[pc] = 2*target_pc; pc += 1
			while pc < target_pc:
				mem[pc] = 768+pc; pc += 1
		print("target_pc = ", target_pc)
		print("pc = ", pc)


		for i in range(4):
			encode(Binop(BinopNum.MOV, ArgReg(True, 2*i), ArgImm8(True, ~i)))
		encode(Binop(BinopNum.MOV, ArgReg(False, 3), ArgImm8(False, 0xfd - 256)))
		encode(Binop(BinopNum.MOV, ArgReg(True, 0), ArgMemR16PlusImm2(True, 2, 0)))
		mem[0xfdfe>>1] = 0xa5e4
		#encode(Binop(BinopNum.MOV, ArgMemR16PlusImm2(True, 2, 6), ArgReg(True, 2))
		encode(Binop(BinopNum.ADD, ArgMemR16PlusImm2(True, 2, 6), ArgReg(True, 2)))
		mem[0xfe04>>1] = 0x100 - 0x002
		encode(Binop(BinopNum.MOV, ArgMemR16PlusImm2(True, 2, 4), ArgReg(True, 2)))
		for i in range(2, 8):
			encode(Binop(BinopNum.MOV, ArgReg(False, i), ArgImm8(False, i**2)))
		#for i in range(pc): print("mem[", i ,"] = ", hex(mem[i]))

		ram_emu = RAMEmulator(mem, delay=13)


		for i in range(8*pc + 14*16):
			#tx, tx_fetch = dut.tx_pins.value.integer, dut.tx_fetch.value.integer
			uo_out = dut.uo_out.value.integer
			tx = uo_out & 3
			tx_fetch = (uo_out >> 2) & 1

			#print("tx = ", tx)
			rx = ram_emu.step(tx)
			#dut.rx_pins.value = rx
			dut.ui_in.value = rx

			await ClockCycles(dut.clk, 1)

	else:
		await ClockCycles(dut.clk, 1)


@cocotb.test()
async def test_cpu(dut):
	dut._log.info("start")
	clock = Clock(dut.clk, 2, units="us")
	cocotb.start_soon(clock.start())

	preserved = True
	try:
		top = dut.top
		cpu = top.cpu
		decoder = cpu.dec
	except AttributeError:
		preserved = False

#	if preserved:
#		sched = decoder.sched
#		alu = sched.alu
#		regfile = alu.registers
#		regs = regfile.regs

		#for i in range(NREGS): regs[i].value = i**2

	mul_supported = dut.MUL_SUPPORTED.value != 0

	# reset
	dut._log.info("reset")
	dut.rst_n.value = 0
	dut.ui_in.value = 0
	await ClockCycles(dut.clk, 10)
	dut.rst_n.value = 1

	state = State()
	for i in range(0, NREGS, 2): state.set_reg(i, randrange(0x10000), True)

	pos = 0
	instructions = []
	pcs = []
	jump_flags = []
	is_opcode = []
	last_addr = None
	num_flushed = 0

	def exec(inst):
		is_opcode.append(True)
		pcs.append(state.pc)
		jump_flags.append(state.jumped)
		inst.execute(state)
		encoded = inst.encode()
		instructions.extend(encoded)
		for i in range(1, len(encoded)):
			pcs.append((pcs[-1] + 2) & 0xffff)
			jump_flags.append(False)
			is_opcode.append(False)

	def get_next_inst(header, addr, jumped):
		nonlocal pos, last_addr, num_flushed
		assert header == TX_HEADER_READ_16 # Check that the fetch is a 16 bit read

		#print((pos, addr, pcs[pos], hex(instructions[pos])))
		#print("pos = ", pos, "addr = ", addr, "pcs[pos] = ", pcs[pos], "jumped = ", jumped, "jump_flags[pos] = ", jump_flags[pos], "is_opcode[pos] = ", is_opcode[pos])

		want_jump = jump_flags[pos]
		assert (not jumped) or want_jump

		#if addr != pcs[pos]:
		if want_jump and not jumped:
			# Prefetching instruction data that should be discarded before it gets to be executed
			last_addr = (last_addr + 2) & 0xffff
			assert addr == last_addr
			num_flushed += 1
			assert num_flushed < 4 # Depends on prefetch queue size
			return (RX_SB_READ_16, 0)

		num_flushed = 0

		assert addr == pcs[pos] # Check that we get the expected address.
		inst = instructions[pos]
		#pos += 1
		if pos < len(instructions): pos += 1
		last_addr = addr
		return (RX_SB_READ_16, inst)

	ram_emu = MockRAMEmulator(delay=13, alt_responder=get_next_inst)
	state.ram_emu = ram_emu

	min_jump = 1

	#for i in range(8):
	#	exec(Binop(BinopNum.MOV, ArgReg(False, i), ArgImm8(False, state.get_reg(i))))
	#exec(Binop(BinopNum.SUB, ArgMemR16PlusR8(False, 2, 7), ArgReg(False, 5))) # Test instruction

	# Intialize all registers to avoid problems with X
	for i in range(0, 8, 2): exec(Binop(BinopNum.MOV, ArgReg(True, i), ArgImm8(True, 0)))
	exec(Binop(BinopNum.MOV, ArgRegSP(False), ArgReg(False, 0)))

	# Test every short call instruction
	for offset in range(-128, 128):
		if offset == 0: continue
		exec(Branch(offset, cc=CCNum.CALL.value))
		for i in range(0, 2):
			exec(Binop(BinopNum.MOV, ArgMemR16PlusImm2(True, 4*i, 0), ArgReg(True, 4*i+2)))

	# Test ret
	exec(Jump(ArgMemPushPopTop(True)))

	# Try all even shl shift steps
	for wide in [False, True]:
		for use_immediate in [False, True]:
			for shift_step in range(15 if wide else 7):
				exec(Binop(BinopNum.MOV, ArgReg(True, 0), ArgImm8(True, -1)))
				if use_immediate:
					exec(Shift(ShiftopNum.SHL, ArgReg(wide, 0), ArgImm6(False, shift_step)))
				else:
					exec(Binop(BinopNum.MOV, ArgReg(False, 2), ArgImm8(False, shift_step)))
					exec(Shift(ShiftopNum.SHL, ArgReg(wide, 0), ArgReg(False, 2)))

				# Read out the state so that the MockRAMEmulator will check it; just read the first four registers
				for i in range(1):
					exec(Binop(BinopNum.MOV, ArgMemR16PlusImm2(True, 4*i, 0), ArgReg(True, 4*i+2)))

	# Test byte compare (sub) plus branch
	# TODO: Test the conditional branches more directly
	for cc in [dut.CC_ALWAYS.value, dut.CC_Z.value, dut.CC_NZ.value, dut.CC_S.value, dut.CC_NS.value, dut.CC_C.value, dut.CC_NC.value, dut.CC_A.value, dut.CC_NA.value, dut.CC_V.value, dut.CC_NV.value, dut.CC_G.value, dut.CC_NG.value]:
		for j in range(-1,2):
			for i in range(-1,2):
				r = [0, randrange(1,32), randrange(-32, 0)]
				si, sj = [r[i], r[j]]
				exec(Binop(BinopNum.MOV, ArgReg(False, 0), ArgImm8(False, r[i])))
				exec(Binop(BinopNum.CMP, ArgReg(False, 0), ArgImm6(False, r[j])))

				#si, sj = sj, si # compensate for using revsub

				ui = si & 255
				uj = sj & 255

				res = (ui - uj) & 255

				if cc == dut.CC_ALWAYS.value: taken = True
				elif cc == dut.CC_Z.value: taken = ui == uj
				elif cc == dut.CC_NZ.value: taken = ui != uj
				elif cc == dut.CC_S.value:  taken = (res & 128) != 0
				elif cc == dut.CC_NS.value: taken = (res & 128) == 0
				elif cc == dut.CC_C.value:  taken = ui >= uj
				elif cc == dut.CC_NC.value: taken = ui <  uj
				elif cc == dut.CC_A.value:  taken = ui >  uj
				elif cc == dut.CC_NA.value: taken = ui <= uj
				elif cc == dut.CC_V.value:  taken = si >= sj
				elif cc == dut.CC_NV.value: taken = si <  sj
				elif cc == dut.CC_G.value:  taken = si >  sj
				elif cc == dut.CC_NG.value: taken = si <= sj
				else: assert False # unknown condition code

				offset = randrange(-128, 128 - min_jump)
				if offset >= 0: offset += min_jump
				#offset = randrange(min_jump, 4)

				#taken = (not taken) or cc == 0
				exec(Branch(offset, cc=cc, taken=taken))

	n_tests = 500

	# Reinitialize to random register values
	for i in range(0, NREGS): exec(Binop(BinopNum.MOV, ArgReg(False, i), ArgImm8(False, randrange(255))))


	# Test different kinds of indirect jumps
	for call in (False, True):
		exec(Jump(rand_arg_mem_r16r8(True), call=call))
		exec(Jump(rand_arg_mem_r16incdec(True), call=call))
		exec(Jump(rand_arg_mem_r16imm2(True), call=call))
		exec(Jump(rand_arg_mem_zp(True), call=False)) # Doesn't support call
		exec(Jump(rand_arg_reg(True), call=call))
		exec(Jump(rand_arg_sext_reg(True), call=call))
		#exec(Jump(rand_arg_zext_reg(True))) # not supported


	for iter in range(n_tests):
		rnd = randrange(13)
		wide = randbool() # used by most cases below
		if rnd == 0:
			if randbool():
				# Avoid jumping with offset = 0
				offset = randrange(-128, 128 - min_jump)
				if offset >= 0: offset += min_jump
				inst = Branch(offset)
				print("Branch ", inst.offset)
			else:
				arg = rand_arg(True, is_src=True, d_symmetry=False)
				call = False if isinstance(arg, ArgMemZP) else randbool()
				inst = Jump(arg, call=call)
				print("Call" if call else "Jump")
		elif rnd <= 2:
			shiftop = choice([ShiftopNum.ROR, ShiftopNum.SAR, ShiftopNum.SHR, ShiftopNum.ROL, ShiftopNum.SHL])
			if randbool():
				arg2 = ArgImm6(False, randrange(8 if shiftop == ShiftopNum.SHL and not wide else 16))
				#shiftop = choice([ShiftopNum.ROR, ShiftopNum.SAR, ShiftopNum.SHR]) # ROL is not supported for immediate; can just use ROR instead
			else:
				arg2 = rand_arg(False, is_src=True, zp_ok=False)
				#shiftop = choice([ShiftopNum.ROR, ShiftopNum.SAR, ShiftopNum.SHR, ShiftopNum.ROL])
			inst = Shift(shiftop, rand_arg_reg(wide), arg2)
		elif rnd == 3:
			if randrange(4) == 0: arg2 = rand_arg_reg(wide)
			else: arg2 = rand_arg_mem(wide)
			inst = Swap(rand_arg_reg(wide), arg2)
		elif rnd == 4:
			opnum = choice([BinopNum.MOV_NEG, BinopNum.REVSUB, BinopNum.MOV_NEGC, BinopNum.REVSBC, BinopNum.AND_NOT, BinopNum.OR_NOT, BinopNum.XOR_NOT, BinopNum.MOV_NOT])
			inst = Binop(opnum, rand_arg_reg(wide), rand_arg_reg(wide))
		elif mul_supported and rnd == 5:
			inst = Mul(ArgReg(wide, randrange(2, 8) & (6 if wide else 7)), rand_arg_imm6(False))
		else:
			opnum = choice([BinopNum.ADD, BinopNum.SUB, BinopNum.ADC, BinopNum.SBC, BinopNum.AND, BinopNum.OR, BinopNum.XOR, BinopNum.CMP, BinopNum.TEST, BinopNum.MOV])
			no_d = opnum in (BinopNum.CMP, BinopNum.TEST)
			if opnum != BinopNum.MOV and randrange(4) == 0: # mov r, imm6 has no encoding; use mov r, imm8 instead
				arg1, arg2 = rand_arg_reg(wide), rand_arg_imm6(wide)
			elif opnum == BinopNum.MOV and randbool(): # mov r, imm8 -- choose imm8 more often since it covers more of the encoding space
				arg1, arg2 = rand_arg_reg(wide), rand_arg_imm8(wide)
			else:
				if randbool() or no_d: arg1, arg2 = rand_arg_reg(wide), rand_arg(wide, is_src=True, d_symmetry = not no_d)
				else:                  arg2, arg1 = rand_arg_reg(wide), rand_arg(wide)

			# Avoid unsupported combinations
			if (opnum == BinopNum.TEST) and isinstance(arg2, ArgImm6): opnum = BinopNum.CMP

			inst = Binop(opnum, arg1, arg2)

		#inst = Binop(BinopNum.SUB, ArgMemR16PlusR8(True, 2, 7), ArgReg(True, 4))
		#inst = Binop(BinopNum.SUB, ArgMemR16PlusR8(False, 2, 7), ArgReg(False, 4))

		# Test instruction
		print("Test:\t", end="")
		exec(inst)

		# Read out the state so that the MockRAMEmulator will check it
		for i in range(2):
			exec(Binop(BinopNum.MOV, ArgMemR16PlusImm2(True, 4*i, 0), ArgReg(True, 4*i+2)))
		# Read out sp and re-randomize the affected register
		# TODO: Use a push or pop instead
		exec(Binop(BinopNum.MOV, ArgReg(True, 0), ArgRegSP(True)))
		exec(Binop(BinopNum.MOV, ArgReg(True, 0), ArgMemR16PlusImm2(True, 0, 0)))


		# TODO: read out flags so that we can test the values

		# Randomize one register value
		exec(Binop(BinopNum.MOV, ArgReg(False, iter & 7), ArgImm8(False, randrange(255))))
#	else:
#		n_tests = 7
#		for i in range(n_tests):
#			exec(Branch(8*(i+1)))


	#print("pcs = ", pcs)

	for i in range(300*n_tests):
		#tx, tx_fetch = dut.tx_pins.value.integer, dut.tx_fetch.value.integer
		uo_out = dut.uo_out.value.integer
		tx = uo_out & 3
		tx_fetch = ((uo_out >> 2) & 1) != 0
		tx_jump  = ((uo_out >> 3) & 1) != 0

		#print("tx = ", tx)
		rx = ram_emu.step(tx, alt=tx_fetch, alt_arg=tx_jump)
		#dut.rx_pins.value = rx
		dut.ui_in.value = rx

		if pos == len(instructions):
			print("Reached last instruction in", i, "cycles")
			print(i/len(instructions), "cycles/instruction word")
			break

		await ClockCycles(dut.clk, 1)

	print("pos =", pos)
	assert pos == len(instructions) # Check that we had time to fetch all instructions
