# SPDX-FileCopyrightText: © 2024 Toivo Henningsson
# SPDX-License-Identifier: MIT

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer, ClockCycles
from random import randrange, getrandbits, choice

from ram_emulator import RAMEmulator
from isa import *


@cocotb.test()
async def test_decoder(dut):
	dut._log.info("start")
	clock = Clock(dut.clk, 2, units="us")
	cocotb.start_soon(clock.start())

	preserved = True
	decoder = dut.dec
	try:
		decoder = dut.dec
	except AttributeError:
		preserved = False

	if preserved:
		sched = decoder.sched
		alu = sched.alu
		regfile = alu.registers
		regs = regfile.regs

		for i in range(NREGS): regs[i].value = i**2

	# reset
	dut._log.info("reset")
	dut.rst_n.value = 0
	await ClockCycles(dut.clk, 10)
	dut.rst_n.value = 1

	if preserved:

		state = State()

		#mem = [0 for i in range(32768)]
		#ram_emu = RAMEmulator(mem)
		#mem[0x3124>>1] = 0xe4e4
		ram_emu = state.ram_emu

		for iter in range(2000):
			state.ram_emu.reset()

			# Fresh random state. TODO: Don't reinitialize everything?
			for i in range(0, NREGS, 2): state.set_reg(i, randrange(0x10000), True)
			state.apply(alu)

			#inst = Binop(BinopNum.SUB, ArgReg(False, 5), ArgReg(False, 4))
			wide = randbool()
			if randrange(5) == 0:

				#arg2 = ArgImm6(False, 2*randrange(8))
				if randbool(): arg2 = ArgImm6(False, 2*randrange(8))
				else: arg2 = rand_arg(False, is_src=True, zp_ok=False)
				inst = Shift(choice([ShiftopNum.ROR, ShiftopNum.SHR]), rand_arg_reg(wide), arg2)

			else:
				# TODO: Test CMP, TEST
				#opnum = choice([BinopNum.ADD, BinopNum.SUB, BinopNum.AND, BinopNum.OR, BinopNum.XOR, BinopNum.MOV])
				#opnum = choice([BinopNum.ADD, BinopNum.SUB, BinopNum.ADC, BinopNum.SBC, BinopNum.AND, BinopNum.OR, BinopNum.XOR, BinopNum.CMP, BinopNum.MOV])
				opnum = choice([BinopNum.ADD, BinopNum.SUB, BinopNum.ADC, BinopNum.SBC, BinopNum.AND, BinopNum.OR, BinopNum.XOR, BinopNum.MOV])
				if opnum != BinopNum.MOV and randrange(4) == 0: # mov r, imm6 has no encoding; use mov r, imm8 instead
					arg1, arg2 = rand_arg_reg(wide), rand_arg_imm6(wide)
				elif opnum == BinopNum.MOV and randbool(): # mov r, imm8 -- choose imm8 more often since it covers more of the encoding space
					arg1, arg2 = rand_arg_reg(wide), rand_arg_imm8(wide)
				else:
					if randbool(): arg1, arg2 = rand_arg_reg(wide), rand_arg(wide, is_src=True)
					else:          arg2, arg1 = rand_arg_reg(wide), rand_arg(wide)

				inst = Binop(opnum, arg1, arg2)

			#inst = Binop(BinopNum.SUB, ArgReg(False, 5), ArgMemR16PlusR8(False, 2, 7))
			#inst = Binop(BinopNum.SUB, ArgMemR16PlusR8(False, 2, 7), ArgReg(False, 5))
			#inst = Binop(BinopNum.SUB, ArgReg(True, 4), ArgMemR16PlusR8(True, 2, 7))
			#inst = Binop(BinopNum.SUB, ArgReg(False, 4), ArgMemR16PlusR8(False, 2, 7))
			#inst = Binop(BinopNum.SUB, ArgMemR16PlusR8(True, 2, 7), ArgReg(True, 4))
			#inst = Binop(BinopNum.SUB, ArgReg(False, 4), ArgImm6(False, 23))
			#inst = Binop(BinopNum.SUB, ArgReg(wide, 4), rand_arg_imm6(wide))
			#inst = Binop(BinopNum.MOV, ArgReg(True, 2), ArgReg(True, 4))
			#inst = Binop(BinopNum.MOV, rand_arg_reg(wide), rand_arg_imm8(wide))
			#inst = Binop(BinopNum.SUB, ArgReg(False, 5), ArgMemZP(False, 121))
			#inst = Binop(BinopNum.SUB, rand_arg_reg(False), rand_arg_mem_zp(False))
			#inst = Binop(BinopNum.SUB, ArgMemZP(False, 121), ArgReg(False, 5))
			#inst = Binop(BinopNum.SUB, ArgReg(True, 4), ArgMemZP(True, 242))
			#inst = Binop(BinopNum.MOV, ArgReg(True, 4), ArgMemZP(True, 0xd2))
			#inst = Binop(BinopNum.SUB, ArgReg(True, 4), ArgMemR16PlusImm2(True, 2, 6))
			#inst = Binop(BinopNum.MOV, ArgReg(True, 4), ArgSextReg(True, 7))
			#inst = Binop(BinopNum.MOV, ArgReg(True, 4), ArgZextReg(True, 7))
			#inst = Binop(BinopNum.SUB, ArgReg(wide, 4), ArgMemR16IncDec(wide, 6, False))
			#inst = Binop(BinopNum.SUB, ArgMemR16IncDec(wide, 6, False), ArgReg(wide, 4))
			#inst = Shift(choice([ShiftopNum.ROR, ShiftopNum.SHR]), rand_arg_reg(wide), ArgImm6(False, 2*randrange(8)))

			inst.execute(state)

			encoding = inst.encode()
			assert 1 <= len(encoding) <= 2
			dut.inst.value = encoding[0]
			dut.imm_data.value = encoding[-1]
			dut.inst_valid.value = 1

			for i in range(48):
				tx = dut.tx_pins.value.integer
				rx = ram_emu.step(tx)
				dut.rx_pins.value = rx

				await ClockCycles(dut.clk, 1)
				if dut.inst_valid.value.integer == 0 and ram_emu.done(): break

			assert dut.inst_valid.value.integer == 0 and ram_emu.done()

			#break
			for i in range(NREGS): assert int(regs[i].value) == state.regs[i]

			assert alu.flag_c.value.integer == int(state.flag_c)
			# TODO: test flag_v
			assert alu.flag_s.value.integer == int(state.flag_s)
			assert alu.flag_z.value.integer == int(state.flag_z)

	else:
		await ClockCycles(dut.clk, 1)
