# SPDX-FileCopyrightText: © 2024 Toivo Henningsson
# SPDX-License-Identifier: MIT

from enum import Enum
from random import randrange, getrandbits, choice

from ram_emulator import MockRAMEmulator

REG_BITS = 8
LOG2_NR = 3

PAIR_BITS = 2*REG_BITS
REG_BITS_MASK = (1 << REG_BITS) - 1
PAIR_BITS_MASK = (1 << PAIR_BITS) - 1
NREGS = 1 << LOG2_NR


def szext(value, pair=False, sext=False):
	if pair: return value
	value = value & REG_BITS_MASK
	if sext and (value & (1 << (REG_BITS - 1))) != 0: return value - (1 << REG_BITS) + (1 << PAIR_BITS)
	else: return value

class State:
	def __init__(self):
		self.regs = [0 for i in range(NREGS)]
		self.carry = 0
		self.pc = 2 # Currently, the first instruction is fetched from address 2.
		self.ram_emu = MockRAMEmulator(delay=1)

		self.flag_c = 0
		self.flag_v = 0
		self.flag_s = 0
		self.flag_z = 0

		self.jumped = False

	def get_reg(self, index, pair=False, sext=False):
		if pair: return self.regs[index & ~1] | (self.regs[index | 1] << REG_BITS)
		else:
			return szext(self.regs[index], sext=sext)

	def get_pc(self):
		return self.pc

	def set_pc(self, pc):
		self.jumped = True
		self.pc = pc & 0xffff

	def step_pc(self, step=1, jump=False):
		assert step==1 or jump
		self.jumped = jump
		self.pc = (self.pc + 2*step) & 0xffff

	def set_reg(self, index, value, pair=False):
		if pair:
			self.regs[index & ~1] = value & REG_BITS_MASK
			self.regs[index | 1] = (value >> REG_BITS) & REG_BITS_MASK
		else: self.regs[index] = value & REG_BITS_MASK

	def set_dest(self, dest, value):
		value &= bitmask(dest.wide)
		if isinstance(dest, ArgReg):
			self.set_reg(dest.get_reg(), value, dest.wide)
		elif isinstance(dest, ArgMem):
			self.ram_emu.write_mem(dest.get_addr(self), value, dest.wide)
		else:
			raise TypeError("Unsupported dest type: ", type(dest))

	def apply(self, alu):
		regs = alu.registers.regs

		alu.carry.value = self.carry
		for i in range(NREGS): regs[i].value = self.regs[i]


def nbits(pair): return PAIR_BITS if pair else REG_BITS
def bitmask(pair): return PAIR_BITS_MASK if pair else REG_BITS_MASK


class BinopNum(Enum):
	ADD = 0
	SUB = 1
	ADC = 2
	SBC = 3
	AND = 4
	OR  = 5
	XOR = 6
	CMP = 7
	TEST = 8
	MOV  = 9


class Arg:
	"""arg"""
	def __init__(self, wide):
		assert isinstance(wide, bool)
		self.wide = wide

	def get_dest(self, state):
		return self

	def apply_side_effect(self, state):
		pass

class ArgMem(Arg):
	"""[addr]"""
	def __init__(self, wide):
		super().__init__(wide)

	def get_value(self, state):
		return state.ram_emu.read_mem(self.get_addr(state), self.wide)

	def get_dest(self, state):
		return ArgMemDest(self.wide, self.get_addr(state))

class ArgMemR16PlusR8(ArgMem):
	"""[r16 + r8]"""
	def __init__(self, wide, reg16, reg8):
		assert 0 <= reg16 <= 7
		assert (reg16 & 1) == 0
		assert 0 <= reg8 <= 7

		super().__init__(wide)
		self.reg16 = reg16
		self.reg8  = reg8

	def get_addr(self, state):
		return (state.get_reg(self.reg16, True) + state.get_reg(self.reg8, False)) & 0xffff
		#return (state.get_reg(self.reg16, True) + (state.get_reg(self.reg8, False) << self.wide)) & 0xffff

	def encode(self, is_dest=False, T=None):
		return 32 | (self.reg16 << 2) | self.reg8

class ArgMemR16IncDec(ArgMem):
	"""[r16++]/[--r16]"""
	def __init__(self, wide, reg16, dec=False):
		assert 0 <= reg16 <= 7
		assert (reg16 & 1) == 0

		super().__init__(wide)
		self.reg16 = reg16
		self.dec = dec

	def get_addr(self, state):
		return (state.get_reg(self.reg16, True) + ((-1 if self.dec else 0) << self.wide)) & 0xffff

	def apply_side_effect(self, state):
		state.set_reg(self.reg16, state.get_reg(self.reg16, True) + ((-1 if self.dec else 1) << self.wide), True)

	def encode(self, is_dest=False, T=None):
		reg8 = self.reg16 | self.dec
		return 32 | (self.reg16 << 2) | reg8

class ArgMemDest(ArgMem):
	"""ArgMemDest"""
	def __init__(self, wide, addr):
		super().__init__(wide)
		self.addr = addr

	def get_addr(self, state):
		return self.addr

class ArgMemR16PlusImm2(ArgMem):
	"""[r16 + imm2]"""
	def __init__(self, wide, reg16, imm2):
		assert 0 <= reg16 <= 7
		assert (reg16 & 1) == 0
		if wide:
			assert 0 <= imm2 <= 7
			assert (imm2 & 1) == 0
		else:
			assert 0 <= imm2 <= 3

		super().__init__(wide)
		self.reg16 = reg16
		self.imm2  = imm2

	def get_addr(self, state):
		return (state.get_reg(self.reg16, True) + self.imm2) & 0xffff

	def encode(self, is_dest=False, T=None):
		return 16 | (self.reg16 << 1) | (self.imm2 >> self.wide)

class ArgMemZP(ArgMem):
	"""[zp=imm7]"""
	def __init__(self, wide, addr):
		if wide:
			assert (addr&1) == 0
			assert 0 <= addr <= 255
		else:
			assert 0 <= addr <= 127
		super().__init__(wide)
		self.addr = addr

	def get_addr(self, state=None):
		return self.addr

	def encode(self, is_dest=False, T=None):
		assert T == ArgMemZP
		return (self.addr >> self.wide) & 127

class ArgReg(Arg):
	"""r8/r16"""
	def __init__(self, wide, reg):
		assert 0 <= reg <= 7
		if wide: assert (reg & 1) == 0

		super().__init__(wide)
		self.reg = reg

	def get_reg(self):
		return self.reg

	def get_value(self, state):
		return state.get_reg(self.reg, self.wide)

	def encode(self, is_dest=False, T=None):
		if self.wide: return self.reg
		else:         return 8 | self.reg

class ArgSextReg(Arg):
	"""sext(r8)"""
	def __init__(self, wide, reg):
		assert 0 <= reg <= 7

		super().__init__(wide)
		self.reg = reg

	def get_reg(self):
		return self.reg

	def get_value(self, state):
		return state.get_reg(self.reg, False, sext=True)

	def encode(self, is_dest=False, T=None):
		assert not is_dest
		return 8 | self.reg

class ArgZextReg(Arg):
	"""zext(r8)"""
	def __init__(self, wide, reg):
		assert 0 <= reg <= 7

		super().__init__(wide)
		self.reg = reg

	def get_reg(self):
		return self.reg

	def get_value(self, state):
		return state.get_reg(self.reg, False)

	def encode(self, is_dest=False, T=None):
		assert T == ArgZextReg
		assert not is_dest
		return 8 | self.reg

class ArgImm(Arg):
	"""immediate"""
	def __init__(self, wide, value):
		super().__init__(wide)
		self.value = value

class ArgImm6(Arg):
	"""imm6"""
	def __init__(self, wide, value):
		assert -32 <= value <= 31
		super().__init__(wide)
		self.value = value

	def get_value(self, state=None):
		return self.value

	def encode(self, is_dest=False, T=None):
		assert not is_dest
		assert T == ArgImm6
		return self.value & 63

class ArgImm8(Arg):
	"""imm8"""
	def __init__(self, wide, value):
		if wide: assert -128 <= value <= 127
		else:    assert -128 <= value <= 255 # TODO: better way?
		super().__init__(wide)
		self.value = value

	def get_value(self, state=None):
		return self.value

	def encode(self, is_dest=False, T=None):
		assert not is_dest
		assert T == ArgImm8
		return self.value & 255

# Temporary: Might be removed, or encoding changed
class ArgPCPlusImm8(Arg):
	"""pc + imm8"""
	def __init__(self, wide, offset):
		assert False # not implemented
		assert wide
		assert -128 <= offset <= 127
		super().__init__(wide)
		self.offset = offset

	def get_value(self, state=None):
		assert False # not implemented yet
		return state.get_pc() + self.offset

	def encode(self, is_dest=False, T=None):
		assert not is_dest
		assert T == ArgPCPlusImm8
		return self.offset & 255

#class ArgPC(Arg):
#	"""pc"""
#	def __init__(self, wide):
#		assert wide
#		super().__init__(wide)
#
#	def get_value(self, state):
#		return state.get_pc()
#
#	def encode(self, is_dest=False, T=None):
#		assert not is_dest
#		return 7


class Instruction:
	def __init__(self):
		pass

class Binop(Instruction):
	def __init__(self, binop, arg1, arg2):
		assert isinstance(binop, BinopNum)
		assert isinstance(arg1, Arg)
		assert isinstance(arg2, Arg)
		assert isinstance(arg1, ArgReg) or isinstance(arg2, ArgReg)
		assert arg1.wide == arg2.wide
		wide = arg1.wide

		super().__init__()
		self.wide = wide
		self.binop = binop
		self.arg1 = arg1
		self.arg2 = arg2

	def get_dest(self, state):
		return self.arg1.get_dest(state)

	def execute(self, state):
		#arg1 = self.arg1.get_value(state)
		#arg2 = self.arg2.get_value(state)

		# Calculate memory addresses before applying side effects (postincrement/predecrement)
		if isinstance(self.arg1, ArgMem): arg1 = self.arg1.get_value(state)
		if isinstance(self.arg2, ArgMem): arg2 = self.arg2.get_value(state)
		dest = self.get_dest(state)

		self.arg1.apply_side_effect(state)
		self.arg2.apply_side_effect(state)

		# Read registers as data after applying side effects, even postincrement
		if not isinstance(self.arg1, ArgMem): arg1 = self.arg1.get_value(state)
		if not isinstance(self.arg2, ArgMem): arg2 = self.arg2.get_value(state)

		arg1 = arg1 & bitmask(self.wide)
		arg2 = arg2 & bitmask(self.wide)

		if   self.binop == BinopNum.ADD:  result = arg1 + arg2
		elif self.binop == BinopNum.SUB:  result = arg1 - arg2 + (bitmask(self.wide)+1)
		elif self.binop == BinopNum.ADC:  result = arg1 + arg2 + state.flag_c
		elif self.binop == BinopNum.SBC:  result = arg1 - arg2 + state.flag_c + bitmask(self.wide)
		elif self.binop == BinopNum.AND:  result = arg1 & arg2
		elif self.binop == BinopNum.OR:   result = arg1 | arg2
		elif self.binop == BinopNum.XOR:  result = arg1 ^ arg2
		elif self.binop == BinopNum.CMP:  result = arg1 # No change
		elif self.binop == BinopNum.TEST: result = arg1 # No change
		elif self.binop == BinopNum.MOV:  result = arg2
		else: raise ValueError("Unsupported binop value: ", self.binop)

		if self.binop != BinopNum.MOV:
			state.flag_z = (result & bitmask(self.wide)) == 0
			state.flag_s = ((result >> (8*(1+self.wide)-1)) & 1) != 0

		if self.binop in (BinopNum.ADD, BinopNum.SUB, BinopNum.ADC, BinopNum.SBC):
			state.flag_c = ((result >> (8*(1+self.wide))) & 1) != 0
			# TODO; model flag_v

		print("binop(", self.wide, ", ", self.binop, ", ", hex(arg1), ", ", hex(arg2), ") =", hex(result))

		state.set_dest(dest, result)
		state.step_pc()

	def encode(self):
		assert 0 <= self.binop.value <= 7 or self.binop == BinopNum.MOV

#	111111
#	5432109876543210
#	1eaaamrrdziiiiii	2^15 r8:  8x binop/shift
#	01aaamrrdziiiiii	2^14 r16: 8x binop/shift
#	001gomrrdziiiiii	2^13 r8:  mov/shift/swap/inc/dec/zero
#	0001omrrdziiiiii	2^12 r16: mov/shift/swap/inc/dec/zero

		d = not isinstance(self.arg1, ArgReg)
		if d: arg2, arg1 = self.arg1, self.arg2
		else: arg1, arg2 = self.arg1, self.arg2

		m = 1
		z = 0
		force_d = False

		T = None

		if isinstance(arg2, ArgImm6):
			assert (m, d) == (1, 0)
			z = 1
			T = ArgImm6
		elif isinstance(arg2, ArgMemZP):
			assert m == 1
			(m, z) = (0, 0) # The zero page address can override z=0
			T = ArgMemZP
		elif isinstance(arg2, ArgZextReg):
			assert self.wide
			assert (m, d) == (1, 0)
			force_d = True
			T = ArgZextReg

		reg1 = arg1.get_reg()

		binop_form = (self.binop.value < 8)

		if binop_form:
			prefix = 1 if self.wide else 2 | (reg1&1)
			return (prefix << 14) | (self.binop.value << 11) | (m << 10) | (((reg1&6)|(d or force_d)) << 7) | (z << 6) | arg2.encode(is_dest=d, T=T)
		else:
			o = 1

			if isinstance(arg2, ArgImm8):
				assert self.binop == BinopNum.MOV
				assert (m, d) == (1, 0)
				d, z = 0, 0 # Will be overridden by the immediate data, but d acts as if it is zero
				o = 0
				T = ArgImm8
			elif isinstance(arg2, ArgPCPlusImm8):
				assert self.binop == BinopNum.MOV
				assert (m, d) == (1, 0)
				m = 0
				d, z = 0, 0 # Will be overridden by the immediate data, but d acts as if it is zero
				o = 0
				T = ArgPCPlusImm8

			prefix = 1 if self.wide else 2 | (reg1&1)
			#print((prefix, o, m, reg1, d, z, arg2.encode(is_dest=d), type(arg2)))
			return (prefix << 12) | (o << 11) | (m << 10) | (((reg1&6)|(d or force_d)) << 7) | (z << 6) | arg2.encode(is_dest=d, T=T)

class Jump(Instruction):
	def __init__(self, arg):
		assert isinstance(arg, Arg)
		assert arg.wide

		super().__init__()
		self.arg = arg

	def execute(self, state):
		# Calculate memory addresses before applying side effects (postincrement/predecrement)
		if isinstance(self.arg, ArgMem): arg = self.arg.get_value(state)

		self.arg.apply_side_effect(state)

		# Read registers as data after applying side effects, even postincrement
		if not isinstance(self.arg, ArgMem): arg = self.arg.get_value(state)

		arg = arg & bitmask(True)


		print("jump(", hex(arg), "=)")

		state.set_pc(arg)

	def encode(self):
		if isinstance(self.arg, ArgMemZP):
			# 111111
			# 5432109876543210
			# 000100000ziiiiii	mov pc, [zp]
			return (1 << 12) | self.arg.encode(is_dest=False, T=ArgMemZP)
		else:
			# 111111
			# 5432109876543210
			# 0010000000iiiiii	mov pc, src
			return (1 << 13) | self.arg.encode(is_dest=False)


class Branch(Instruction):
	def __init__(self, offset, cc=0, taken=None):
		assert -128 <= offset <= 127
		assert 0 <= cc <= 15

		super().__init__()
		self.offset = offset
		self.cc = cc
		if cc == 0: # always
			assert taken != False
			taken = True
		self.taken = taken

	def execute(self, state):
		#print("PC = ", state.pc)
		assert isinstance(self.taken, bool)
		if self.taken: state.step_pc(self.offset, jump=True)
		else: state.step_pc()
		#print("PC = ", state.pc)

	def encode(self):
#	111111
#	5432109876543210
#	0000ccccbbbbbbbb	2^12 branch

		return (self.cc << 8) | (self.offset & 255)


def randbool():
	return bool(getrandbits(1))

def rand_arg_reg(wide):
	return ArgReg(wide, randrange(NREGS)&~wide)

def rand_arg_sext_reg(wide):
	return ArgSextReg(wide, randrange(NREGS))

def rand_arg_zext_reg(wide):
	return ArgZextReg(wide, randrange(NREGS))

def rand_arg_mem_r16r8(wide):
	r16 = randrange(NREGS)&~1
	r8 = randrange(NREGS-2)
	if (r8&~1) == r16: r8 += 2
	assert (r8&~1) != r16
	return ArgMemR16PlusR8(wide, r16, r8)

def rand_arg_mem_r16incdec(wide):
	return ArgMemR16IncDec(wide, randrange(NREGS)&~1, randbool())

def rand_arg_mem_r16imm2(wide):
	return ArgMemR16PlusImm2(wide, randrange(NREGS)&~1, randrange(3)<<wide)

def rand_arg_mem_zp(wide):
	return ArgMemZP(wide, randrange(128) << wide)

def rand_arg_imm6(wide):
	return ArgImm6(wide, randrange(-32, 32))

def rand_arg_imm8(wide):
	return ArgImm8(wide, randrange(-128, 128))

def rand_arg(wide, is_src=False, d_symmetry=True):
	n = randrange(5+(1+d_symmetry)*(is_src and wide))
	if   n == 0: return rand_arg_mem_r16r8(wide)
	elif n == 1: return rand_arg_mem_r16incdec(wide)
	elif n == 2: return rand_arg_mem_r16imm2(wide)
	elif n == 3: return rand_arg_mem_zp(wide)
	elif n == 4: return rand_arg_reg(wide)
	# is_src and wide:
	elif n == 5: return rand_arg_sext_reg(wide)
	elif n == 6: return rand_arg_zext_reg(wide)
	else: assert False
