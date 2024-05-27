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

REG_INDEX_MUL_MSB_DEST = 7


def sext(value, pair=False):
	value = value & bitmask(pair)
	if pair:
		if sext and (value & (1 << (PAIR_BITS - 1))) != 0: return value - (1 << PAIR_BITS)
		else: return value
	else:
		if sext and (value & (1 << (REG_BITS - 1))) != 0: return value - (1 << REG_BITS)
		else: return value

def szext(value, pair=False, sext=False):
	if pair: return value
	value = value & REG_BITS_MASK
	if sext and (value & (1 << (REG_BITS - 1))) != 0: return value - (1 << REG_BITS) + (1 << PAIR_BITS)
	else: return value

class State:
	def __init__(self):
		self.regs = [0 for i in range(NREGS)]
		self.sp = 0
		self.carry = 0
		self.pc = 0xfffc # Currently, the first instruction is fetched from address 0xfffc.
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
		assert 1 <= step <= 2 or jump
		self.jumped = jump
		self.pc = (self.pc + 2*step) & 0xffff

	def set_reg(self, index, value, pair=False):
		if pair:
			self.regs[index & ~1] = value & REG_BITS_MASK
			self.regs[index | 1] = (value >> REG_BITS) & REG_BITS_MASK
		else: self.regs[index] = value & REG_BITS_MASK

	def get_sp(self, wide):
		if wide: return self.sp + 0x100
		else:    return self.sp

	def set_sp(self, value): self.sp = value & 0xff

	def get_flags(self): return self.flag_z | (self.flag_s << 1) | (self.flag_v << 2) | (self.flag_c << 3)
	def set_flags(self, value):
		self.flag_z = (value & 1) != 0
		self.flag_s = (value & 2) != 0
		self.flag_v = (value & 4) != 0
		self.flag_c = (value & 8) != 0

	def set_dest(self, dest, value):
		value &= bitmask(dest.wide)
		if isinstance(dest, ArgReg):
			self.set_reg(dest.get_reg(), value, dest.wide)
		elif isinstance(dest, ArgMem):
			self.ram_emu.write_mem(dest.get_addr(self), value, dest.wide)
		elif isinstance(dest, ArgRegSP):
			self.set_sp(value)
		elif isinstance(dest, ArgRegFlags):
			self.set_flags(value)
		else:
			raise TypeError("Unsupported dest type: ", type(dest))

	def apply(self, alu):
		regs   = alu.registers
		g_regs = regs.general_registers.regs
		sp_reg = regs.sp_register.regs

		alu.carry.value = self.carry
		for i in range(NREGS): g_regs[i].value = self.regs[i]
		sp_reg.value = self.sp

		alu.flag_c.value = self.flag_c
		alu.flag_v.value = self.flag_v
		alu.flag_s.value = self.flag_s
		alu.flag_z.value = self.flag_z


def nbits(pair): return PAIR_BITS if pair else REG_BITS
def bitmask(pair): return PAIR_BITS_MASK if pair else REG_BITS_MASK


class BinopNum(Enum):
	ADD = 0
	SUB = 1 # Interpeted as revsub when arg2 is an imm6
	ADC = 2
	SBC = 3
	AND = 4
	OR  = 5
	XOR = 6
	CMP = 7
	TEST = 8
	MOV  = 9

	MOV_NEG  = 16 + ADD
	REVSUB   = 16 + SUB
	MOV_NEGC = 16 + ADC
	REVSBC   = 16 + SBC
	AND_NOT  = 16 + AND
	OR_NOT   = 16 + OR
	XOR_NOT  = 16 + XOR

	MOV_NOT  = 16 + 9

class ShiftopNum(Enum):
	ROR = 0
	SAR = 2
	SHR = 4
	SHL = 6
	ROL = 7

class CCNum(Enum):
	ALWAYS = 0x0
	CALL   = 0x1
	DJNZ0  = 0x2
	DJNZ1  = 0x3
	Z      = 0x4
	NZ     = 0x5
	S      = 0x6
	NS     = 0x7
	C      = 0x8
	NC     = 0x9
	A      = 0xa
	NA     = 0xb
	V      = 0xc
	NV     = 0xd
	G      = 0xe
	NG     = 0xf
	AE     = C
	NB     = C
	NAE    = NC
	B      = NC
	NBE    = A
	BE     = NA
	GE     = V
	NL     = V
	NGE    = NV
	L      = NV
	NLE    = G
	LE     = NG


class Arg:
	"""arg"""
	def __init__(self, wide):
		assert isinstance(wide, bool)
		self.wide = wide

	def get_dest(self, state):
		return self

	def get_extra_words(self):
		return 0

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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		return 32 | (self.reg16 << 2) | self.reg8

	def __str__(self):
		return f"ArgMemR16PlusR8({self.reg16}, {self.reg8})"

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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		reg8 = self.reg16 | self.dec
		return 32 | (self.reg16 << 2) | reg8

	def __str__(self):
		return f"ArgMemR16IncDec({self.reg16}, {self.dec})"

class ArgMemPushPopTop(ArgMem):
	"""[push/pop/top]"""
	def __init__(self, wide, dir=0): # push: dir=1, pop: dir=-1, top: dir = 0. Set dir=0 and it will be updated by execute if needed
		assert -1 <= dir <= 1

		super().__init__(wide)
		self.dir = dir

	def get_addr(self, state):
		return (state.get_sp(True) + ((-1 if self.dir == 1 else 0) << self.wide)) & 0xffff

	def apply_side_effect(self, state):
		state.set_sp(state.get_sp(False) - (self.dir << self.wide))

	def encode(self, is_dest=False, T=None, extra_dest=None):
		return 5

	def __str__(self):
		return f"ArgMemPushPopTop({self.dir})"

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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		return 16 | (self.reg16 << 1) | (self.imm2 >> self.wide)

	def __str__(self):
		return f"ArgMemR16PlusImm2({self.reg16}, {self.imm2})"

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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert T == ArgMemZP
		return (self.addr >> self.wide) & 127

	def __str__(self):
		return f"ArgMemZP({self.addr})"

class ArgMemImm16(ArgMem):
	"""imm16"""
	def __init__(self, wide, addr):
		assert 0 <= addr <= 65535 # TODO: better way?
		super().__init__(wide)
		self.addr = addr

	def get_addr(self, state=None):
		return self.addr

	def get_extra_words(self):
		return 1

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert isinstance(extra_dest, list)
		assert len(extra_dest) == 0
		extra_dest.append(self.addr)
		return 3

	def __str__(self):
		return f"ArgMemImm16({self.addr})"


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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		if self.wide: return self.reg
		else:         return 8 | self.reg

	def __str__(self):
		return f"ArgReg({self.reg})"

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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert not is_dest
		return 8 | self.reg

	def __str__(self):
		return f"ArgSextReg({self.reg})"

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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert T == ArgZextReg
		assert not is_dest
		return 8 | self.reg

	def __str__(self):
		return f"ArgZextReg({self.reg})"

class ArgRegSP(Arg):
	"""sp"""
	def __init__(self, wide):
		super().__init__(wide)

	def get_value(self, state=None):
		return state.get_sp(self.wide)

	def encode(self, is_dest=False, T=None, extra_dest=None):
		return 7

	def __str__(self):
		return f"ArgRegSP()"


class ArgRegFlags(Arg):
	"""sp"""
	def __init__(self):
		super().__init__(False)

	def get_value(self, state=None):
		return state.get_flags()

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert not self.wide
		# 000010 flags when wide=0
		return 2

	def __str__(self):
		return f"ArgRegFlags()"


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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert not is_dest
		assert T == ArgImm6
		return self.value & 63

	def __str__(self):
		return f"ArgImm6({self.value})"

class ArgImm8(Arg):
	"""imm8"""
	def __init__(self, wide, value):
		if wide: assert -128 <= value <= 127
		else:    assert -128 <= value <= 255 # TODO: better way?
		super().__init__(wide)
		self.value = value

	def get_value(self, state=None):
		return self.value

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert not is_dest
		assert T == ArgImm8
		return self.value & 255

	def __str__(self):
		return f"ArgImm8({self.value})"

class ArgImm16(Arg):
	"""imm16"""
	def __init__(self, wide, value):
		if wide: assert -32768 <= value <= 65535 # TODO: better way?
		else:    assert -128 <= value <= 255
		super().__init__(wide)
		self.value = value

	def get_value(self, state=None):
		return self.value

	def get_extra_words(self):
		return 1

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert not is_dest
		assert isinstance(extra_dest, list)
		assert len(extra_dest) == 0
		extra_dest.append(self.value & 0xffff)
		return 1

	def __str__(self):
		return f"ArgImm16({self.value})"

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

	def encode(self, is_dest=False, T=None, extra_dest=None):
		assert not is_dest
		assert T == ArgPCPlusImm8
		return self.offset & 255

	def __str__(self):
		return f"ArgPCPlusImm8({self.offset})"

#class ArgPC(Arg):
#	"""pc"""
#	def __init__(self, wide):
#		assert wide
#		super().__init__(wide)
#
#	def get_value(self, state):
#		return state.get_pc()
#
#	def encode(self, is_dest=False, T=None, extra_dest=None):
#		assert not is_dest
#		return 7


class Instruction:
	def __init__(self):
		pass

	def adjust(self):
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

	def adjust(self):
		if isinstance(self.arg1, ArgMemPushPopTop) and self.binop == BinopNum.MOV: self.arg1.dir =  1 # push
		if isinstance(self.arg2, ArgMemPushPopTop):                                self.arg2.dir = -1 # pop

	def execute(self, state):
		self.adjust()

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

		update_dest = True

		# replace sub r, imm6 with revsub r, imm6
		if isinstance(self.arg2, ArgImm6) and self.binop in (BinopNum.SUB, BinopNum.SBC):
			arg1, arg2 = arg2, arg1

		if   self.binop == BinopNum.ADD:     result = arg1 + arg2
		elif self.binop == BinopNum.SUB:     result = arg1 - arg2 + (bitmask(self.wide)+1)
		elif self.binop == BinopNum.REVSUB:  result = arg2 - arg1 + (bitmask(self.wide)+1)
		elif self.binop == BinopNum.MOV_NEG: result =      - arg2 + (bitmask(self.wide)+1)
		elif self.binop == BinopNum.CMP:     result = arg1 - arg2 + (bitmask(self.wide)+1); update_dest = False # same as SUB but don't update dest
		elif self.binop == BinopNum.ADC:     result = arg1 + arg2 + state.flag_c
		elif self.binop == BinopNum.SBC:     result = arg1 - arg2 + state.flag_c + bitmask(self.wide)
		elif self.binop == BinopNum.REVSBC:  result = arg2 - arg1 + state.flag_c + bitmask(self.wide)
		elif self.binop == BinopNum.MOV_NEGC:result =      - arg2 + state.flag_c + bitmask(self.wide)
		elif self.binop == BinopNum.AND:     result = arg1 & arg2
		elif self.binop == BinopNum.TEST:    result = arg1 & arg2; update_dest = False
		elif self.binop == BinopNum.OR:      result = arg1 | arg2
		elif self.binop == BinopNum.XOR:     result = arg1 ^ arg2
		elif self.binop == BinopNum.MOV:     result = arg2
		elif self.binop == BinopNum.AND_NOT: result = arg1 & (~arg2 & bitmask(self.wide))
		elif self.binop == BinopNum.OR_NOT:  result = arg1 | (~arg2 & bitmask(self.wide))
		elif self.binop == BinopNum.XOR_NOT: result = arg1 ^ (~arg2 & bitmask(self.wide))
		elif self.binop == BinopNum.MOV_NOT: result = (~arg2 & bitmask(self.wide))
		else: raise ValueError("Unsupported binop value: ", self.binop)

		if not isinstance(self.arg1, ArgRegFlags):
			if self.binop != BinopNum.MOV:
				state.flag_z = (result & bitmask(self.wide)) == 0
				state.flag_s = ((result >> (8*(1+self.wide)-1)) & 1) != 0

			if self.binop in (BinopNum.ADD, BinopNum.SUB, BinopNum.ADC, BinopNum.SBC, BinopNum.CMP, BinopNum.MOV_NEG, BinopNum.REVSUB, BinopNum.MOV_NEGC, BinopNum.REVSBC):
				state.flag_c = ((result >> (8*(1+self.wide))) & 1) != 0
				# TODO; model flag_v

		#print("binop(", self.wide, ", ", self.binop, ", ", hex(arg1), ", ", hex(arg2), ") =", hex(result))
		print("binop(", self.wide, ", ", self.binop, ", ", str(self.arg1), ":", hex(arg1), ", ", str(self.arg2), ":", hex(arg2), ") =", hex(result))

		if update_dest: state.set_dest(dest, result)
		state.step_pc(1 + self.arg1.get_extra_words() + self.arg2.get_extra_words())

	def encode(self):
		#assert 0 <= self.binop.value <= 7 or self.binop == BinopNum.MOV
		#assert 0 <= self.binop.value <=  BinopNum.MOV.value
		assert 0 <= self.binop.value <=  BinopNum.MOV.value or (isinstance(self.arg1, ArgReg) and isinstance(self.arg2, ArgReg))
		assert not (self.binop.value in (BinopNum.CMP, BinopNum.TEST) and not isinstance(self.arg1, ArgReg))

		extra = []

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
		force_no_d = False

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
		elif isinstance(arg2, ArgSextReg):
			force_no_d = True

		reg1 = arg1.get_reg()

		binop = self.binop
		if binop.value >= BinopNum.MOV_NEG.value:
			assert isinstance(self.arg1, ArgReg) and isinstance(self.arg2, ArgReg)
			binop = BinopNum(binop.value - BinopNum.MOV_NEG.value)
			assert 0 <= binop.value <= BinopNum.MOV.value
			assert binop not in (BinopNum.CMP, BinopNum.TEST)
			force_d = True

		binop_form = (binop.value < 8) or (binop == BinopNum.TEST)

		if binop_form:
			if binop == BinopNum.CMP:
				assert not d
				assert not force_d
				force_no_d = True
			elif binop == BinopNum.TEST:
				assert not isinstance(arg2, ArgImm6) # not supported
				assert not isinstance(arg2, ArgZextReg) # not supported
				assert not d
				assert (not force_no_d) or isinstance(arg2, ArgSextReg)
				force_no_d = False
				force_d = True
				binop = BinopNum.CMP

			assert not (force_d and force_no_d)

			prefix = 1 if self.wide else 2 | (reg1&1)
			enc = (prefix << 14) | (binop.value << 11) | (m << 10) | (((reg1&6)|(d or force_d)) << 7) | (z << 6) | arg2.encode(is_dest=d, T=T, extra_dest=extra)
			return [enc] + extra
		else:
			o = 1

			assert not isinstance(arg2, ArgImm6) # Use ArgImm8 for mov

			if isinstance(arg2, ArgImm8):
				assert binop == BinopNum.MOV
				assert (m, d) == (1, 0)
				d, z = 0, 0 # Will be overridden by the immediate data, but d acts as if it is zero
				o = 0
				T = ArgImm8
			elif isinstance(arg2, ArgPCPlusImm8):
				assert binop == BinopNum.MOV
				assert (m, d) == (1, 0)
				m = 0
				d, z = 0, 0 # Will be overridden by the immediate data, but d acts as if it is zero
				o = 0
				T = ArgPCPlusImm8

			assert not (force_d and force_no_d)

			prefix = 1 if self.wide else 2 | (reg1&1)
			#print((prefix, o, m, reg1, d, z, arg2.encode(is_dest=d), type(arg2)))
			enc = (prefix << 12) | (o << 11) | (m << 10) | (((reg1&6)|(d or force_d)) << 7) | (z << 6) | arg2.encode(is_dest=d, T=T, extra_dest=extra)
			return [enc] + extra


class Swap(Instruction):
	def __init__(self, arg1, arg2):
		assert isinstance(arg1, ArgReg)
		assert isinstance(arg2, Arg)
		assert arg1.wide == arg2.wide
		wide = arg1.wide

		super().__init__()
		self.wide = wide
		self.arg1 = arg1
		self.arg2 = arg2

	def execute(self, state):
		# Calculate memory addresses before applying side effects (postincrement/predecrement)
		if isinstance(self.arg1, ArgMem): arg1 = self.arg1.get_value(state)
		if isinstance(self.arg2, ArgMem): arg2 = self.arg2.get_value(state)
		dest1 = self.arg1.get_dest(state)
		dest2 = self.arg2.get_dest(state)

		self.arg1.apply_side_effect(state)
		self.arg2.apply_side_effect(state)

		# Read registers as data after applying side effects, even postincrement
		if not isinstance(self.arg1, ArgMem): arg1 = self.arg1.get_value(state)
		if not isinstance(self.arg2, ArgMem): arg2 = self.arg2.get_value(state)

		arg1 = arg1 & bitmask(self.wide)
		arg2 = arg2 & bitmask(self.wide)

		print("swap(", self.wide, ", ", str(self.arg1), ":", hex(arg1), ", ", str(self.arg2), ":", hex(arg2), ")")

		state.set_dest(dest1, arg2)
		state.set_dest(dest2, arg1)
		state.step_pc(1 + self.arg1.get_extra_words() + self.arg2.get_extra_words())

	def encode(self):
		reg1 = self.arg1.get_reg()
		extra = []
		if isinstance(self.arg2, ArgMemZP):
			# 111111
			# 5432109876543210
			# 001g00rr1ziiiiii	swap [zp], r8
			# 000100rr1ziiiiii	swap [zp], r16

			m, d, z, o = 0, 1, 0, 0

			prefix = 1 if self.wide else 2 | (reg1&1)
			enc = (prefix << 12) | (o << 11) | (m << 10) | (((reg1&6)|d) << 7) | (z << 6) | self.arg2.encode(is_dest=True, T=ArgMemZP, extra_dest=extra)
			return [enc] + extra

		else:
			# 111111
			# 5432109876543210
			# 001g11rr11iiiiii	swap dest8,  r8
			# 000111rr11iiiiii	swap dest16, r16

			assert isinstance(self.arg2, (ArgReg, ArgMem)) # only supported cases so far

			m, d, z, o = 1, 1, 1, 1

			prefix = 1 if self.wide else 2 | (reg1&1)
			enc = (prefix << 12) | (o << 11) | (m << 10) | (((reg1&6)|d) << 7) | (z << 6) | self.arg2.encode(is_dest=True, extra_dest=extra)
			return [enc] + extra


class Shift(Instruction):
	def __init__(self, shiftop, arg1, arg2):
		assert isinstance(shiftop, ShiftopNum)
		assert isinstance(arg1, ArgReg)
		assert isinstance(arg2, Arg)
		self.wide = arg1.wide
		assert not arg2.wide
		assert not (self.wide and isinstance(arg2, ArgRegFlags)) # not supported, decodes wrong

		super().__init__()
		self.shiftop = shiftop
		self.arg1 = arg1
		self.arg2 = arg2

	def adjust(self):
		if isinstance(self.arg2, ArgMemPushPopTop): self.arg2.dir = -1 # pop

	def execute(self, state):
		self.adjust()

		# Calculate memory addresses before applying side effects (postincrement/predecrement)
		if isinstance(self.arg2, ArgMem): arg2 = self.arg2.get_value(state)

		self.arg2.apply_side_effect(state)

		# Read registers as data after applying side effects, even postincrement
		arg1 = self.arg1.get_value(state) # Always a register
		if not isinstance(self.arg2, ArgMem): arg2 = self.arg2.get_value(state)

		arg2 = arg2 & 15

		if   self.shiftop == ShiftopNum.ROR: result = ((arg1 | (arg1 << nbits(self.wide))) >> (arg2 if self.wide else (arg2 & 7))) & bitmask(self.wide)
		elif self.shiftop == ShiftopNum.ROL: result = ((arg1 | (arg1 << nbits(self.wide))) >> ((-arg2) & 15 if self.wide else ((-arg2) & 7))) & bitmask(self.wide)
		elif self.shiftop == ShiftopNum.SAR: result = sext(arg1, pair=self.wide) >> arg2
		elif self.shiftop == ShiftopNum.SHR: result = arg1 >> arg2
		elif self.shiftop == ShiftopNum.SHL: result = arg1 << (arg2 if self.wide else (arg2 & 7))
		else: assert False # not implemented

		result = result & bitmask(self.wide)

		#print("shift(", self.wide, ", ", self.shiftop, ", ", hex(arg1), ", ", hex(arg2), ") =", hex(result))
		print("shift(", self.wide, ", ", self.shiftop, ", ", str(self.arg1), ":", hex(arg1), ", ", str(self.arg2), ":", hex(arg2), ") =", hex(result))

		state.set_dest(self.arg1.get_dest(state), result)
		state.step_pc(1 + self.arg2.get_extra_words())

	def encode(self):
		reg1 = self.arg1.get_reg()
		extra = []

		aaa = self.shiftop.value

		if isinstance(self.arg2, ArgImm6):
			# 111111
			# 5432109876543210
			# 001g11rr01bbiiii	shift r8,  imm4
			# 000111rr01bbiiii	shift r16, imm4
			prefix = 1 if self.wide else 2 | (reg1&1)
			m, d, z = 1, 0, 1
			o = 1

			arg2_enc = self.arg2.encode(T=ArgImm6, extra_dest=extra)
			assert 0 <= arg2_enc <= 15

			if self.shiftop == ShiftopNum.SHL:
				if not self.wide: assert 0 <= arg2_enc <= 7
				arg2_enc = (-arg2_enc) & (15 if self.wide else 7)

			if self.shiftop == ShiftopNum.ROL:
				# Convert rol to ror
				arg2_enc = (-arg2_enc) & (15 if self.wide else 7)
				aaa = ShiftopNum.ROR.value

			assert (aaa & 1) == 0
			bb = aaa >> 1

			enc = (prefix << 12) | (o << 11) | (m << 10) | (((reg1&6)|d) << 7) | (z << 6) | (bb << 4) | arg2_enc
			return [enc] + extra

		else:
			# 111111
			# 5432109876543210
			# 1eaaa1rr11iiiiii	shift r8,   src
			# 01aaa1rr11iiiiii	shift r16,  src
			prefix = 1 if self.wide else 2 | (reg1&1)
			m, d, z = 1, 1, 1
			enc = (prefix << 14) | (aaa << 11) | (m << 10) | (((reg1&6)|d) << 7) | (z << 6) | self.arg2.encode(extra_dest=extra)
			return [enc] + extra


class Mul(Instruction):
	def __init__(self, arg1, arg2):
		assert isinstance(arg1, ArgReg)
		assert isinstance(arg2, Arg)
		self.wide = arg1.wide
		assert not arg2.wide
		assert not (self.wide and isinstance(arg2, ArgRegFlags)) # not supported, decodes wrong

		super().__init__()
		self.arg1 = arg1
		self.arg2 = arg2

	def adjust(self):
		if isinstance(self.arg2, ArgMemPushPopTop): self.arg2.dir = -1 # pop

	def execute(self, state):
		self.adjust()

		# Calculate memory addresses before applying side effects (postincrement/predecrement)
		if isinstance(self.arg2, ArgMem): arg2 = self.arg2.get_value(state)

		self.arg2.apply_side_effect(state)

		# Read registers as data after applying side effects, even postincrement
		arg1 = self.arg1.get_value(state) # Always a register
		if not isinstance(self.arg2, ArgMem): arg2 = self.arg2.get_value(state)

		if isinstance(self.arg2, ArgImm6): arg2 = arg2 & 0x3f
		else:                              arg2 = arg2 & 0xff

		result = arg1 * arg2

		#result = result & bitmask(self.wide)

		print("mul(", self.wide, ", ", str(self.arg1), ":", hex(arg1), ", ", str(self.arg2), ":", hex(arg2), ") =", hex(result))

		state.set_dest(self.arg1.get_dest(state), result)
		reg1 = self.arg1.get_reg()
		regmask = 6 if self.wide else 7
		if not (reg1 & regmask == REG_INDEX_MUL_MSB_DEST & regmask):
			state.set_reg(REG_INDEX_MUL_MSB_DEST, result >> nbits(self.wide))

		state.step_pc(1 + self.arg2.get_extra_words())

	def encode(self):
		reg1 = self.arg1.get_reg()
		extra = []

		assert (reg1&6) != 0 # currently not supported

		if isinstance(self.arg2, ArgImm6):
			# 111111
			# 5432109876543210
			# 001g00rr00iiiiii	mul r8,  imm6
			# 000100rr00iiiiii	mul r16, imm6
			prefix = 1 if self.wide else 2 | (reg1&1)
			m, d, z = 0, 0, 0
			o = 0

			arg2_enc = self.arg2.encode(T=ArgImm6, extra_dest=extra)

			enc = (prefix << 12) | (o << 11) | (m << 10) | (((reg1&6)|d) << 7) | (z << 6) | arg2_enc
			return [enc] + extra

		else:
			# 111111
			# 5432109876543210
			# 001g00rr01iiiiii	mul r8,  src8
			# 0010010000000110
			# 000100rr01iiiiii	mul r16, src8
			prefix = 1 if self.wide else 2 | (reg1&1)
			m, d, z = 0, 0, 1
			o = 0

			arg2_enc = self.arg2.encode(extra_dest=extra)

			enc = (prefix << 12) | (o << 11) | (m << 10) | (((reg1&6)|d) << 7) | (z << 6) | arg2_enc
			print("Mul: enc = ", enc)
			return [enc] + extra


class Jump(Instruction):
	def __init__(self, arg, call=False):
		assert isinstance(arg, Arg)
		assert arg.wide

		super().__init__()
		self.arg = arg
		self.call = call

	def adjust(self):
		if isinstance(self.arg, ArgMemPushPopTop): self.arg.dir = -1 # pop

	def execute(self, state):
		self.adjust()

		if self.call:
			#push_addr  = (state.get_reg(REG_INDEX_SP_GR, pair=True) - 2) & 0xffff
			push_addr  = (state.get_sp(True) - 2) & 0xffff
			step_size = 1 + self.arg.get_extra_words()
			push_value = (state.get_pc() + 2*step_size) & 0xffff
			#state.set_reg(REG_INDEX_SP_GR, push_addr, pair=True)
			state.set_sp(push_addr)
			state.ram_emu.read_mem(push_addr, True)
			state.ram_emu.write_mem(push_addr, push_value, True)

		# Calculate memory addresses before applying side effects (postincrement/predecrement)
		if isinstance(self.arg, ArgMem): arg = self.arg.get_value(state)

		self.arg.apply_side_effect(state)

		# Read registers as data after applying side effects, even postincrement
		if not isinstance(self.arg, ArgMem): arg = self.arg.get_value(state)

		arg = arg & bitmask(True)


		print("call(" if self.call else "jump(", hex(arg), ")")

		state.set_pc(arg)

	def encode(self):
		extra = []
		if isinstance(self.arg, ArgMemZP):
			assert not self.call
			# 111111
			# 5432109876543210
			# 000100000ziiiiii	mov pc, [zp]
			enc = (1 << 12) | self.arg.encode(is_dest=False, T=ArgMemZP, extra_dest=extra)
			return [enc] + extra

		else:
			# 111111
			# 5432109876543210
			# 001000000ciiiiii	mov pc, src
			enc = (1 << 13) | (self.call << 6) | self.arg.encode(is_dest=False, extra_dest=extra)
			return [enc] + extra


class Branch(Instruction):
	def __init__(self, offset, cc=0, taken=None):
		assert -128 <= offset <= 127
		assert 0 <= cc <= 15

		super().__init__()
		self.offset = offset
		self.cc = cc
		if cc in (CCNum.ALWAYS.value, CCNum.CALL.value):
			assert taken != False
			taken = True
		self.taken = taken

	def execute(self, state):
		if self.cc == CCNum.CALL.value:
			#push_addr  = (state.get_reg(REG_INDEX_SP_GR, pair=True) - 2) & 0xffff
			push_addr  = (state.get_sp(True) - 2) & 0xffff
			step_size = 1 # + self.arg.get_extra_words()
			push_value = (state.get_pc() + 2*step_size) & 0xffff
			#state.set_reg(REG_INDEX_SP_GR, push_addr, pair=True)
			state.set_sp(push_addr)
			state.ram_emu.read_mem(push_addr, True)
			state.ram_emu.write_mem(push_addr, push_value, True)

		#print("PC = ", state.pc)
		assert isinstance(self.taken, bool)

		prev_pc = state.get_pc()

		if self.taken: state.step_pc(self.offset, jump=True)
		else: state.step_pc()

		print("branch(", CCNum(self.cc), ",", hex(self.offset), ",", self.taken, "):", hex(prev_pc),"  -> ", hex(state.get_pc()))
		#print("PC = ", state.pc)

	def encode(self):
#	111111
#	5432109876543210
#	0000ccccbbbbbbbb	2^12 branch

		return [(self.cc << 8) | (self.offset & 255)]


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

def rand_arg_mem_imm16(wide):
	return ArgMemImm16(wide, randrange(0x10000))

def rand_arg_imm6(wide):
	return ArgImm6(wide, randrange(-32, 32))

def rand_arg_imm8(wide):
	return ArgImm8(wide, randrange(-128, 128))

def rand_arg_imm16(wide):
	return ArgImm16(wide, randrange(0, 0x100 << (wide*8)))


def rand_arg(wide, is_src=False, d_symmetry=True, zp_ok=True, flags_ok=False):
	if False:
		n = randrange(1 + is_src)
		if n == 0: return rand_arg_mem_imm16(wide)
		elif n == 1: return rand_arg_imm16(wide)
		else: assert False

	if flags_ok and not wide and randrange(10) == 0: return ArgRegFlags()

	n = (not zp_ok) + randrange(7+zp_ok+is_src+(1+d_symmetry)*(is_src and wide))
	if   n == 0: return rand_arg_mem_zp(wide)
	elif n == 1: return rand_arg_mem_r16r8(wide)
	elif n == 2: return rand_arg_mem_r16incdec(wide)
	elif n == 3: return rand_arg_mem_r16imm2(wide)
	elif n == 4: return rand_arg_reg(wide)
	elif n == 5: return ArgRegSP(wide)
	elif n == 6: return rand_arg_mem_imm16(wide)
	elif n == 7: return ArgMemPushPopTop(wide, 0)
	# is_src:
	elif n == 8: return rand_arg_imm16(wide)
	# is_src and wide:
	elif n == 9: return rand_arg_sext_reg(wide)
	# is_src and wide and d_symmetry:
	elif n == 10: return rand_arg_zext_reg(wide)
	else: assert False

def rand_arg_mem(wide, is_src=False, d_symmetry=True, zp_ok=True):
	n = (not zp_ok) + randrange(4+zp_ok)
	if   n == 0: return rand_arg_mem_zp(wide)
	elif n == 1: return rand_arg_mem_r16r8(wide)
	elif n == 2: return rand_arg_mem_r16incdec(wide)
	elif n == 3: return rand_arg_mem_r16imm2(wide)
	elif n == 4: return rand_arg_mem_imm16(wide)
	elif n == 5: return ArgMemPushPopTop(wide, 0)
	else: assert False
