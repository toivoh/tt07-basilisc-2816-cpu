# SPDX-FileCopyrightText: © 2024 Toivo Henningsson
# SPDX-License-Identifier: MIT

from random import randrange

# Duplicated from common.vh
TX_HEADER_READ_16  = 0
TX_HEADER_WRITE_8  = 2
TX_HEADER_WRITE_16 = 3
RX_SB_READ_16 = 1

class RAMEmulator:
	def __init__(self, mem, IO_BITS=2, PAYLOAD_CYCLES=8, delay=0):
		self.IO_BITS = IO_BITS
		self.PAYLOAD_CYCLES = PAYLOAD_CYCLES
		self.mem = mem
		self.delay = delay

		self.WORD_SIZE = self.PAYLOAD_CYCLES * self.IO_BITS
		self.tx_mask = (1 << self.IO_BITS) - 1

		self.addr = 0

		self.rx_counter = 0
		self.rx_header = 0
		self.rx_buffer = 0

		self.tx_buffer = 0
		self.tx_delay_buffer = 0

	def step(self, rx):
		"""Takes rx, returns tx"""

		if self.rx_counter == 0:
			if (rx&1) != 0:
				self.rx_counter = 1
		elif self.rx_counter == 1:
			self.rx_header = rx
			self.rx_counter += 1
			#print("rx_header = ", self.rx_header)
		else:
			self.rx_buffer = (self.rx_buffer | (rx << self.WORD_SIZE))>> self.IO_BITS
			self.rx_counter += 1

			if self.rx_counter == self.PAYLOAD_CYCLES + 2:
				# Whole payload received
				self.rx_counter = 0

				if self.rx_header == TX_HEADER_READ_16:
					self.addr = self.rx_buffer

					#data = self.mem[self.addr>>1] # Ignore LSB of addr for now when reading 16 bits
					if (self.addr & 1) == 0:
						data = self.mem[self.addr>>1]
					else:
						data = ((self.mem[self.addr>>1] >> 8) & 0xff) | ((self.mem[((self.addr + 1)&0xffff)>>1] << 8) & 0xff00)

					self.tx_buffer = (data << (2*self.IO_BITS)) | RX_SB_READ_16 # including start bits
					print("Read16 mem[", hex(self.addr), "] = ", hex(data))

				elif self.rx_header == TX_HEADER_WRITE_16:
					print("Write16 mem[", hex(self.addr), "] = ", hex(self.rx_buffer))

					#self.mem[self.addr>>1] = self.rx_buffer & 0xffff
					data = self.rx_buffer & 0xffff
					if (self.addr & 1) == 0:
						self.mem[self.addr>>1] = data
					else:
						self.mem[self.addr>>1] = (self.mem[self.addr>>1] & 0x00ff) | ((data << 8) & 0xff00)
						ind = ((self.addr + 1)&0xffff)>>1
						self.mem[ind]         = (self.mem[ind]           & 0xff00) | ((data >> 8) & 0x00ff)

					self.addr = (self.addr + 2) & 0xffff
					# TODO: ack?
				elif self.rx_header == TX_HEADER_WRITE_8:
					addr_lsb = self.addr & 1
					data = self.rx_buffer & 0xff
					#data = (self.rx_buffer >> (8*addr_lsb)) & 0xff
					print("Write8 mem[", hex(self.addr), "] = ", hex(data))
					#addr_lsb = (self.rx_buffer >> 8) & 1
					if addr_lsb == 0: self.mem[self.addr>>1] = (self.mem[self.addr>>1] & 0xff00) | data
					else:             self.mem[self.addr>>1] = (self.mem[self.addr>>1] & 0x00ff) | (data << 8)
					#self.addr = (self.addr + addr_lsb) & 0xffff
					self.addr = (self.addr + 1) & 0xffff
					# TODO: ack?

		tx0 = self.tx_buffer & self.tx_mask
		self.tx_buffer >>= self.IO_BITS

		self.tx_delay_buffer |= (tx0 << (self.IO_BITS*self.delay))
		tx = self.tx_delay_buffer & self.tx_mask
		self.tx_delay_buffer >>= self.IO_BITS

		return tx


class MockRAMEmulator:
	def __init__(self, IO_BITS=2, PAYLOAD_CYCLES=8, delay=0, alt_responder=None):
		self.IO_BITS = IO_BITS
		self.PAYLOAD_CYCLES = PAYLOAD_CYCLES
		self.delay = delay
		self.alt_responder = alt_responder

		self.WORD_SIZE = self.PAYLOAD_CYCLES * self.IO_BITS
		self.tx_mask = (1 << self.IO_BITS) - 1

		self.tx_buffer = 0
		self.tx_delay_buffer = 0
		self.alt = False

		self.reset(False)

	def reset(self, check=True):
		if check:
			#print((self.done(), self.rx_counter, self.seq_index, len(self.sequence)))
			#assert self.tx_buffer == 0
			assert self.rx_counter == 0
			assert self.seq_index == len(self.sequence)

		self.rx_counter = 0
		self.rx_header = 0
		self.rx_buffer = 0

		#self.tx_buffer = 0

		self.sequence = []
		self.seq_index = 0

	def done(self):
		return self.tx_buffer == 0 and self.seq_index == len(self.sequence)

	def read_mem(self, addr, wide):
		data = randrange(0x10000)
		#addr &= ~1 # lsb of addr is just ignored for reads for now
		self.sequence.append(((TX_HEADER_READ_16, addr, 0xffff), (RX_SB_READ_16, data)))
		print("Read mem[", hex(addr), "] =", hex(data))

		if wide: return data
		else: return data & 0xff
		#elif (addr&1)==0: return data & 0xff
		#else: return (data >> 8) & 0xff

	def write_mem(self, addr, value, wide):
		assert len(self.sequence) >= 1
		tx_last, rx_last = self.sequence[-1]
		assert tx_last[0:2] == (TX_HEADER_READ_16, addr)

		if wide:
			self.sequence.append(((TX_HEADER_WRITE_16, value, 0xffff), None))
		else:
			#shift = (addr&1)*8
			#self.sequence.append(((TX_HEADER_WRITE_8, value << shift, 0xff << shift), None))
			self.sequence.append(((TX_HEADER_WRITE_8, value, 0xff), None))
		print("Write mem16[" if wide else "Write mem8[", hex(addr), "] =", hex(value))

	def step(self, rx, alt=False, alt_arg=None):
		"""Takes rx, returns tx"""

		if self.rx_counter == 0:
			if (rx&1) != 0:
				self.rx_counter = 1
		elif self.rx_counter == 1:
			self.rx_header = rx
			self.alt = alt
			self.alt_arg = alt_arg
			self.rx_counter += 1
			#print("rx_header = ", self.rx_header)
		else:
			self.rx_buffer = (self.rx_buffer | (rx << self.WORD_SIZE))>> self.IO_BITS
			self.rx_counter += 1

			if self.rx_counter == self.PAYLOAD_CYCLES + 2:
				# Whole payload received
				self.rx_counter = 0

				if self.alt:
					tx_response = self.alt_responder(self.rx_header, self.rx_buffer, self.alt_arg)
				else:
					assert self.seq_index < len(self.sequence)
					rx_expect, tx_response = self.sequence[self.seq_index]
					rx_header_expect, rx_data_expect, rx_mask = rx_expect
					self.seq_index += 1
					assert (self.rx_header, self.rx_buffer & rx_mask) == (rx_header_expect, rx_data_expect & rx_mask)
					#print((self.rx_header, self.rx_buffer & rx_mask), "==", (rx_header_expect, rx_data_expect & rx_mask))

				if tx_response != None:
					assert self.tx_buffer == 0 # so that we can replace it
					header, data = tx_response
					self.tx_buffer = (data << (2*self.IO_BITS)) | header

		tx0 = self.tx_buffer & self.tx_mask
		self.tx_buffer >>= self.IO_BITS

		self.tx_delay_buffer |= (tx0 << (self.IO_BITS*self.delay))
		tx = self.tx_delay_buffer & self.tx_mask
		self.tx_delay_buffer >>= self.IO_BITS

		return tx
