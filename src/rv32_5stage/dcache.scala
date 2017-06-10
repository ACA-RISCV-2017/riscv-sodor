package object AcaCustom
{
	import Chisel._
	import Node._
	import Sodor.Constants._
	import Common._

	class DCacheInterface()(implicit conf: SodorConfiguration) extends Module
	{
		val io = new Bundle {
			val core_port = (new MemPortIo(data_width=conf.xprlen)).flip
			val mem_port = new MemPortIo(data_width=conf.xprlen)
		}

                io.mem_port.req.valid := Bool(false)
                io.mem_port.req.bits.addr := Bits(0)
                io.mem_port.req.bits.data := Bits(0)
                io.mem_port.req.bits.fcn := M_XRD
                io.mem_port.req.bits.typ := MT_WU
                io.core_port.resp.valid := Bool(false)
                io.core_port.resp.bits.data := Bits(0)
                io.core_port.req.ready := Bool(true)

                val num_bytes_per_cache_line = 4
                val cache_idx_width = 10

                val num_words_per_cache_line = num_bytes_per_cache_line / 4
                val num_cache_lines = 1 << cache_idx_width
                val idx_lsb = log2Up(num_bytes_per_cache_line) // 2
                val block_width = 8 * num_bytes_per_cache_line
                val tag_width = conf.xprlen - idx_lsb - cache_idx_width
                val flag_width = 2 // valid and dirty bit
                val num_bits_per_cache_line = flag_width + tag_width + block_width
                val data_bank = Mem(Bits(width = num_bits_per_cache_line),
                                    num_cache_lines,
                                    seqRead = false
                                    )

                val req_valid      = io.core_port.req.valid
                val req_addr       = io.core_port.req.bits.addr
                val req_data       = io.core_port.req.bits.data
                val req_fcn        = io.core_port.req.bits.fcn
                val req_typ        = io.core_port.req.bits.typ
                val byte_shift_amt = io.core_port.req.bits.addr(idx_lsb-1, 0)
                val bit_shift_amt  = Cat(byte_shift_amt, UInt(0, 3))

                val data_idx = req_addr >> idx_lsb
                val cache_idx = data_idx(cache_idx_width-1, 0)
                val tag_idx = (data_idx >> cache_idx_width)(tag_width-1, 0)
                // addr = [tag_idx, cache_idx, byte_shift_amt]

                val cache_not_init = Reg(init = Bool(true))
                when (cache_not_init) {
                  cache_not_init := Bool(false)
                  for (i <- 0 until num_cache_lines) {
                    data_bank(i) := Bits(0)
                  }
                }

                val line = data_bank(cache_idx)
                val block = line(block_width-1, 0)
                val tag = (line >> block_width)(tag_width-1, 0)
                val flag = (line >> (block_width + tag_width))(flag_width-1, 0)
                val dirty_bit = Bool(flag(0))
                val valid_bit = Bool(flag(1))

                // write-back cache
                when (req_valid && req_fcn === M_XWR) {
                  // write access
                  when (valid_bit && tag === tag_idx) {
                    // cache hit
                    val wdata = Cat(
                      Bits("b11", 2), // set valid bit and dirty bit
                      Bits(0, tag_width),
                      Fill(num_words_per_cache_line, StoreDataGen(req_data, req_typ))
                    )
                    val wmask = Cat(
                      Bits("b11", 2),
                      Bits(0, tag_width),
                      (StoreMask(req_typ) << bit_shift_amt)(block_width-1, 0)
                    )
                    data_bank.write(cache_idx, wdata, wmask)
                    // io.core_port.resp.bits.data := req_data
                    io.core_port.resp.valid := Bool(true)
                    io.core_port.req.ready := Bool(true)

                    /* FIXME */
                    // Something is very wrong, write-back cache will exception
                    // write-through cache seems okay. test `fence.i` always crash
                    // =(
                    io.core_port.resp.valid := Bool(false)
                    io.core_port.req.ready := Bool(false)
                    val new_block = data_bank(cache_idx)
                    // Need to write-back dirty data
                    val s_ready :: s_waiting :: Nil = Enum(UInt(), 2)
                    val state = Reg(init = s_ready)
                    switch (state) {
                      is (s_ready) {
                        io.mem_port.req.valid := Bool(true)
                        io.mem_port.req.bits.addr := Cat(tag_idx, cache_idx, Bits(0, 2))
                        io.mem_port.req.bits.data := new_block
                        io.mem_port.req.bits.fcn := M_XWR
                        io.mem_port.req.bits.typ := MT_WU
                        state := s_waiting
                      }
                      is (s_waiting) {
                        when (io.mem_port.resp.valid) {
                          io.mem_port.req.valid := Bool(false)
                          data_bank(cache_idx) := Bits(0)
                          io.core_port.resp.valid := Bool(true)
                          io.core_port.req.ready := Bool(true)
                          state := s_ready
                        } .otherwise {
                          io.mem_port.req.valid := Bool(true)
                          io.mem_port.req.bits.addr := Cat(tag_idx, cache_idx, Bits(0, 2))
                          io.mem_port.req.bits.data := new_block
                          io.mem_port.req.bits.fcn := M_XWR
                          io.mem_port.req.bits.typ := MT_WU
                        }
                      }
                    }


                  } .otherwise {
                    // cache miss
                    io.core_port.req.ready := Bool(false)
                    when (valid_bit && dirty_bit) {
                      // Need to write-back dirty data first
                      val s_ready :: s_waiting :: Nil = Enum(UInt(), 2)
                      val state = Reg(init = s_ready)
                      switch (state) {
                        is (s_ready) {
                          io.mem_port.req.valid := Bool(true)
                          io.mem_port.req.bits.addr := Cat(tag, cache_idx, Bits(0, 2))
                          io.mem_port.req.bits.data := block
                          io.mem_port.req.bits.fcn := M_XWR
                          io.mem_port.req.bits.typ := MT_WU
                          state := s_waiting
                        }
                        is (s_waiting) {
                          when (io.mem_port.resp.valid) {
                            io.mem_port.req.valid := Bool(false)
                            data_bank(cache_idx) := Bits(0)
                            state := s_ready
                          } .otherwise {
                            io.mem_port.req.valid := Bool(true)
                            io.mem_port.req.bits.addr := Cat(tag, cache_idx, Bits(0, 2))
                            io.mem_port.req.bits.data := block
                            io.mem_port.req.bits.fcn := M_XWR
                            io.mem_port.req.bits.typ := MT_WU
                          }
                        }
                      }
                    } .otherwise {
                      val s_ready :: s_waiting :: Nil = Enum(UInt(), 2)
                      val state = Reg(init = s_ready)
                      switch (state) {
                        is (s_ready) {
                          io.mem_port.req.valid := Bool(true)
                          io.mem_port.req.bits.addr := Cat(req_addr >> 2, Bits(0, 2))
                          io.mem_port.req.bits.fcn := M_XRD
                          io.mem_port.req.bits.typ := MT_WU
                          state := s_waiting
                        }
                        is (s_waiting) {
                          when (io.mem_port.resp.valid) {
                            io.mem_port.req.valid := Bool(false)
                            io.mem_port.req.bits.addr := Cat(req_addr >> 2, Bits(0, 2))
                            io.mem_port.req.bits.fcn := M_XRD
                            io.mem_port.req.bits.typ := MT_WU
                            val orig_data = io.mem_port.resp.bits.data
                            val wdata = Fill(num_words_per_cache_line, StoreDataGen(req_data, req_typ))
                            val wmask = (StoreMask(req_typ) << bit_shift_amt)(block_width-1, 0)
                            data_bank(cache_idx) := Cat(
                              // Bits("b11", 2),
                              Bits("b10", 2),
                              tag_idx,
                              (orig_data & ~wmask) | (wdata & wmask)
                            )
                            // io.core_port.resp.bits.data := req_data
                            // io.core_port.resp.valid := Bool(true)
                            // io.core_port.req.ready := Bool(true)
                            state := s_ready
                          } .otherwise {
                            io.mem_port.req.valid := Bool(true)
                            io.mem_port.req.bits.addr := Cat(req_addr >> 2, Bits(0, 2))
                            io.mem_port.req.bits.fcn := M_XRD
                            io.mem_port.req.bits.typ := MT_WU
                          }
                        }
                      }
                    }
                  }
                } .elsewhen (req_valid && req_fcn === M_XRD) {
                  // read access
                  when (valid_bit && tag === tag_idx) {
                    // cache hit
                    io.core_port.resp.bits.data := LoadDataGen(block >> bit_shift_amt, req_typ)
                    io.core_port.resp.valid := Bool(true)
                    io.core_port.req.ready := Bool(true)
                  } .otherwise {
                    // cache miss
                    io.core_port.req.ready := Bool(false)
                    when (valid_bit && dirty_bit) {
                      // Need to write-back dirty data first
                      val s_ready :: s_waiting :: Nil = Enum(UInt(), 2)
                      val state = Reg(init = s_ready)
                      switch (state) {
                        is (s_ready) {
                          io.mem_port.req.valid := Bool(true)
                          io.mem_port.req.bits.addr := Cat(tag, cache_idx, Bits(0, 2))
                          io.mem_port.req.bits.data := block
                          io.mem_port.req.bits.fcn := M_XWR
                          io.mem_port.req.bits.typ := MT_WU
                          state := s_waiting
                        }
                        is (s_waiting) {
                          when (io.mem_port.resp.valid) {
                            io.mem_port.req.valid := Bool(false)
                            data_bank(cache_idx) := Bits(0)
                            state := s_ready
                          } .otherwise {
                            io.mem_port.req.valid := Bool(true)
                            io.mem_port.req.bits.addr := Cat(tag, cache_idx, Bits(0, 2))
                            io.mem_port.req.bits.data := block
                            io.mem_port.req.bits.fcn := M_XWR
                            io.mem_port.req.bits.typ := MT_WU
                          }
                        }
                      }
                    } .otherwise {
                      val s_ready :: s_waiting :: Nil = Enum(UInt(), 2)
                      val state = Reg(init = s_ready)
                      switch (state) {
                        is (s_ready) {
                          when (io.mem_port.req.ready) {
                            io.mem_port.req.valid := Bool(true)
                            io.mem_port.req.bits.addr := Cat(req_addr >> 2, Bits(0, 2))
                            io.mem_port.req.bits.fcn := M_XRD
                            io.mem_port.req.bits.typ := MT_WU
                            state := s_waiting
                          }
                        }
                        is (s_waiting) {
                          when (io.mem_port.resp.valid) {
                            val orig_data = io.mem_port.resp.bits.data
                            io.mem_port.req.valid := Bool(false)
                            io.mem_port.req.bits.addr := Cat(req_addr >> 2, Bits(0, 2))
                            io.mem_port.req.bits.fcn := M_XRD
                            io.mem_port.req.bits.typ := MT_WU
                            data_bank(cache_idx) := Cat(
                              Bits("b10", 2),
                              tag_idx,
                              orig_data
                            )
                            io.core_port.resp.bits.data := LoadDataGen(orig_data >> bit_shift_amt, req_typ)
                            io.core_port.resp.valid := Bool(true)
                            io.core_port.req.ready := Bool(true)
                            state := s_ready
                          } .otherwise {
                            io.mem_port.req.valid := Bool(true)
                            io.mem_port.req.bits.addr := Cat(req_addr >> 2, Bits(0, 2))
                            io.mem_port.req.bits.fcn := M_XRD
                            io.mem_port.req.bits.typ := MT_WU
                          }
                        }
                      }
                    }
                  }
                }
	}

	class NoDCache()(implicit conf: SodorConfiguration) extends Module
	{
		val io = new Bundle {
			val core_port = (new MemPortIo(data_width=conf.xprlen)).flip
			val mem_port = new MemPortIo(data_width=conf.xprlen)
		}
		io.mem_port <> io.core_port
	}

	class NoDCache2()(implicit conf: SodorConfiguration) extends Module
	{
		val io = new Bundle {
			val core_port = (new MemPortIo(data_width=conf.xprlen)).flip
			val mem_port = new MemPortIo(data_width=conf.xprlen)
		}
		// Extract desired data from burst
		val burst_len = 64 // 64 byte per line
		val burst_len_bit = 6 // 2^6 = 64
		val word_len = 4 // 4 byte per word
		val word_len_bit = 2 // 2^2 = 4

		val req_addr = io.core_port.req.bits.addr
		val burst_data = io.mem_port.resp.bits.burst_data

		val word_idx_in_burst = req_addr(burst_len_bit - 1, word_len_bit)
		val word_data = 
			Mux1H(UIntToOH(word_idx_in_burst, width=(burst_len / word_len)), burst_data)

		val byte_idx_in_word = req_addr(word_len_bit - 1, 0)
		val read_data = LoadDataGen(word_data >> (byte_idx_in_word << 3), io.core_port.req.bits.typ)

		// Wiring
		io.mem_port.req.valid <> io.core_port.req.valid
		io.mem_port.req.ready <> io.core_port.req.ready
		io.mem_port.req.bits.addr <> io.core_port.req.bits.addr
		io.mem_port.req.bits.data <> io.core_port.req.bits.data
		io.mem_port.req.bits.fcn <> io.core_port.req.bits.fcn
		io.mem_port.req.bits.typ <> io.core_port.req.bits.typ

		io.core_port.resp.valid <> io.mem_port.resp.valid
		io.core_port.resp.bits.data := read_data
		io.core_port.resp.bits.burst_data := Bits(0)
		
	}

        type DCache = DCacheInterface
}
