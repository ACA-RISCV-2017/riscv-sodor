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
      val htif_port = (new MemPortIo(data_width=64)).flip
    }

    io.mem_port.req.valid := Bool(false)
    io.mem_port.req.bits.addr := Bits(0)
    io.mem_port.req.bits.data := Bits(0)
    io.mem_port.req.bits.burst_data := Bits(0)
    io.mem_port.req.bits.fcn := M_XRD
    io.mem_port.req.bits.typ := MT_WU
    io.core_port.resp.valid := Bool(false)
    io.core_port.resp.bits.data := Bits(0)
    val req_ready_reg = Reg(init = Bool(true))
    io.core_port.req.ready := req_ready_reg

    val num_bytes_per_cache_line = 64
    val cache_idx_width = 10  // 1024 entries
    val set_associativity = 4 // 4-way set associative

    val num_words_per_cache_line = num_bytes_per_cache_line / 4   // 16
    val num_cache_lines = 1 << cache_idx_width
    val idx_width = log2Up(num_bytes_per_cache_line)              // 6
    val block_width = 8 * num_bytes_per_cache_line                // 512
    val tag_width = conf.xprlen - idx_width - cache_idx_width     // 16
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
    val byte_shift_amt = req_addr(idx_width-1, 0)
    val bit_shift_amt  = byte_shift_amt << 3

    val data_idx = req_addr >> idx_width
    val cache_idx = if (cache_idx_width == 0) {
        Bits(0)
      } else {
        data_idx(cache_idx_width-1, 0)
      }
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

    def write_back_cache_block() = {
      val s_ready :: s_waiting :: Nil = Enum(UInt(), 2)
      val state = Reg(init = s_ready)
      switch (state) {
      is (s_ready) {
        when (io.mem_port.req.ready) {
          // Send write request to main memory and wait
          val waddr = if (cache_idx_width == 0) {
              Cat(tag, Bits(0, idx_width))
            } else {
              Cat(tag, cache_idx, Bits(0, idx_width))
            }
          io.mem_port.req.valid := Bool(true)
          io.mem_port.req.bits.addr := waddr
          for (k <- 0 until 16) {
            io.mem_port.req.bits.burst_data(k) := block(32 * k + 31, 32 * k)
          }
          io.mem_port.req.bits.fcn := M_XWRBURST
          io.mem_port.req.bits.typ := MT_WU
          state := s_waiting
        }
      }
      is (s_waiting) {
        when (io.mem_port.resp.valid) {
          // Flush cache-line to all zero, cache-line is ready for new data
          data_bank(cache_idx) := Bits(0)
          state := s_ready
        }
      }
      }
    }

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
        io.core_port.resp.valid := Bool(true)
        req_ready_reg := Bool(true)
      } .otherwise {
        // cache miss
        req_ready_reg := Bool(false)
        when (valid_bit && dirty_bit) {
          // Need to write-back dirty data first
          write_back_cache_block()
        } .otherwise {
          val s_ready :: s_waiting :: Nil = Enum(UInt(), 2)
          val state = Reg(init = s_ready)
          switch (state) {
          is (s_ready) {
            when (io.mem_port.req.ready) {
              io.mem_port.req.valid := Bool(true)
              io.mem_port.req.bits.addr := Cat(req_addr >> idx_width, Bits(0, idx_width))
              io.mem_port.req.bits.fcn := M_XRD
              io.mem_port.req.bits.typ := MT_WU
              state := s_waiting
            }
          }
          is (s_waiting) {
            when (io.mem_port.resp.valid) {
              val orig_data = io.mem_port.resp.bits.burst_data.toBits
              val wdata = Fill(num_words_per_cache_line, StoreDataGen(req_data, req_typ))
              val wmask = (StoreMask(req_typ) << bit_shift_amt)(block_width-1, 0)
              data_bank(cache_idx) := Cat(
                Bits("b11", 2),
                tag_idx,
                (orig_data & ~wmask) | (wdata & wmask)
              )
              req_ready_reg := Bool(true)
              state := s_ready
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
        req_ready_reg := Bool(true)
      } .otherwise {
        // cache miss
        req_ready_reg := Bool(false)
        when (valid_bit && dirty_bit) {
          // Need to write-back dirty data first
          write_back_cache_block()
        } .otherwise {
          val s_ready :: s_waiting :: Nil = Enum(UInt(), 2)
          val state = Reg(init = s_ready)
          switch (state) {
          is (s_ready) {
            when (io.mem_port.req.ready) {
              io.mem_port.req.valid := Bool(true)
              io.mem_port.req.bits.addr := Cat(req_addr >> idx_width, Bits(0, idx_width))
              io.mem_port.req.bits.fcn := M_XRD
              io.mem_port.req.bits.typ := MT_WU
              state := s_waiting
            }
          }
          is (s_waiting) {
            when (io.mem_port.resp.valid) {
              val orig_data = io.mem_port.resp.bits.burst_data.toBits
              data_bank(cache_idx) := Cat(
                Bits("b10", 2),
                tag_idx,
                orig_data
              )
              io.core_port.resp.bits.data := LoadDataGen(orig_data >> bit_shift_amt, req_typ)
              io.core_port.resp.valid := Bool(true)
              req_ready_reg := Bool(true)
              state := s_ready
            }
          }
          }
        }
      }
    }

    // HTIF
    val htif_resp_valid = Reg(Bool())
    val htif_resp_rdata = Reg(Bits(width = 64))
    io.htif_port.resp.valid := htif_resp_valid
    io.htif_port.resp.bits.data := htif_resp_rdata
    htif_resp_valid := Bool(false)
    when (io.htif_port.req.valid) {
      val req_addr = io.htif_port.req.bits.addr
      val req_data = io.htif_port.req.bits.data
      val byte_shift_amt = req_addr(idx_width-1, 0)
      val bit_shift_amt  = byte_shift_amt << 3
      val data_idx = req_addr >> idx_width
      val cache_idx = if (cache_idx_width == 0) {
          Bits(0)
        } else {
          data_idx(cache_idx_width-1, 0)
        }
      val tag_idx = (data_idx >> cache_idx_width)(tag_width-1, 0)
      val line = data_bank(cache_idx)
      val block = line(block_width-1, 0)
      val tag = (line >> block_width)(tag_width-1, 0)
      val flag = (line >> (block_width + tag_width))(flag_width-1, 0)
      val dirty_bit = Bool(flag(0))
      val valid_bit = Bool(flag(1))
      when (valid_bit && tag === tag_idx) {
        when (io.htif_port.req.bits.fcn === M_XWR) {
          assert(req_addr(2, 0) === Bits(0))
          val wdata = Cat(
            Bits("b11", 2),
            Bits(0, tag_width),
            Fill(num_bytes_per_cache_line / 8, req_data)
          )
          val wmask = Cat(
            Bits("b11", 2),
            Bits(0, tag_width),
            (Fill(64, Bits(1, 1)) << bit_shift_amt)(block_width-1, 0)
          )
          data_bank.write(cache_idx, wdata, wmask)
        } .otherwise {
          htif_resp_valid := Bool(true)
          htif_resp_rdata := (block >> bit_shift_amt)(63, 0)
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
