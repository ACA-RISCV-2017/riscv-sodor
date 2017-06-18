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
    val cache_idx_width   = 8   // 256 entries
    val set_associativity = 4   // 4-way set associative, total 1024 cache slot

    val num_words_per_cache_line = num_bytes_per_cache_line / 4   // 16
    val num_cache_sets = 1 << cache_idx_width
    val idx_width = log2Up(num_bytes_per_cache_line)              // 6
    val block_width = 8 * num_bytes_per_cache_line                // 512
    val tag_width = conf.xprlen - idx_width - cache_idx_width     // 16
    val flag_width = 2 // valid and dirty bit
    val num_bits_per_cache_line = flag_width + tag_width + block_width
    val data_bank = Array.fill(set_associativity) {
        Mem(Bits(width = num_bits_per_cache_line),
        num_cache_sets,
        seqRead = false
      )
    }
    val cache_line_age = Reg(
      Vec.fill(num_cache_sets) {
        Vec.fill(set_associativity) {
          UInt(width = log2Up(set_associativity))
        }
      }
    )

    val cache_not_init = Reg(init = Bool(true))
    when (cache_not_init) {
      cache_not_init := Bool(false)
      for (i <- 0 until set_associativity) {
        for (j <- 0 until num_cache_sets) {
          data_bank(i)(j) := Bits(0)
          cache_line_age(j)(i) := UInt(i)
        }
      }
    }

    val req_valid      = io.core_port.req.valid
    val req_addr       = io.core_port.req.bits.addr
    val req_data       = io.core_port.req.bits.data
    val req_fcn        = io.core_port.req.bits.fcn
    val req_typ        = io.core_port.req.bits.typ
    val byte_shift_amt = req_addr(idx_width-1, 0)
    val bit_shift_amt  = byte_shift_amt << 3
    val cache_idx = if (cache_idx_width == 0) {
        Bits(0)
      } else {
        (req_addr >> idx_width)(cache_idx_width-1, 0)
      }
    val tag_idx = (req_addr >> (idx_width + cache_idx_width))(tag_width-1, 0)
    // addr = [tag_idx, cache_idx, byte_shift_amt]

    val cache_hit_vec = Vec.fill(set_associativity) { Bool() }
    val cache_hit_line = Reg(Bits(width = log2Up(set_associativity)))
    for (i <- 0 until set_associativity) {
      val data = data_bank(i)
      val line = data(cache_idx)
      val block = line(block_width-1, 0)
      val tag = (line >> block_width)(tag_width-1, 0)
      val flag = (line >> (block_width + tag_width))(flag_width-1, 0)
      val dirty_bit = Bool(flag(0))
      val valid_bit = Bool(flag(1))
      when (valid_bit && tag === tag_idx) {
        cache_hit_vec(i) := Bool(true)
        cache_hit_line := Bits(i)
      } .otherwise {
        cache_hit_vec(i) := Bool(false)
      }
    }

    val lru = cache_line_age(cache_idx)
    def is_update_line(lineno: Int) : Bool = {
      lru(lineno) === UInt(0)
    }

    def lru_update_line(lineno: UInt) = {
      lru(lineno) := UInt(set_associativity - 1)
      for (i <- 0 until set_associativity) {
        when (lru(i) > lru(lineno)) {
          lru(i) := lru(i) - UInt(1)
        }
      }
    }

    // FIXME: HACKY CODE!!!
    val processing_outstanding_miss = Reg(init = Bool(false))
    val cache_hit = cache_hit_vec.toBits != Bits(0)
    when (req_valid) {
    when (cache_hit) {
      // cache hit
      when (req_fcn === M_XWR) {
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
        for (i <- 0 until set_associativity) {
          when (cache_hit_vec(i)) {
            data_bank(i).write(cache_idx, wdata, wmask)
          }
        }
      } .otherwise {
        for (i <- 0 until set_associativity) {
          when (cache_hit_vec(i)) {
            val data = data_bank(i)
            val line = data(cache_idx)
            val block = line(block_width-1, 0)
            io.core_port.resp.bits.data := LoadDataGen(block >> bit_shift_amt, req_typ)
          }
        }
      }
      lru_update_line(cache_hit_line)
      io.core_port.resp.valid := Bool(true)
      req_ready_reg := Bool(true)
    }
    when (~cache_hit || processing_outstanding_miss) {
      // cache miss
      processing_outstanding_miss := Bool(true)
      val need_write_back = Reg(Bool())
      val wb_addr = Reg(Bits())
      val wb_data = Reg(Vec.fill(16) { Bits(width = 32) })

      val s_ready :: s_read_block :: s_write_block :: Nil = Enum(UInt(), 3)
      val state = Reg(init = s_ready)
      switch (state) {
      is (s_ready) {
        when (io.mem_port.req.ready) {
          // linefill: read new cache line (block)
          io.mem_port.req.valid := Bool(true)
          io.mem_port.req.bits.addr := Cat(req_addr >> idx_width, Bits(0, idx_width))
          io.mem_port.req.bits.fcn := M_XRD
          io.mem_port.req.bits.typ := MT_WU
          need_write_back := Bool(false)
          for (i <- 0 until set_associativity) {
            when (is_update_line(i)) {
              val data = data_bank(i)
              val line = data(cache_idx)
              val block = line(block_width-1, 0)
              val tag = (line >> block_width)(tag_width-1, 0)
              val flag = (line >> (block_width + tag_width))(flag_width-1, 0)
              val dirty_bit = Bool(flag(0))
              val valid_bit = Bool(flag(1))
              when (valid_bit && dirty_bit) {
                // we are going to replace a dirty block, so remember to write
                // back dirty block after linefill
                need_write_back := Bool(true)
                if (cache_idx_width == 0) {
                  wb_addr := Cat(tag, Bits(0, idx_width))
                } else {
                  wb_addr := Cat(tag, cache_idx, Bits(0, idx_width))
                }
                for (k <- 0 until 16) {
                  wb_data(k) := block(32 * k + 31, 32 * k)
                }
              }
            }
          }
          state := s_read_block
        }
      }
      is (s_read_block) {
        when (io.mem_port.resp.valid) {
          // linefill data ready: fill in the new line and prepare to write back
          // dirty line (if there is any)
          val orig_data = io.mem_port.resp.bits.burst_data.toBits
          val wdata = Fill(num_words_per_cache_line, StoreDataGen(req_data, req_typ))
          val wmask = (StoreMask(req_typ) << bit_shift_amt)(block_width-1, 0)
          for (i <- 0 until set_associativity) {
            when (is_update_line(i)) {
              when (req_fcn === M_XWR) {
                data_bank(i)(cache_idx) := Cat(
                  Bits("b11", 2),
                  tag_idx,
                  (orig_data & ~wmask) | (wdata & wmask)
                )
              } .otherwise {
                data_bank(i)(cache_idx) := Cat(
                  Bits("b10", 2),
                  tag_idx,
                  orig_data
                )
              }
            }
          }
          lru_update_line(cache_hit_line)
          when (need_write_back) {
            state := s_write_block
          } .otherwise {
            io.core_port.resp.valid := Bool(true)
            io.core_port.resp.bits.data := LoadDataGen(orig_data >> bit_shift_amt, req_typ)
            processing_outstanding_miss := Bool(false)
            req_ready_reg := Bool(true)
            state := s_ready
          }
        }
      }
      is (s_write_block) {
        when (io.mem_port.req.ready) {
          // Send write request to main memory and wait
          io.mem_port.req.valid := Bool(true)
          io.mem_port.req.bits.addr := wb_addr
          for (k <- 0 until 16) {
            io.mem_port.req.bits.burst_data(k) := wb_data(k)
          }
          io.mem_port.req.bits.fcn := M_XWRBURST
          io.mem_port.req.bits.typ := MT_WU
          processing_outstanding_miss := Bool(false)
          req_ready_reg := Bool(true)
          state := s_ready
        }
      }
      }
    }
    } // req_valid === Bool(true)

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
      val cache_idx = if (cache_idx_width == 0) {
          Bits(0)
        } else {
          (req_addr >> idx_width)(cache_idx_width-1, 0)
        }
      val tag_idx = (req_addr >> (idx_width + cache_idx_width))(tag_width-1, 0)
      for (i <- 0 until set_associativity) {
        val line = data_bank(i)(cache_idx)
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
            data_bank(i).write(cache_idx, wdata, wmask)
          } .otherwise {
            htif_resp_valid := Bool(true)
            htif_resp_rdata := (block >> bit_shift_amt)(63, 0)
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
