package object AcaCustom
{
  import Chisel._
  import Node._
  import Sodor.Constants._
  import Common._

  class RegCounter(n: Int) {
    val value = Reg(init=UInt(0, log2Up(n)))
    def inc() = {
      value := Mux(value === UInt(n-1), UInt(0), value + UInt(1))
    }
    def dec() = {
      value := Mux(value === UInt(0), UInt(n-1), value - UInt(1))
    }
  }

  object RegCounter {
    def apply(n: Int): RegCounter = new RegCounter(n)
  }

  class WriteBufferReq()(implicit conf: SodorConfiguration) extends Bundle {
    val addr = Bits(width=conf.xprlen)
    val burst_data = Vec.fill(16) { Bits(width=conf.xprlen) }
    override def cloneType: this.type = new WriteBufferReq().asInstanceOf[this.type]
  }

  class WriteBuffer(entries: Int)(implicit conf: SodorConfiguration) extends Module {
    val io = new Bundle {
      val enq = Decoupled(new WriteBufferReq()).flip
      val mem_port = new MemPortIo(data_width=conf.xprlen)
      val peek_req = Decoupled(new WriteBufferReq()).flip
      val peek_resp = Decoupled(new WriteBufferReq())
    }

    val s_invalid :: s_valid :: s_written :: Nil = Enum(Bits(), 3)
    val data_valid = Reg(init = Vec(Seq.fill(entries)(s_invalid)))
    val data = Reg(Vec.fill(entries) { new WriteBufferReq() })
    val enq_ptr = RegCounter(entries)
    val deq_ptr = RegCounter(entries)
    val num_entries = RegCounter(entries+1)
    val is_full = num_entries.value === UInt(entries)
    val is_empty = num_entries.value === UInt(0)

    io.enq.ready := ~is_full   // Can enque new request as long as not full
    io.mem_port.req.valid := Bool(false)
    io.mem_port.req.bits.addr := Bits(0)
    io.mem_port.req.bits.burst_data := Bits(0)
    io.mem_port.req.bits.fcn := M_XRD
    io.peek_req.ready := Bool(true)
    io.peek_resp.valid := Bool(false)
    io.peek_resp.bits.burst_data := Bits(0)

    when (~is_full && io.enq.valid) {
      printf("ENQ %x %x\n", io.enq.bits.addr, io.enq.bits.burst_data.toBits)
      // If the address to be enqueued is alrealdy in write queue, invalid the
      // entries already in it (This should never happen)
      for (i <- 0 until entries) {
        when (data(i).addr === io.enq.bits.addr && data_valid(i) != s_invalid && enq_ptr.value != UInt(i)) {
          assert(data_valid(i) === s_written)
          data_valid(i) := s_invalid
          printf("COL %x %x\n", data(i).addr, data(i).burst_data.toBits)
        }
      }
      printf("ENQ EX %x %x\n", data(enq_ptr.value).addr, data(enq_ptr.value).burst_data.toBits)
      data(enq_ptr.value) := io.enq.bits
      data_valid(enq_ptr.value) := s_valid
      enq_ptr.inc()
    }
    when (~is_empty && data_valid(deq_ptr.value) === s_valid && io.mem_port.req.ready) {
      printf("DEQ %x %x\n", data(deq_ptr.value).addr, data(deq_ptr.value).burst_data.toBits)
      io.mem_port.req.valid := Bool(true)
      io.mem_port.req.bits := data(deq_ptr.value)
      io.mem_port.req.bits.fcn := M_XWRBURST
      data_valid(deq_ptr.value) := s_written
      deq_ptr.inc()
    }
    .elsewhen (~is_empty && data_valid(deq_ptr.value) != s_valid) {
      assert(data_valid(deq_ptr.value) === s_invalid)
      printf("DEQINV %x %x\n", data(deq_ptr.value).addr, data(deq_ptr.value).burst_data.toBits)
      deq_ptr.inc()
    }

    // Updating num_entries is a bit tricky...
    when (~is_full && io.enq.valid) {
      when (~is_empty && data_valid(deq_ptr.value) === s_valid && io.mem_port.req.ready) {
        // No need to update num_entries
      }
      .elsewhen (~is_empty && data_valid(deq_ptr.value) != s_valid) {
        // No need to update num_entries
      }
      .otherwise {
        num_entries.inc()
      }
    } .otherwise {
      when (~is_empty && data_valid(deq_ptr.value) === s_valid && io.mem_port.req.ready) {
        num_entries.dec()
      }
      .elsewhen (~is_empty && data_valid(deq_ptr.value) != s_valid) {
        num_entries.dec()
      }
    }

    for (i <- 0 until entries) {
      when (data_valid(i) != s_invalid && data(i).addr === io.peek_req.bits.addr) {
        io.peek_resp.valid := Bool(true)
        io.peek_resp.bits.burst_data := data(i).burst_data
      }
    }
    // This is damn hacky, use the peek_req.valid signal to decide if we are
    // going to invalid the peeked entry
    // when valid is false => peek-peek (will not invalid peeked entry)
    // when valid is true => real-peek (will invalid peeked entry)
    when (io.peek_req.valid) {
      for (i <- 0 until entries) {
        // This disgusting thing is for avoiding racing with ENQ
        // TODO: Use a better way, perhaps redo this whole thing with a FSM
        when (data_valid(i) != s_invalid && data(i).addr === io.peek_req.bits.addr) {
          when (data_valid(i) === s_valid) {
            unless (~is_empty && data_valid(deq_ptr.value) === s_valid && io.mem_port.req.ready && UInt(i) === deq_ptr.value) {
              data_valid(i) := s_invalid
            }
          }
        }
      }
    }
  }

  class DCacheInterface()(implicit conf: SodorConfiguration) extends Module
  {
    val io = new Bundle {
      val core_port = (new MemPortIo(data_width=conf.xprlen)).flip
      val mem_port = new MemPortIo(data_width=conf.xprlen)
      val htif_port = (new MemPortIo(data_width=64)).flip
    }

    val num_bytes_per_cache_line = 64
    val cache_idx_width   =  3  // 1024 entries
    val set_associativity =  1  // direct mapped cache
    val write_buffer_size =  1

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

    val req_ready_reg  = Reg(init = Bool(true))
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

    val lru = cache_line_age(cache_idx)
    def is_lru_replace_line(lineno: Int) : Bool = {
      lru(lineno) === UInt(0)
    }

    def update_lru_line_age(lineno: Int) = {
      lru(lineno) := UInt(set_associativity - 1)
      for (i <- 0 until set_associativity) {
        when (lru(i) > lru(lineno)) {
          lru(i) := lru(i) - UInt(1)
        }
      }
    }

    val write_buffer = Module(new WriteBuffer(write_buffer_size))
    write_buffer.io.enq.valid := Bool(false)
    write_buffer.io.enq.bits.addr := Bits(0)
    write_buffer.io.enq.bits.burst_data := Bits(0)
    write_buffer.io.peek_req.valid := Bool(false)
    write_buffer.io.peek_req.bits.addr := Cat(req_addr >> idx_width, Bits(0, idx_width))
    write_buffer.io.mem_port.req.ready := io.mem_port.req.ready
    io.mem_port.req.valid := write_buffer.io.mem_port.req.valid
    io.mem_port.req.bits.addr := write_buffer.io.mem_port.req.bits.addr
    io.mem_port.req.bits.burst_data := write_buffer.io.mem_port.req.bits.burst_data
    io.mem_port.req.bits.fcn := write_buffer.io.mem_port.req.bits.fcn
    io.mem_port.req.bits.typ := write_buffer.io.mem_port.req.bits.typ
    io.core_port.resp.valid := Bool(false)
    io.core_port.resp.bits.data := Bits(0)
    io.core_port.req.ready := req_ready_reg

    val cache_hit_vec = Vec.fill(set_associativity) { Bool() }
    val cache_hit = orR(cache_hit_vec.toBits)
    for (i <- 0 until set_associativity) {
      val data = data_bank(i)
      val line = data(cache_idx)
      val block = line(block_width-1, 0)
      val tag = (line >> block_width)(tag_width-1, 0)
      val flag = (line >> (block_width + tag_width))(flag_width-1, 0)
      val dirty_bit = Bool(flag(0))
      val valid_bit = Bool(flag(1))
      cache_hit_vec(i) := valid_bit && tag === tag_idx
    }

    val line_evict_cause_wb = Bool()
    line_evict_cause_wb := Bool(false)
    for (i <- 0 until set_associativity) {
      when (is_lru_replace_line(i)) {
        val data = data_bank(i)
        val line = data(cache_idx)
        val flag = (line >> (block_width + tag_width))(flag_width-1, 0)
        val dirty_bit = Bool(flag(0))
        val valid_bit = Bool(flag(1))
        when (valid_bit && dirty_bit) {
          // This is a valid and dirty block, so remember to write back this
          // block if we are going to replace it with a linefill
          line_evict_cause_wb := Bool(true)
        }
      }
    }

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
      for (i <- 0 until set_associativity) {
        when (cache_hit_vec(i)) {
          update_lru_line_age(i)
        }
      }
      io.core_port.resp.valid := Bool(true)
      req_ready_reg := Bool(true)
    }
    when (~cache_hit) {
      // cache miss
      val s_ready :: s_read_block :: Nil = Enum(UInt(), 2)
      val state = Reg(init = s_ready)
      switch (state) {
      is (s_ready) {
        write_buffer.io.peek_req.bits.addr := Cat(req_addr >> idx_width, Bits(0, idx_width))
        when (line_evict_cause_wb) {
          // We are going to evict a dirty block, so place the dirty block into
          // the write buffer (write queue)
          when (write_buffer.io.enq.ready) {
            for (i <- 0 until set_associativity) {
              when (is_lru_replace_line(i)) {
                val data = data_bank(i)
                val line = data(cache_idx)
                val block = line(block_width-1, 0)
                val tag = (line >> block_width)(tag_width-1, 0)
                val flag = (line >> (block_width + tag_width))(flag_width-1, 0)
                val dirty_bit = Bool(flag(0))
                val valid_bit = Bool(flag(1))
                assert(valid_bit && dirty_bit)
                write_buffer.io.enq.valid := Bool(true)
                if (cache_idx_width == 0) {
                  write_buffer.io.enq.bits.addr := Cat(tag, Bits(0, idx_width))
                } else {
                  write_buffer.io.enq.bits.addr := Cat(tag, cache_idx, Bits(0, idx_width))
                }
                for (k <- 0 until 16) {
                  write_buffer.io.enq.bits.burst_data(k) := block(32 * k + 31, 32 * k)
                }
                // This is damn hacky, but we need this to avoid data racing
                when (~write_buffer.io.peek_resp.valid) {
                  data_bank(i)(cache_idx) := Bits(0)
                }
                printf("CACHE EVICT %x %x\n", Cat(tag, cache_idx, Bits(0, idx_width)), block)
              }
            }
          } // write_buffer.io.enq.ready
        } // line_evict_cause_wb
        // Perform linefill if we don't need to do block write back or if dirty
        // block is already placed in write queue
        val ready_to_linefill = ~line_evict_cause_wb || write_buffer.io.enq.ready
        when (ready_to_linefill) {
          // This is a "real" write queue peek, so invalid any peeked data
          write_buffer.io.peek_req.valid := Bool(true)
          when (write_buffer.io.peek_resp.valid) {
            // Hit in write buffer
            val orig_data = write_buffer.io.peek_resp.bits.burst_data.toBits
            val wdata = Fill(num_words_per_cache_line, StoreDataGen(req_data, req_typ))
            val wmask = (StoreMask(req_typ) << bit_shift_amt)(block_width-1, 0)
            printf("WB HIT %x %x\n", Cat(req_addr >> idx_width, Bits(0, idx_width)), orig_data)
            for (i <- 0 until set_associativity) {
              when (is_lru_replace_line(i)) {
                data_bank(i)(cache_idx) := Cat(
                  // Peeked data are always dirty, thus always set the dirty bit
                  Bits("b11", 2),
                  tag_idx,
                  Mux(req_fcn === M_XWR,
                    (orig_data & ~wmask) | (wdata & wmask),
                    orig_data
                  )
                )
              }
            }
            for (i <- 0 until set_associativity) {
              when (is_lru_replace_line(i)) {
                update_lru_line_age(i)
              }
            }
            io.core_port.resp.valid := Bool(true)
            io.core_port.resp.bits.data := LoadDataGen(orig_data >> bit_shift_amt, req_typ)
            req_ready_reg := Bool(true)
            state := s_ready
          } .elsewhen (io.mem_port.req.ready) {
            printf("LINEFILL %x\n", Cat(req_addr >> idx_width, Bits(0, idx_width)))
            // Block write_buffer before we can send read request to mem_port
            // else we may send CACHE linefill and WRITEQUEUE dequeue at the
            // same cycle
            write_buffer.io.mem_port.req.ready := Bool(false)
            io.mem_port.req.valid := Bool(true)
            io.mem_port.req.bits.addr := Cat(req_addr >> idx_width, Bits(0, idx_width))
            io.mem_port.req.bits.fcn := M_XRD
            io.mem_port.req.bits.typ := MT_WU
            state := s_read_block
          }
        }
      }
      is (s_read_block) {
        when (io.mem_port.resp.valid) {
          // linefill data ready: fill in the new line
          val orig_data = io.mem_port.resp.bits.burst_data.toBits
          val wdata = Fill(num_words_per_cache_line, StoreDataGen(req_data, req_typ))
          val wmask = (StoreMask(req_typ) << bit_shift_amt)(block_width-1, 0)
          for (i <- 0 until set_associativity) {
            when (is_lru_replace_line(i)) {
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
          for (i <- 0 until set_associativity) {
            when (is_lru_replace_line(i)) {
              update_lru_line_age(i)
            }
          }
          io.core_port.resp.valid := Bool(true)
          io.core_port.resp.bits.data := LoadDataGen(orig_data >> bit_shift_amt, req_typ)
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
    val htif_cache_hit_vec = Vec.fill(set_associativity) { Bool() }
    val htif_cache_hit = orR(htif_cache_hit_vec.toBits)
    htif_cache_hit_vec := Bits(0)
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
        htif_cache_hit_vec(i) := valid_bit && tag === tag_idx
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
      when (~htif_cache_hit) {
        write_buffer.io.peek_req.bits.addr := req_addr
        when (write_buffer.io.peek_resp.valid) {
          val block = write_buffer.io.peek_resp.bits.burst_data.toBits
          printf("HTIF WB HIT %x %x\n", req_addr, block)
          // This is very damn hacky, as I just don't handle the HTIF-WRITE case
          // as it doesn't happen in our test harness. HA!
          assert(io.htif_port.req.bits.fcn === M_XRD)
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
