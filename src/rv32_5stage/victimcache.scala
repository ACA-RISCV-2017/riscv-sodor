package object AcaCustomVictimCache
{
  import Chisel._
  import Node._
  import Sodor.Constants._
  import Common._

  class VictimCache()(implicit conf: SodorConfiguration) extends Module
  {
    val io = new Bundle {
      val cache_port = (new MemPortIo(data_width=conf.xprlen)).flip
      val mem_port = new MemPortIo(data_width=conf.xprlen)
    }

	  val entry_num = 4

    val cache_valid = Vec.fill(entry_num) { Reg(Bool(), init=Bool(false)) }
    val cache_addr = Vec.fill(entry_num) { Reg(Bits(width = conf.xprlen)) }
    val cache_data = Vec.fill(entry_num) { Vec.fill(16) { Reg(Bits(width=32)) } }

    val s_idle :: s_hit :: Nil = Enum(UInt(), 2)
    val state = Reg(init = s_idle)

    val reg_req = Reg(io.cache_port.req)
    val hit_index = Reg(Bits(width=log2Up(entry_num)))
    val write_index = Reg(Bits(width=log2Up(entry_num)), init=Bits(0))

    // state
    switch (state) {
      is(s_idle) {
        when (io.cache_port.req.valid && io.cache_port.req.bits.fcn === M_XRD)
        {
          for (i <- 0 until entry_num) {
            when (cache_valid(i) && aligned(io.cache_port.req.bits.addr) === cache_addr(i)) {
              printf("Victim Cache Hit!")
              state := s_hit
            }
          }
        }
      }
      is(s_hit) {
        state := s_idle
      }
    }

    // I/O
    // Default: both ports are wired
    io.cache_port.req.ready := io.mem_port.req.ready
    io.cache_port.resp.valid := io.mem_port.resp.valid
    io.cache_port.resp.bits := io.mem_port.resp.bits
    io.mem_port.req.valid := io.cache_port.req.valid
    io.mem_port.req.bits := io.cache_port.req.bits

    switch (state) {
      is(s_idle) {
        when (io.cache_port.req.valid && io.cache_port.req.bits.fcn === M_XRD)
        {
          for (i <- 0 until entry_num) {
            when (cache_valid(i) && aligned(io.cache_port.req.bits.addr) === cache_addr(i)) {
              io.cache_port.resp.valid := Bool(false)
              io.mem_port.req.valid := Bool(false)
              hit_index := Bits(i)
            }
          }
        }
      }
      is(s_hit) {
        io.cache_port.req.ready := Bool(false)
        io.cache_port.resp.valid := Bool(true)
        io.cache_port.resp.bits.data := cache_data(UInt(hit_index))(UInt(reg_req.bits.addr(5, 2)))
        io.cache_port.resp.bits.burst_data := cache_data(UInt(hit_index))
        io.mem_port.req.valid := Bool(false)
      }
    }

    // Victim Cache
    when (state === s_idle && io.cache_port.req.valid && io.cache_port.req.bits.fcn === M_XWRBURST) {
      for (i <- 0 until entry_num) {
        when (cache_valid(i) && aligned(io.cache_port.req.bits.addr) === cache_addr(i) && write_index =/= Bits(i)) {
          cache_valid(i) := Bool(false)
        }
      }
      cache_valid(UInt(write_index)) := Bool(true)
      cache_addr(UInt(write_index)) := aligned(io.cache_port.req.bits.addr)
      cache_data(UInt(write_index)) := io.cache_port.req.bits.burst_data
      write_index := write_index + UInt(1)
    }
  }

  def aligned(addr : Bits) : Bits = 
  {
    return Cat(addr(addr.getWidth - 1, 6), Bits(0, width=6))
  }
}
