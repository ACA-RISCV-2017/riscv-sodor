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

    val cache_valid = Reg(Bool(), init=Bool(false))
    val cache_addr = Reg(Bits(width = conf.xprlen))
    val cache_data = Vec.fill(16) { Reg(Bits(width=32)) }

    val s_idle :: s_hit :: Nil = Enum(UInt(), 2)
    val state = Reg(init = s_idle)

    val reg_req = Reg(io.cache_port.req)

    // state
    switch (state) {
      is(s_idle) {
        when (io.cache_port.req.valid && io.cache_port.req.bits.fcn === M_XRD && 
          cache_valid && aligned(io.cache_port.req.bits.addr) === cache_addr)
        {
          printf("Hit!")
          state := s_hit
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
        when (io.cache_port.req.valid && io.cache_port.req.bits.fcn === M_XRD &&
          cache_valid && aligned(io.cache_port.req.bits.addr) === cache_addr)
        {
          io.cache_port.resp.valid := Bool(false)
          io.mem_port.req.valid := Bool(false)
        }
      }
      is(s_hit) {
        io.cache_port.req.ready := Bool(false)
        io.cache_port.resp.valid := Bool(true)
        io.cache_port.resp.bits.data := cache_data(UInt(reg_req.bits.addr(5, 2)))
        io.cache_port.resp.bits.burst_data := cache_data
        io.mem_port.req.valid := Bool(false)
      }
    }

    // Victim Cache
    when (state === s_idle && io.cache_port.req.valid && io.cache_port.req.bits.fcn === M_XWRBURST)
    {
      printf("Changed!")
      cache_valid := Bool(true)
      cache_addr := aligned(io.cache_port.req.bits.addr)
      cache_data := io.cache_port.req.bits.burst_data
    }
  }

  def aligned(addr : Bits) : Bits = 
  {
    return Cat(addr(addr.getWidth - 1, 6), Bits(0, width=6))
  }
}
