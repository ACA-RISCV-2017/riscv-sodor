//**************************************************************************
// RISCV Processor Tile
//--------------------------------------------------------------------------
//

package Sodor
{

import Chisel._
import Node._
import Constants._
import Common._   
import Common.Util._   
import AcaCustom._
import AcaCustomVictimCache._

class SodorTileIo extends Bundle  
{
   val host     = new HTIFIO()
}

class SodorTile(implicit val conf: SodorConfiguration) extends Module
{
   val io = new SodorTileIo()
   
   val core   = Module(new Core(resetSignal = io.host.reset))
   val memory = Module(new ScratchPadMemory(num_core_ports = 2))
   val victim_cache = Module(new VictimCache())
   val dcache = Module(new DCache())

   core.io.imem <> memory.io.core_ports(0)
   core.io.dmem <> dcache.io.core_port
   dcache.io.mem_port <> victim_cache.io.cache_port
   victim_cache.io.mem_port <> memory.io.core_ports(1)

   val htif   = Module(new HTIFCoherencyModule())
   htif.io.mem_port <> memory.io.htif_port
   htif.io.cache_port <> dcache.io.htif_port

   // HTIF/memory request
   htif.io.htif_port.req.valid     := io.host.mem_req.valid
   htif.io.htif_port.req.bits.addr := io.host.mem_req.bits.addr.toUInt
   htif.io.htif_port.req.bits.data := io.host.mem_req.bits.data
   htif.io.htif_port.req.bits.fcn  := Mux(io.host.mem_req.bits.rw, M_XWR, M_XRD)
   io.host.mem_req.ready             := htif.io.htif_port.req.ready     

   // HTIF/htif response
   io.host.mem_rep.valid := htif.io.htif_port.resp.valid
   io.host.mem_rep.bits := htif.io.htif_port.resp.bits.data

   core.io.host <> io.host
}

class HTIFCoherencyModule(implicit conf: SodorConfiguration) extends Module {
  val io = new Bundle {
    val htif_port = (new MemPortIo(data_width=64)).flip
    val mem_port = new MemPortIo(data_width=64)
    val cache_port = new MemPortIo(data_width=64)
  }
  io.mem_port.req.valid := io.htif_port.req.valid
  io.mem_port.req.bits.addr := io.htif_port.req.bits.addr
  io.mem_port.req.bits.data := io.htif_port.req.bits.data
  io.mem_port.req.bits.fcn  := io.htif_port.req.bits.fcn
  io.cache_port.req.valid := io.htif_port.req.valid
  io.cache_port.req.bits.addr := io.htif_port.req.bits.addr
  io.cache_port.req.bits.data := io.htif_port.req.bits.data
  io.cache_port.req.bits.fcn  := io.htif_port.req.bits.fcn

  // NOTE: HTIF expects synchronous read.
  io.htif_port.req.ready := Bool(true)
  io.htif_port.resp.valid := Reg(
    next = io.htif_port.req.valid && io.htif_port.req.bits.fcn === M_XRD
  )
  io.htif_port.resp.bits.data := Mux(
    io.cache_port.resp.valid,
    io.cache_port.resp.bits.data,
    io.mem_port.resp.bits.data
  )
}

}
