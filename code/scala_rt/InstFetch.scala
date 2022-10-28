
import chisel3._
import chisel3.util._

class InstFetch extends Module {
  val io = IO(new Bundle {
    val imem    = new RomIO
    val br_en   = Input(Bool())
    val br_addr = Input(UInt(32.W))
    val pc      = Output(UInt(32.W))
    val inst    = Output(UInt(32.W))        
  })

  val pc = RegInit("h7ffffffc".U(32.W))
  pc := Mux(io.br_en, io.br_addr, pc + 4.U)

  io.imem.en := true.B
  io.imem.addr := pc.asUInt() 

  io.pc := pc
  io.inst := io.imem.rdata(31, 0)
}
