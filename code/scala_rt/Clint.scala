import chisel3._
import chisel3.util._
import chisel3.util.experimental._

class Clint extends Module {
  val io = IO(new Bundle {
    val clint_wen = Input(Bool())
    val clint_ren = Input(Bool())
    val clint_addr  = Input(UInt(32.W))
    val clint_wdata = Input(UInt(64.W))
    val clint_wmask = Input(UInt(64.W))
    val clint_rdata = Output(UInt(64.W)) 
  })

  val mtime = RegInit(UInt(64.W), 0.U)
  val mtimecmp = RegInit(UInt(64.W), 0.U)

  // Suppose the unit of mtime is us, core frequency is 100 MHz.
  // 1 us / 100 MHz = 100
  // val tick = RegInit(UInt(64.W), 1.U)
  // val counter = RegInit(UInt(64.W), 0.U)
  // counter := Mux(counter < tick, counter + 1.U, 0.U)
  // when (counter === tick) {
  //   mtime := mtime + 1.U
  // }
  mtime := mtime + 1.U

  when (io.clint_addr(15,0) === "h4000".U && io.clint_ren) {
    io.clint_rdata :=  mtimecmp
  } .elsewhen (io.clint_addr(15,0) === "hbff8".U && io.clint_ren) {
    io.clint_rdata :=  mtime
  } otherwise {
    io.clint_rdata := 0.U
  }

  when (io.clint_addr(15,0) === "h4000".U && io.clint_wen) {
    mtimecmp :=  io.clint_wdata & io.clint_wmask
  } .elsewhen (io.clint_addr(15,0) === "hbff8".U && io.clint_wen) {
    mtime :=  io.clint_wdata & io.clint_wmask
  } 

  val mtip = RegInit(0.U)
  mtip := (mtime >= mtimecmp).asUInt()
  BoringUtils.addSource(mtip, "mtip")
}