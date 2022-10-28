import chisel3._
import chisel3.util._
import chisel3.util.experimental._
import difftest._

class SimTop extends Module {
  val io = IO(new Bundle {
    val logCtrl = new LogCtrlIO
    val perfInfo = new PerfInfoIO
    val uart = new UARTIO
  })

  val core = Module(new Core)

  val mem = Module(new Ram2r1w)
  mem.io.imem <> core.io.imem
  mem.io.dmem <> core.io.dmem

  // val rf_a0 = WireInit(0.U(64.W))
  // BoringUtils.addSink(rf_a0, "rf_a0")
  // io.uart.out.valid := (core.io.inst(6,0) === "b1111011".U)
  // io.uart.out.ch := rf_a0

  io.uart.out.valid := false.B
  io.uart.out.ch := 0.U
  io.uart.in.valid := false.B

}
