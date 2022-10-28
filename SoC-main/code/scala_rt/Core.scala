import chisel3._
import chisel3.util.experimental._
import difftest._
import ID._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = new RomIO
    val dmem = new RamIO
    // val inst = Output(UInt(32.W))
  }) 

  val fetch = Module(new InstFetch)
  fetch.io.imem <> io.imem

  val decode = Module(new Decode)
  decode.io.inst := fetch.io.inst

  val rf = Module(new RegFile)
  rf.io.pc       := fetch.io.pc
  rf.io.rs1_addr := decode.io.rs1_addr
  rf.io.rs2_addr := decode.io.rs2_addr
  rf.io.rs1_type := decode.io.rs1_type
  rf.io.rs2_type := decode.io.rs2_type
  rf.io.rd_addr  := decode.io.rd_addr
  rf.io.rd_en    := decode.io.rd_en
  rf.io.imm      := decode.io.imm
  rf.io.csr_imm  := decode.io.csr_imm

  val csr = Module(new CSR)
  csr.io.pc := fetch.io.pc
  csr.io.fuType   := decode.io.fuType
  csr.io.op_type  := decode.io.op_type
  csr.io.rs1  := rf.io.rs1
  csr.io.rs2  := rf.io.rs2
  rf.io.irq   := csr.io.irq

  val execution = Module(new Execution)
  execution.io.pc := fetch.io.pc
  execution.io.op_type := decode.io.op_type
  execution.io.fuType  := decode.io.fuType
  execution.io.rs1 := rf.io.rs1
  execution.io.rs2 := rf.io.rs2
  execution.io.src2:= rf.io.src2
  execution.io.imm := decode.io.imm
  execution.io.dmem <> io.dmem
  execution.io.csr_Jmp_addr := csr.io.csr_Jmp_addr
  execution.io.csr_Jmp_en   := csr.io.csr_Jmp_en
  execution.io.csr_data     := csr.io.csr_data

  fetch.io.br_en   := execution.io.br_en
  fetch.io.br_addr := execution.io.br_addr
  rf.io.rd_data    := execution.io.rd_data

  val clint = Module(new Clint)
  clint.io.clint_wen <> execution.io.clint_wen
  clint.io.clint_ren <> execution.io.clint_ren
  clint.io.clint_addr <> execution.io.clint_addr
  clint.io.clint_wdata <> execution.io.clint_wdata 
  clint.io.clint_wmask <> execution.io.clint_wmask
  execution.io.clint_rdata <> clint.io.clint_rdata

  /* ----- Difftest ------------------------------ */
  val inst_valid = (fetch.io.inst =/= 0.U)
  val mtimecmp = (io.dmem.addr(31,0) === "h02004000".U) || (io.dmem.addr(31,0) === "h0200bff8".U)
  val string = (fetch.io.inst(6,0) === "b1111011".U) //skip 0x7b self-defined putch instruction
  val skip = string || mtimecmp || ((fetch.io.inst(31, 20) === "hb00".U) && (decode.io.fuType === FuCsr))

  val dt_ic = Module(new DifftestInstrCommit)
  dt_ic.io.clock    := clock
  dt_ic.io.coreid   := 0.U
  dt_ic.io.index    := 0.U
//  dt_ic.io.valid    := true.B
  dt_ic.io.valid    := RegNext(inst_valid)
  dt_ic.io.pc       := RegNext(fetch.io.pc)
  dt_ic.io.instr    := RegNext(fetch.io.inst)
  dt_ic.io.special  := 0.U
  dt_ic.io.skip     := RegNext(skip)
  dt_ic.io.isRVC    := false.B
  dt_ic.io.scFailed := false.B
  dt_ic.io.wen      := RegNext(decode.io.rd_en)
  dt_ic.io.wdata    := RegNext(execution.io.rd_data)
  dt_ic.io.wdest    := RegNext(decode.io.rd_addr)

  val dt_ae = Module(new DifftestArchEvent)
  dt_ae.io.clock        := clock
  dt_ae.io.coreid       := 0.U
  dt_ae.io.intrNO       := 0.U
  dt_ae.io.cause        := 0.U
  dt_ae.io.exceptionPC  := 0.U

  val cycle_cnt = RegInit(0.U(64.W))
  val instr_cnt = RegInit(0.U(64.W))

  cycle_cnt := cycle_cnt + 1.U
  instr_cnt := instr_cnt + 1.U

  val rf_a0 = WireInit(0.U(64.W))
  BoringUtils.addSink(rf_a0, "rf_a0")

  when (string) {
   printf("%c", rf_a0(7, 0))
  }

  val dt_te = Module(new DifftestTrapEvent)
  dt_te.io.clock    := clock
  dt_te.io.coreid   := 0.U
  dt_te.io.valid    := (fetch.io.inst === "h0000006b".U)
  dt_te.io.code     := rf_a0(2, 0)
  dt_te.io.pc       := fetch.io.pc
  dt_te.io.cycleCnt := cycle_cnt
  dt_te.io.instrCnt := instr_cnt
 
}
