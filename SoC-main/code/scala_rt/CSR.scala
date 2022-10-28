

import chisel3._
import chisel3.util._
import chisel3.util.experimental._
import difftest._
import ID._

class CSR extends Module {
  val io = IO(new Bundle {
    val pc       = Input(UInt(32.W))
    val fuType   = Input(UInt(2.W))
    val op_type  = Input(UInt(6.W))
    val rs1      = Input(UInt(64.W))
    val rs2      = Input(UInt(64.W))
    val csr_Jmp_addr = Output(UInt(32.W)) 
    val csr_Jmp_en = Output(Bool())
    val csr_data  = Output(UInt(64.W))
    val irq      = Output(Bool())
  })

  val mtvec     = RegInit(UInt(64.W), 0.U)
  val mcause    = RegInit(UInt(64.W), 0.U)
  val mstatus   = RegInit(UInt(64.W), "h00001800".U)
  val mepc      = RegInit(UInt(64.W), 0.U)
  val mcycle    = RegInit(UInt(64.W), 0.U)
  val minstret  = RegInit(UInt(64.W), 0.U)
  val mie       = RegInit(UInt(64.W), 0.U)
  val mip       = RegInit(UInt(64.W), 0.U)
  val mscratch  = RegInit(UInt(64.W), 0.U)  

  def access(isCsr: Bool, addr: UInt, src: UInt, cmd: UInt): UInt = {
    val rdata = MuxLookup(addr, 0.U, Array(
      Mtvec.U   -> mtvec,
      Mcause.U  -> mcause,
      Mepc.U    -> mepc,
      Mstatus.U -> mstatus,
      Mcycle.U  -> mcycle,
      Minstret.U -> minstret,
      Mscratch.U -> mscratch,
      Mie.U      -> mie
    ))
    val wdata = MuxLookup(cmd, 0.U, Array(
      CsrW  -> src,
      CsrS  -> (rdata | src),
      CsrC  -> (rdata & ~src),
      CsrWi -> src,
      CsrSi -> (rdata | src),
      CsrCi -> (rdata & ~src)             
    ))

    when (isCsr && cmd =/= CsrJmp) {
      when (addr === Mtvec.U) { mtvec := wdata }
      when (addr === Mstatus.U) { 
        val mstatus_xs = wdata(16,15)
        val mstatus_fs = wdata(14,13)
        val mstatus_sd = ((mstatus_xs === "b11".U) || (mstatus_fs === "b11".U)).asUInt()
        mstatus := Cat(mstatus_sd, wdata(62, 0)) }     
      when (addr === Mepc.U) { mepc := wdata }
      when (addr === Mcause.U) { mcause := wdata }
      when (addr === Mie.U) { mie := wdata }
      when (addr === Mscratch.U) { mscratch := wdata }
    }

    rdata
  }

  def jmp_en(isCsr: Bool, addr: UInt, pc: UInt, cmd: UInt): Bool = {
    val br_en = isCsr && cmd === CsrJmp
    when (br_en && addr === privEcall) {
      mepc := pc
      mcause := 11.U
      mstatus := Cat(mstatus(63, 13), 1.U, 1.U, mstatus(10, 8), mstatus(3), mstatus(6, 4), 0.U, mstatus(2, 0))
    }.elsewhen (br_en && addr === privMret) {
      mstatus := Cat(mstatus(63, 13), 0.U, 0.U, mstatus(10, 8) ,1.U, mstatus(6, 4), mstatus(7), mstatus(2, 0))
    }
    
    br_en
  }

  def jmp_addr(addr: UInt): UInt = {
    val br_addr = MuxLookup(addr, "h80000000".U, Array(
      privEcall -> mtvec,
      privMret  -> mepc
    ))
    br_addr
  }

  mcycle := mcycle + 1.U
  def instrCnt(instrCommit: Bool) {
    when (instrCommit) {
      minstret := minstret + 1.U
    }
  }

  // Interrupt
  val s_idle :: s_wait :: Nil = Enum(2)
  val intr_state = RegInit(s_idle)

  val intr = RegInit(Bool(), false.B)
  val irq_en = RegInit(Bool(), false.B)  
  val intr_no = RegInit(UInt(64.W), 0.U)
  val mtip = RegInit(0.U)
  BoringUtils.addSink(mtip, "mtip")
//  mip := Cat(Fill(56, 0.U), mtip, Fill(7, 0.U))

  irq_en := false.B
  intr := false.B
  val intr_global_en = (mstatus(3) === 1.U)
  val intr_clint_en = (mie(7) === 1.U && mtip === 1.U)
  switch (intr_state) {
    is (s_idle) {
      when (intr_global_en && intr_clint_en) {
        intr_state := s_wait
        irq_en := true.B
      }
    }
    is (s_wait) {
        mepc := io.pc
        mcause := "h8000000000000007".U
        mstatus := Cat(mstatus(63, 13), 1.U, 1.U, mstatus(10, 8), mstatus(3), mstatus(6, 4), 0.U, mstatus(2, 0))
        intr := true.B
        intr_no := 7.U
        intr_state := s_idle
    }
  }
 // io.irq := RegNext(irq_en)
  io.irq := irq_en
  io.csr_data := access(io.fuType === FuCsr, io.rs2(11, 0), io.rs1, io.op_type)
  io.csr_Jmp_en   := jmp_en(io.fuType === FuCsr, io.rs2(11, 0), io.pc, io.op_type) || io.irq
  io.csr_Jmp_addr := Mux(io.irq, mtvec, jmp_addr(io.rs2(11, 0)))

  //difftest for arch event & CSR state
  val dt_ae = Module(new DifftestArchEvent)
  dt_ae.io.clock        := clock
  dt_ae.io.coreid       := 0.U
  dt_ae.io.intrNO       := Mux(intr, intr_no, 0.U)
  dt_ae.io.cause        := 0.U
  dt_ae.io.exceptionPC  := Mux(intr, mepc, 0.U)

  val dt_cs = Module(new DifftestCSRState)
  dt_cs.io.clock          := clock
  dt_cs.io.coreid         := 0.U
  dt_cs.io.priviledgeMode := 3.U  // Machine mode
  dt_cs.io.mstatus        := mstatus
  dt_cs.io.sstatus        := mstatus & "h80000003000de122".U
  dt_cs.io.mepc           := mepc
  dt_cs.io.sepc           := 0.U
  dt_cs.io.mtval          := 0.U
  dt_cs.io.stval          := 0.U
  dt_cs.io.mtvec          := mtvec
  dt_cs.io.stvec          := 0.U
  dt_cs.io.mcause         := mcause
  dt_cs.io.scause         := 0.U
  dt_cs.io.satp           := 0.U
  dt_cs.io.mip            := 0.U
  dt_cs.io.mie            := mie
  dt_cs.io.mscratch       := mscratch 
  dt_cs.io.sscratch       := 0.U
  dt_cs.io.mideleg        := 0.U
  dt_cs.io.medeleg        := 0.U   
}
