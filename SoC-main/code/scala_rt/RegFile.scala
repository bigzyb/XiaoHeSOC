
import chisel3._
import chisel3.util.experimental._
import difftest._
import ID._

class RegFile extends Module {
  val io = IO(new Bundle {
    val pc       = Input(UInt(32.W))    
    val rs1_addr = Input(UInt(5.W))
    val rs2_addr = Input(UInt(5.W))
    val rs1_type = Input(UInt(2.W))
    val rs2_type = Input(UInt(2.W))        
    val rd_addr  = Input(UInt(5.W))
    val rd_en    = Input(Bool())
    val rd_data  = Input(UInt(64.W))       
    val imm      = Input(UInt(64.W))  
    val csr_imm  = Input(UInt(64.W))   
    val irq      = Input(Bool())   
    val rs1  = Output(UInt(64.W))
    val rs2  = Output(UInt(64.W)) 
    val src2 = Output(UInt(64.W))       
  })

  val rf = RegInit(VecInit(Seq.fill(32)(0.U(64.W))))
  def read(addr: UInt) : UInt = Mux(addr === 0.U, 0.U, rf(addr))
  def write(addr: UInt, data: UInt) = { rf(addr) := Mux(addr === 0.U, 0.U, data) }

  val rs1_data = read(io.rs1_addr)
  val rs2_data = read(io.rs2_addr)
  io.rs1 := Mux(io.rs1_type === Src1Pc, io.pc, Mux(io.rs1_type === Src1Imm, io.csr_imm, rs1_data))
  io.rs2 := Mux(io.rs2_type === Src2Reg, rs2_data, io.imm)
  io.src2 := rs2_data; // for S-type and B-type
  
  when (io.rd_en && ~io.irq) { write(io.rd_addr, io.rd_data) }

  // val rf_diff = RegInit(VecInit(Seq.fill(32)(0.U(64.W))))
  //   for (i <- 0 until 32) {
  //     rf_diff(i)  := Mux((io.rd_en && (io.rd_addr === i.U) && (i.U =/= 0.U)) , io.rd_data , rf(i))
  //   }
  val dt_ar = Module(new DifftestArchIntRegState)
  dt_ar.io.clock  := clock
  dt_ar.io.coreid := 0.U
 // dt_ar.io.gpr    := rf_diff
  dt_ar.io.gpr    := rf

  BoringUtils.addSource(rf(10), "rf_a0")
}

