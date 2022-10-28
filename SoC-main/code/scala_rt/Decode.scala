import chisel3._
import chisel3.util._
import ID._

class Decode extends Module {
  val io = IO(new Bundle {
    val inst     = Input(UInt(32.W))
    val rs1_addr = Output(UInt(5.W))
    val rs1_type = Output(UInt(2.W))
    val rs2_addr = Output(UInt(5.W))
    val rs2_type = Output(UInt(2.W))
    val rd_addr  = Output(UInt(5.W))
    val rd_en    = Output(Bool())
    val op_type  = Output(UInt(6.W)) 
    val fuType   = Output(UInt(2.W))    
    val imm      = Output(UInt(64.W))
    val csr_imm  = Output(UInt(64.W))
  })

  val instr = io.inst
  val instrType :: fuType :: op_type :: Nil = ListLookup(instr, DecodeDefault, DecodeTable)

  io.op_type  := op_type
  io.fuType   := fuType
  io.rs1_addr := instr(19, 15)
  io.rs2_addr := instr(24, 20)

  val csri = (io.op_type === CsrWi) || (io.op_type === CsrSi) || (io.op_type === CsrCi)
  def isrfWen(instrType : UInt): Bool = instrType(2)
  io.rd_en := isrfWen(instrType)
  io.rd_addr := Mux(isrfWen(instrType), instr(11, 7), instr(24, 20))
  
  val rs1_type = MuxLookup(instrType, Src1Reg, Array(
    InstrI -> Src1Reg,
    InstrR -> Src1Reg, 
    InstrS -> Src1Reg, 
    InstrB -> Src1Reg, 
    InstrU -> Src1Pc , 
    InstrJ -> Src1Pc ,
    InstrN -> Src1Pc 
  ))

  io.rs1_type := Mux(csri, Src1Imm, rs1_type)
  io.rs2_type := MuxLookup(instrType, Src2Reg, List(
    InstrI -> Src2Imm,
    InstrR -> Src2Reg,
    InstrS -> Src2Imm,
    InstrB -> Src2Imm,
    InstrU -> Src2Imm,
    InstrJ -> Src2Imm,
    InstrN -> Src2Imm
  ))

  io.imm := MuxLookup(instrType, 0.U(64.W), Array(
    InstrI -> Cat(Fill(52, instr(31)), instr(31, 20)),
    InstrS -> Cat(Fill(52, instr(31)), instr(31, 25), instr(11, 7)),
    InstrB -> Cat(Fill(52, instr(31)), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)),
    InstrU -> Cat(Fill(32, instr(31)), instr(31, 12), 0.U(12.W)),
    InstrJ -> Cat(Fill(44, instr(31)), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W))
  ))
    
  io.csr_imm := Mux(csri, Cat(0.U(59.W), instr(19, 15)), 0.U(64.W)) 
}

