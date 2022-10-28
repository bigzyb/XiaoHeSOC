
import chisel3._
import chisel3.util._
import ID._

class Execution extends Module {
  val io = IO(new Bundle {
    val pc       = Input(UInt(32.W))
    val op_type  = Input(UInt(6.W))
    val rs1      = Input(UInt(64.W))
    val rs2      = Input(UInt(64.W))
    val src2     = Input(UInt(64.W))
    val fuType   = Input(UInt(2.W))  
    val imm      = Input(UInt(64.W))
    val csr_Jmp_addr = Input(UInt(32.W)) 
    val csr_Jmp_en   = Input(Bool())
    val csr_data = Input(UInt(64.W))    
    val br_en    = Output(Bool())
    val br_addr  = Output(UInt(32.W))    
    val rd_data  = Output(UInt(64.W))
    val dmem     = new RamIO
    val clint_rdata = Input(UInt(64.W)) 
    val clint_wen = Output(Bool())
    val clint_ren = Output(Bool())
    val clint_addr  = Output(UInt(32.W))
    val clint_wdata = Output(UInt(64.W))
    val clint_wmask = Output(UInt(64.W)) 
  })


  def Alu_access(rs1: UInt, rs2: UInt, imm:UInt, op_type: UInt): UInt = {
    val shamt = rs2(4, 0)
    val shamt64 = imm(5, 0)
    val sraw  = ((rs1(31,0).asSInt >> shamt).asUInt)
    MuxLookup(op_type, 0.U, Array(
      AluAdd  -> (rs1  +  rs2),
      AluSll  -> (rs1 << rs2(5, 0)),
      AluSlli -> (rs1 << shamt64),
      AluSlt  -> ((rs1.asSInt < rs2.asSInt).asUInt),
      AluSltu -> ((rs1 < rs2).asUInt),
      AluXor  -> (rs1  ^  rs2),
      AluSrl  -> (rs1  >> shamt),
      AluSrli -> (rs1  >> shamt64),      
      AluOr   -> (rs1  |  rs2),
      AluAnd  -> (rs1  &  rs2),
      AluSub  -> (rs1  -  rs2),
      AluLui  -> rs2,
      AluSra  -> ((rs1.asSInt >> shamt).asUInt),
      AluSrai -> ((rs1.asSInt >> shamt64).asUInt),      
      AluAddiw  -> Cat(Fill(32,(rs1 + rs2)(31)),(rs1 + rs2)(31,0)),
      AluSraw   -> Cat(Fill(32,sraw(31)), sraw(31,0)),
      AluSllw   -> Cat(Fill(32, (rs1(31,0) << shamt)(31)), (rs1(31,0) << shamt)(31,0)),
      AluSrlw   -> Cat(Fill(32, (rs1(31,0) >> shamt)(31)), (rs1(31,0) >> shamt)(31,0)),  
      AluSubw   -> Cat(Fill(32,(rs1 - rs2)(31)),(rs1 - rs2)(31,0))      
    ))
  }

  def Bru_en(isBru: Bool, pc: UInt, offset: UInt, rs1: UInt, rs2: UInt, op_type: UInt):Bool = {
    isBru && MuxLookup(op_type, false.B, Array(
      BruBeq  -> (rs1 === rs2),
      BruBne  -> (rs1 =/= rs2),
      BruBlt  -> (rs1.asSInt  <  rs2.asSInt),
      BruBge  -> (rs1.asSInt >=  rs2.asSInt),
      BruBltu -> (rs1  <  rs2),
      BruBgeu -> (rs1  >= rs2),
      BruJal  -> true.B,
      BruJalr -> true.B
    ))
  }

  def rdataExt(rdata: UInt, func: UInt, addr: UInt): UInt = {
    MuxLookup(func, rdata, Array(
       LsuLb   -> MuxLookup(addr(2,0),0.U(64.W),Array(
          "b000".U -> Cat(Fill(56, rdata(7)), rdata(7, 0)),
          "b001".U -> Cat(Fill(56, rdata(15)), rdata(15, 8)),
          "b010".U -> Cat(Fill(56, rdata(23)), rdata(23, 16)),
          "b011".U -> Cat(Fill(56, rdata(31)), rdata(31, 24)),
          "b100".U -> Cat(Fill(56, rdata(39)), rdata(39, 32)),
          "b101".U -> Cat(Fill(56, rdata(47)), rdata(47, 40)),
          "b110".U -> Cat(Fill(56, rdata(55)), rdata(55, 48)),
          "b111".U -> Cat(Fill(56, rdata(63)), rdata(63, 56))
      )),       
      LsuLh   ->  MuxLookup(addr(2,1),0.U(64.W),Array(
          "b00".U  -> Cat(Fill(48, rdata(15)), rdata(15, 0)),
          "b01".U  -> Cat(Fill(48, rdata(31)), rdata(31, 16)),
          "b10".U  -> Cat(Fill(48, rdata(47)), rdata(47, 32)),
          "b11".U  -> Cat(Fill(48, rdata(63)), rdata(63, 48))
      )),
      LsuLw   -> MuxLookup(addr(2),0.U(64.W),Array(
          "b0".U  -> Cat(Fill(32, rdata(31)), rdata(31, 0)),
          "b1".U  -> Cat(Fill(32, rdata(63)), rdata(63, 32))
      )),
      LsuLd   -> rdata,
      LsuLbu  -> MuxLookup(addr(2,0),0.U(64.W),Array(
          "b000".U -> Cat(0.U(56.W), rdata(7, 0)),
          "b001".U -> Cat(0.U(56.W), rdata(15, 8)),
          "b010".U -> Cat(0.U(56.W), rdata(23, 16)),
          "b011".U -> Cat(0.U(56.W), rdata(31, 24)),
          "b100".U -> Cat(0.U(56.W), rdata(39, 32)),
          "b101".U -> Cat(0.U(56.W), rdata(47, 40)),
          "b110".U -> Cat(0.U(56.W), rdata(55, 48)),
          "b111".U -> Cat(0.U(56.W), rdata(63, 56))
      )),
      LsuLhu  -> MuxLookup(addr(2,1),0.U(64.W),Array(
          "b00".U -> Cat(0.U(48.W), rdata(15, 0)),
          "b01".U -> Cat(0.U(48.W), rdata(31, 16)),
          "b10".U -> Cat(0.U(48.W), rdata(47, 32)),
          "b11".U -> Cat(0.U(48.W), rdata(63, 48))
      )),      
      LsuLwu  -> MuxLookup(addr(2),0.U(64.W),Array(
          "b0".U  -> Cat(0.U(32.W), rdata(31, 0)),
          "b1".U  -> Cat(0.U(32.W), rdata(63, 32))
      ))            
    ))
  }

  val aluOut  = Alu_access(io.rs1, io.rs2, io.imm, io.op_type)
  val jmp_en   = Bru_en(io.fuType === FuBru, io.pc, io.rs2, io.rs1, io.src2, io.op_type)
  val jmp_addr = Mux(io.op_type === BruJalr, io.rs1 + io.rs2, io.pc + io.rs2)

  io.dmem.addr := io.rs1 + io.rs2
  io.clint_addr:= io.rs1 + io.rs2
  val mem_en = io.fuType === FuLsu
  io.dmem.en   := mem_en && (io.dmem.addr(31,16) =/= "h0200".U)
  io.clint_ren := mem_en && (io.dmem.addr(31,16) === "h0200".U)
  val mem_wen = io.fuType === FuLsu && io.op_type(3)
  io.dmem.wen  := mem_wen && (io.dmem.addr(31,16) =/= "h0200".U)
  io.clint_wen := mem_wen && (io.dmem.addr(31,16) === "h0200".U)
  io.rd_data:= MuxLookup(io.fuType, 0.U(64.W), Array(
    FuAlu -> aluOut,
    FuBru -> (io.pc + 4.U),
    FuLsu -> Mux(io.clint_ren, io.clint_rdata, rdataExt(io.dmem.rdata, io.op_type, io.dmem.addr)),
    FuCsr -> io.csr_data   
  ))

  when (io.csr_Jmp_en) { 
    io.br_en := io.csr_Jmp_en 
    io.br_addr := io.csr_Jmp_addr}
  .otherwise { 
    io.br_en := jmp_en 
    io.br_addr := jmp_addr}

  val mem_wdata = MuxLookup(io.op_type, 0.U(64.W), Array(
      LsuSb  ->  MuxLookup(io.dmem.addr(2,0),0.U(64.W),Array(
          "b000".U -> Cat(0.U(56.W), io.src2(7, 0)),
          "b001".U -> Cat(0.U(48.W), io.src2(7, 0), 0.U(8.W)),
          "b010".U -> Cat(0.U(40.W), io.src2(7, 0), 0.U(16.W)),
          "b011".U -> Cat(0.U(32.W), io.src2(7, 0), 0.U(24.W)),
          "b100".U -> Cat(0.U(24.W), io.src2(7, 0), 0.U(32.W)),
          "b101".U -> Cat(0.U(16.W), io.src2(7, 0), 0.U(40.W)),
          "b110".U -> Cat(0.U( 8.W), io.src2(7, 0), 0.U(48.W)),
          "b111".U -> Cat(io.src2(7, 0), 0.U(56.W))
      )),
      LsuSh  ->  MuxLookup(io.dmem.addr(2,1),0.U(64.W),Array(
          "b00".U -> Cat(0.U(48.W), io.src2(15, 0)),
          "b01".U -> Cat(0.U(32.W), io.src2(15, 0), 0.U(16.W)),
          "b10".U -> Cat(0.U(16.W), io.src2(15, 0), 0.U(32.W)),
          "b11".U -> Cat(io.src2(15, 0), 0.U(48.W))
      )),
      LsuSw  ->  MuxLookup(io.dmem.addr(2),0.U(64.W),Array(
          "b0".U -> Cat(0.U(32.W), io.src2(31, 0)),
          "b1".U -> Cat(io.src2(31, 0), 0.U(32.W))
      )),  
      LsuSd  -> io.src2    
  ))
  io.dmem.wdata  := mem_wdata
  io.clint_wdata := mem_wdata
  val mem_wmask  = MuxLookup(io.op_type, 0.U(64.W), Array(
      LsuSb  ->  MuxLookup(io.dmem.addr(2,0),0.U(64.W),Array(
          "b000".U -> "h0000_0000_0000_00FF".U,
          "b001".U -> "h0000_0000_0000_FF00".U,
          "b010".U -> "h0000_0000_00FF_0000".U,
          "b011".U -> "h0000_0000_FF00_0000".U,
          "b100".U -> "h0000_00FF_0000_0000".U,
          "b101".U -> "h0000_FF00_0000_0000".U,
          "b110".U -> "h00FF_0000_0000_0000".U,
          "b111".U -> "hFF00_0000_0000_0000".U   
      )) ,
      LsuSh  ->  MuxLookup(io.dmem.addr(2,1),0.U(64.W),Array(
          "b00".U -> "h0000_0000_0000_FFFF".U,
          "b01".U -> "h0000_0000_FFFF_0000".U,
          "b10".U -> "h0000_FFFF_0000_0000".U,
          "b11".U -> "hFFFF_0000_0000_0000".U  
      )) ,
      LsuSw  ->  MuxLookup(io.dmem.addr(2),0.U(64.W),Array(
          "b0".U -> "h0000_0000_FFFF_FFFF".U,
          "b1".U -> "hFFFF_FFFF_0000_0000".U
      )) , 
      LsuSd  ->  "hFFFF_FFFF_FFFF_FFFF".U     
  )) 

  io.dmem.wmask  := mem_wmask
  io.clint_wmask := mem_wmask

}
