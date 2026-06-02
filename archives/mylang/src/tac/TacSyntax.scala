package tac

case class TacProgram(items: List[TacTopLevel])

abstract class TacTopLevel
case class TacFunctionDef(name: String, global:Boolean, params: List[String], body: List[TacInstruction])


abstract class TacInstruction
case class Return(value: TacValue) extends TacInstruction
case class SignExtend(src: TacValue, dest:TacValue) extends TacInstruction
case class ZeroExtend(src: TacValue, dest:TacValue) extends TacInstruction
case class F64ToI32(src: TacValue, dest:TacValue) extends TacInstruction
case class I32ToF64(src: TacValue, dest:TacValue) extends TacInstruction
case class F64ToU32(src: TacValue, dest:TacValue) extends TacInstruction
case class U32ToF64(src: TacValue, dest:TacValue) extends TacInstruction
case class Truncate(src: TacValue, dest:TacValue) extends TacInstruction
case class Unary(op: TacUnaryOp, src: TacValue, dest:TacValue) extends TacInstruction
case class Binary(op: TacBinaryOp, src1: TacValue, src2: TacValue, dest: TacValue) extends TacInstruction
case class Copy(src: TacValue, dest: TacValue) extends TacInstruction
case class GetAddress(src: TacValue, dest: TacValue) extends TacInstruction
case class Load(srcPtr: TacValue, dest: TacValue) extends TacInstruction
case class Store(src: TacValue, destPtr: TacValue) extends TacInstruction
case class AddPtr(ptr: TacValue, index: TacValue, scale: Int, dest: TacValue) extends TacInstruction
case class CopyToOffset(src: TacValue, dest: String, offset: Int) extends TacInstruction
case class CopyFromOffset(src: String, offset: Int, dest: TacValue) extends TacInstruction
case class Jump(label: String) extends TacInstruction
case class JumpIfZero(cond: TacValue, label: String) extends TacInstruction
case class JumpIfNotZero(cond: TacValue, label: String) extends TacInstruction
case class Label(name: String) extends TacInstruction
case class FunCall(f: String, args: List[TacValue], dest: Option[TacValue]) extends TacInstruction

abstract class TacValue
case class Constant(c: TacConst) extends TacValue

abstract class TacConst
case class ConstI8(value: Byte) extends TacConst
case class ConstI16(value: Short) extends TacConst
case class ConstI32(value: Int) extends TacConst
case class ConstI64(value: Long) extends TacConst
case class ConstU8(value: Short) extends TacConst
case class ConstU16(value: Int) extends TacConst
case class ConstU32(value: Long) extends TacConst
case class ConstU64(value: BigInt) extends TacConst
case class ConstF16(bits: Short) extends TacConst
case class ConstF32(value: Float) extends TacConst
case class ConstF64(value: Double) extends TacConst

abstract class TacUnaryOp
case class TacComplement() extends TacUnaryOp
case class TacNegate() extends TacUnaryOp
case class TacNot() extends TacUnaryOp

abstract class TacBinaryOp
case class TacAdd() extends TacBinaryOp
case class TacSubtract() extends TacBinaryOp
case class TacMultiply() extends TacBinaryOp
case class TacDivide() extends TacBinaryOp
case class TacMod() extends TacBinaryOp
case class TacEqual() extends TacBinaryOp
case class TacNotEqual() extends TacBinaryOp
case class TacLessThan() extends TacBinaryOp
case class TacLessOrEqual() extends TacBinaryOp
case class TacGreaterThan() extends TacBinaryOp
case class TacGreaterOrEqual() extends TacBinaryOp
