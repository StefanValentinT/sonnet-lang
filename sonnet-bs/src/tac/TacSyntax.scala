package tac

import syntax.*

// instructions are typeless, only locations are typed
object Tac {
    case class Program(items: List[TopLevelItem])

    abstract sealed class TopLevelItem
    case class FunctionDef(name: String, isGlobal: Boolean, params: List[Var], body: List[Instruction]) extends TopLevelItem
    case class StaticVariable(name: String, isGlobal: Boolean, typ: Type, init: Const)                  extends TopLevelItem

    abstract sealed class Instruction
    case class Return(value: Val)                                          extends Instruction
    case class GetAddress(src: Val, dest: Val)                             extends Instruction
    case class AddPtr(ptr: Val, index: Val, scale: Int, dest: Val)         extends Instruction
    case class CopyToOffset(src: Val, dest: Val, offset: Int)              extends Instruction
    case class Load(src_ptr: Val, dest: Val)                               extends Instruction
    case class Store(src: Val, dest_ptr: Val)                              extends Instruction
    case class SignExtend(src: Val, dest: Val)                             extends Instruction
    case class ZeroExtend(src: Val, dest: Val)                             extends Instruction
    case class Truncate(src: Val, dest: Val)                               extends Instruction
    case class FloatToFloat(src: Val, dest: Val)                           extends Instruction
    case class SignedIntToFloat(src: Val, dest: Val)                       extends Instruction
    case class UnsignedIntToFloat(src: Val, dest: Val)                     extends Instruction
    case class FloatToSignedInt(src: Val, dest: Val)                       extends Instruction
    case class FloatToUnsignedInt(src: Val, dest: Val)                     extends Instruction
    case class Unary(unaryOp: UnaryOp, src: Val, dest: Val)                extends Instruction
    case class Binary(binaryOp: BinaryOp, src1: Val, src2: Val, dest: Val) extends Instruction
    case class Copy(src: Val, dest: Val)                                   extends Instruction
    case class Label(name: String)                                         extends Instruction
    case class Jump(target: Label)                                         extends Instruction
    case class JumpIfZero(cond: Val, target: Label)                        extends Instruction
    case class JumpIfNotZero(cond: Val, target: Label)                     extends Instruction
    case class FunctionCall(target: String, args: List[Val], dest: Val)    extends Instruction

    abstract sealed class Val
    case class Constant(value: Const)        extends Val
    case class Var(value: String, typ: Type) extends Val

    abstract sealed class Type
    case class I8()  extends Type
    case class I16() extends Type
    case class I32() extends Type
    case class I64() extends Type
    case class U8()  extends Type
    case class U16() extends Type
    case class U32() extends Type
    case class U64() extends Type
    case class F16() extends Type
    case class F32() extends Type
    case class F64() extends Type

    enum UnaryOp {
        case Complement, Negate, Not
    }

    enum BinaryOp {
        case Add, Subtract, Multiply, Divide, Remainder,
            And, Or, Equal, NotEqual, LessThan, LessOrEqual,
            GreaterThan, GreaterOrEqual,
            BitAnd, BitOr, BitXor, LShift, RShift
    }
}

def getTacValType(t: Tac.Val): Tac.Type = t match {
    case Tac.Var(_, t)                 => t
    case Tac.Constant(Const.I8Lit(_))  => Tac.I8()
    case Tac.Constant(Const.I16Lit(_)) => Tac.I16()
    case Tac.Constant(Const.I32Lit(_)) => Tac.I32()
    case Tac.Constant(Const.I64Lit(_)) => Tac.I64()

    case Tac.Constant(Const.U8Lit(_))  => Tac.U8()
    case Tac.Constant(Const.U16Lit(_)) => Tac.U16()
    case Tac.Constant(Const.U32Lit(_)) => Tac.U32()
    case Tac.Constant(Const.U64Lit(_)) => Tac.U64()

    case Tac.Constant(Const.F16Lit(_)) => Tac.F16()
    case Tac.Constant(Const.F32Lit(_)) => Tac.F32()
    case Tac.Constant(Const.F64Lit(_)) => Tac.F64()
}

def isSigned(t: Tac.Type): Boolean = t match {
    case Tac.I16() | Tac.I32() | Tac.I64() | Tac.I8() => true
    case Tac.U16() | Tac.U32() | Tac.U64() | Tac.U8() => false
}

def isFloat(t: Tac.Type): Boolean = t match {
    case Tac.F16() | Tac.F32() | Tac.F64() => true
    case _                                 => false
}
