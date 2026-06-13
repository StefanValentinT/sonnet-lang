package arm64

import syntax.Const
import tac.Tac
import syntax.Size

object Asm {
    case class Program(items: List[TopLevelItem])

    abstract sealed class TopLevelItem
    case class FunctionDef(name: String, isGlobal: Boolean, instructions: List[Instruction]) extends TopLevelItem
    case class StaticVariable(name: String, isGlobal: Boolean, alignment: Size, init: Const) extends TopLevelItem

    abstract sealed class Instruction
    case class Mov(src: Operand, dest: Operand)                                  extends Instruction
    case class Load(src: StackSlot, dest: Register)                              extends Instruction
    case class Store(src: Register, dest: StackSlot)                             extends Instruction
    case class Adrp(dest: Register, label: String)                               extends Instruction
    case class LoadData(src: Data, baseReg: Register, dest: Register)            extends Instruction
    case class StoreData(src: Register, dest: Data, baseReg: Register)           extends Instruction
    case class Unary(op: UnaryOp, operand: Operand)                              extends Instruction
    case class Sextb(src: Operand, dest: Operand)                                extends Instruction
    case class Sexth(src: Operand, dest: Operand)                                extends Instruction
    case class Sextw(src: Operand, dest: Operand)                                extends Instruction
    case class Uxtb(src: Operand, dest: Operand)                                 extends Instruction
    case class Uxth(src: Operand, dest: Operand)                                 extends Instruction
    case class Uxtw(src: Operand, dest: Operand)                                 extends Instruction
    case class AllocateStack(size: Int)                                          extends Instruction
    case class DeallocateStack(size: Int)                                        extends Instruction
    case class Push(src: Operand)                                                extends Instruction
    case class Call(target: String)                                              extends Instruction
    case class Ret()                                                             extends Instruction
    case class Binary(op: BinaryOp, src1: Operand, src2: Operand, dest: Operand) extends Instruction
    // dest = src3 - (src1 * src2)
    case class MultiplySubtract(src1: Operand, src2: Operand, src3: Operand, dest: Operand) extends Instruction
    case class Compare(source1: Operand, source2: Operand)                                  extends Instruction
    case class ConditionalSet(condition: ConditionCode, destination: Operand)               extends Instruction
    case class ConditionalBranch(condition: ConditionCode, targetLabel: String)             extends Instruction
    case class Branch(targetLabel: String)                                                  extends Instruction
    case class Label(name: String)                                                          extends Instruction

    abstract sealed class Operand
    case class Imm8(value: Int)                    extends Operand
    case class Imm16(value: Long)                  extends Operand
    case class Imm32(value: Int)                   extends Operand
    case class Imm64(value: Long)                  extends Operand
    case class Register(reg: Reg)                  extends Operand
    case class PseudoReg(name: String, size: Size) extends Operand
    case class StackSlot(offset: Int, size: Size)  extends Operand
    case class Data(location: String, size: Size)  extends Operand

    def getOperandSize(op: Asm.Operand): Size = op match {
        case Asm.Imm8(_)            => Size.Byte1
        case Asm.Imm16(_)           => Size.Byte2
        case Asm.Imm32(_)           => Size.Byte4
        case Asm.Imm64(_)           => Size.Byte8
        case Asm.Register(reg)      => if (reg == reg.to64) Size.Byte8 else Size.Byte4
        case Asm.PseudoReg(_, size) => size
        case Asm.StackSlot(_, size) => size
        case Asm.Data(_, size)      => size
    }

    enum Reg {
        // 32-bit registers
        case W0, W1, W2, W3, W4, W5, W6, W7,
            W9, W10, W11,
            WZR,

            // 64-bit counterparts
            X0, X1, X2, X3, X4, X5, X6, X7,
            X9, X10, X11,
            XZR,

            // intra-procedure-call
            X16, X17,

            // Link register
            X30

        def to64: Reg = this match {
            case W0        => X0; case W1  => X1; case W2   => X2; case W3 => X3
            case W4        => X4; case W5  => X5; case W6   => X6; case W7 => X7
            case W9        => X9; case W10 => X10; case W11 => X11
            case WZR       => XZR
            case already64 => already64
        }

        def to32: Reg = this match {
            case X0        => W0; case X1  => W1; case X2   => W2; case X3 => W3
            case X4        => W4; case X5  => W5; case X6   => W6; case X7 => W7
            case X9        => W9; case X10 => W10; case X11 => W11
            case XZR       => WZR
            case already32 => already32
        }

    }
    val paramRegisters = List(Asm.Reg.W0, Asm.Reg.W1, Asm.Reg.W2, Asm.Reg.W3, Asm.Reg.W4, Asm.Reg.W5, Asm.Reg.W6, Asm.Reg.W7)

    def selectParamRegister(index: Int, t: Tac.Type): Asm.Reg = {
        val base = Asm.paramRegisters(index)
        t match {
            case Tac.I8() | Tac.U8()   => base.to32
            case Tac.I16() | Tac.U16() => base.to32
            case Tac.I32() | Tac.U32() => base.to32
            case Tac.I64() | Tac.U64() => base.to64
        }
    }

    enum UnaryOp {
        case Neg, Not
    }

    enum BinaryOp { case Add, Sub, Mult, Div, UDiv, BitAnd, BitOr, BitXor, Lsl, Asr, Lsr }

    enum ConditionCode {
        case Equal,
            NotEqual,

            // Signed
            LessThan,
            LessOrEqual,
            GreaterThan,
            GreaterOrEqual,

            // Unsigned
            CarryClear,
            LowerOrSame,
            CarrySet,
            Higher,
    }
}
