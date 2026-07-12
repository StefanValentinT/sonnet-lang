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
    case class Mov(src: Operand, dest: Operand)                                             extends Instruction
    case class Load(src: StackSlot, dest: Register)                                         extends Instruction
    case class Store(src: Register, dest: StackSlot)                                        extends Instruction
    case class LoadIndexed(dest: Register, base: Register, offsetReg: Register, size: Size) extends Instruction
    case class StoreIndexed(src: Register, base: Register, offsetReg: Register, size: Size) extends Instruction
    case class GetAddress(src: Operand, dest: Operand)                                      extends Instruction
    case class LoadIndirect(srcPtr: Operand, dest: Operand)                                 extends Instruction
    case class StoreIndirect(srcVal: Operand, destPtr: Operand)                             extends Instruction
    case class LoadLabelAddr(dest: Register, label: String)                                 extends Instruction
    case class Adrp(dest: Register, label: String)                                          extends Instruction
    case class LoadData(src: Data, baseReg: Register, dest: Register)                       extends Instruction
    case class StoreData(src: Register, dest: Data, baseReg: Register)                      extends Instruction
    case class Unary(op: UnaryOp, operand: Operand)                                         extends Instruction
    case class Sextb(src: Operand, dest: Operand)                                           extends Instruction
    case class Sexth(src: Operand, dest: Operand)                                           extends Instruction
    case class Sextw(src: Operand, dest: Operand)                                           extends Instruction
    case class Uxtb(src: Operand, dest: Operand)                                            extends Instruction
    case class Uxth(src: Operand, dest: Operand)                                            extends Instruction
    case class Uxtw(src: Operand, dest: Operand)                                            extends Instruction
    case class AllocateStack(size: Int)                                                     extends Instruction
    case class DeallocateStack(size: Int)                                                   extends Instruction
    case class Push(src: Operand)                                                           extends Instruction
    case class Call(target: String)                                                         extends Instruction
    case class Ret()                                                                        extends Instruction
    case class Binary(op: BinaryOp, src1: Operand, src2: Operand, dest: Operand)            extends Instruction
    // dest = src3 - (src1 * src2)
    case class MultiplySubtract(src1: Operand, src2: Operand, src3: Operand, dest: Operand) extends Instruction
    case class Compare(source1: Operand, source2: Operand)                                  extends Instruction
    case class ConditionalSet(condition: ConditionCode, destination: Operand)               extends Instruction
    case class ConditionalBranch(condition: ConditionCode, targetLabel: String)             extends Instruction
    case class Branch(targetLabel: String)                                                  extends Instruction
    case class Label(name: String)                                                          extends Instruction

    case class FMov(src: Operand, dest: Operand)                                   extends Instruction
    case class FBinary(op: FBinaryOp, src1: Operand, src2: Operand, dest: Operand) extends Instruction
    case class FCompare(source1: Operand, source2: Operand)                        extends Instruction

    case class FpToFp(src: Operand, dest: Operand)       extends Instruction
    case class SignedToFp(src: Operand, dest: Operand)   extends Instruction
    case class UnsignedToFp(src: Operand, dest: Operand) extends Instruction
    case class FpToSigned(src: Operand, dest: Operand)   extends Instruction
    case class FpToUnsigned(src: Operand, dest: Operand) extends Instruction

    abstract sealed class Operand
    case class Imm8(value: Int)                           extends Operand
    case class Imm16(value: Long)                         extends Operand
    case class Imm32(value: Int)                          extends Operand
    case class Imm64(value: Long)                         extends Operand
    case class Float16Lit(value: Float)                   extends Operand
    case class Float32Lit(value: Float)                   extends Operand
    case class Float64Lit(value: Double)                  extends Operand
    case class Register(reg: Reg)                         extends Operand
    case class PseudoReg(name: String, size: Size)        extends Operand
    case class PseudoMem(name: String, size: Size)        extends Operand
    case class Indexed(base: Reg, index: Reg, scale: Int) extends Operand
    case class StackSlot(offset: Int, size: Size)         extends Operand
    case class Data(location: String, size: Size)         extends Operand

    def getOperandSize(op: Asm.Operand): Size = op match {
        case Asm.Imm8(_)  => Size.Byte1
        case Asm.Imm16(_) => Size.Byte2
        case Asm.Imm32(_) => Size.Byte4
        case Asm.Imm64(_) => Size.Byte8

        case Asm.Float16Lit(_) => Size.Byte2
        case Asm.Float32Lit(_) => Size.Byte4
        case Asm.Float64Lit(_) => Size.Byte8

        case Asm.Register(reg) =>
            reg match {
                case Asm.Reg.X0 | Asm.Reg.X1 | Asm.Reg.X2 | Asm.Reg.X3 | Asm.Reg.X4 | Asm.Reg.X5 | Asm.Reg.X6 | Asm.Reg.X7 | Asm.Reg.X9 | Asm.Reg.X10 | Asm.Reg.X11 | Asm.Reg.XZR | Asm.Reg.X16 | Asm.Reg.X17 | Asm.Reg.X30 | Asm.Reg.D0 | Asm.Reg.D1 | Asm.Reg.D2 | Asm.Reg.D3 | Asm.Reg.D4 | Asm.Reg.D5 | Asm.Reg.D6 | Asm.Reg.D7 | Asm.Reg.D9 | Asm.Reg.D10 | Asm.Reg.D11 =>
                    Size.Byte8

                case Asm.Reg.W0 | Asm.Reg.W1 | Asm.Reg.W2 | Asm.Reg.W3 | Asm.Reg.W4 | Asm.Reg.W5 | Asm.Reg.W6 | Asm.Reg.W7 | Asm.Reg.W9 | Asm.Reg.W10 | Asm.Reg.W11 | Asm.Reg.WZR | Asm.Reg.S0 | Asm.Reg.S1 | Asm.Reg.S2 | Asm.Reg.S3 | Asm.Reg.S4 | Asm.Reg.S5 | Asm.Reg.S6 | Asm.Reg.S7 | Asm.Reg.S9 | Asm.Reg.S10 | Asm.Reg.S11 =>
                    Size.Byte4

                case Asm.Reg.H0 | Asm.Reg.H1 | Asm.Reg.H2 | Asm.Reg.H3 | Asm.Reg.H4 | Asm.Reg.H5 | Asm.Reg.H6 | Asm.Reg.H7 | Asm.Reg.H9 | Asm.Reg.H10 | Asm.Reg.H11 =>
                    Size.Byte2
            }
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
            XZR, X29,

            // intra-procedure-call
            X16, X17,

            // Link register
            X30

        // 16-bit floating point registers
        case H0, H1, H2, H3, H4, H5, H6, H7, H9, H10, H11
        // 32-bit floating point registers
        case S0, S1, S2, S3, S4, S5, S6, S7, S9, S10, S11
        // 64-bit floating point registers
        case D0, D1, D2, D3, D4, D5, D6, D7, D9, D10, D11

        def to64: Reg = this match {
            case W0        => X0; case W1  => X1; case W2   => X2; case W3   => X3
            case W4        => X4; case W5  => X5; case W6   => X6; case W7   => X7
            case W9        => X9; case W10 => X10; case W11 => X11; case WZR => XZR
            case S0        => D0; case S1  => D1; case S2   => D2; case S3   => D3
            case S4        => D4; case S5  => D5; case S6   => D6; case S7   => D7
            case S9        => D9; case S10 => D10; case S11 => D11
            case H0        => D0
            case already64 => already64
        }

        def to32: Reg = this match {
            case X0        => W0; case X1  => W1; case X2   => W2; case X3   => W3
            case X4        => W4; case X5  => W5; case X6   => W6; case X7   => W7
            case X9        => W9; case X10 => W10; case X11 => W11; case XZR => WZR
            case D0        => S0; case D1  => S1; case D2   => S2; case D3   => S3
            case D4        => S4; case D5  => S5; case D6   => S6; case D7   => S7
            case D9        => S9; case D10 => S10; case D11 => S11
            case already32 => already32
        }

        def to16: Reg = this match {
            case D0 | S0   => H0; case D1 | S1   => H1; case D2 | S2    => H2; case D3 | S3 => H3
            case D4 | S4   => H4; case D5 | S5   => H5; case D6 | S6    => H6; case D7 | S7 => H7
            case D9 | S9   => H9; case D10 | S10 => H10; case D11 | S11 => H11
            case already16 => already16
        }

    }
    val paramRegisters = List(Asm.Reg.W0, Asm.Reg.W1, Asm.Reg.W2, Asm.Reg.W3, Asm.Reg.W4, Asm.Reg.W5, Asm.Reg.W6, Asm.Reg.W7)

    val floatParamRegisters = List(Asm.Reg.D0, Asm.Reg.D1, Asm.Reg.D2, Asm.Reg.D3, Asm.Reg.D4, Asm.Reg.D5, Asm.Reg.D6, Asm.Reg.D7)

    def selectParamRegister(index: Int, t: Tac.Type): Asm.Reg = {
        t match {
            case Tac.F16() => Asm.floatParamRegisters(index).to16
            case Tac.F32() => Asm.floatParamRegisters(index).to32
            case Tac.F64() => Asm.floatParamRegisters(index)
            case _ =>
                val base = Asm.paramRegisters(index)
                t match {
                    case Tac.I8() | Tac.U8() | Tac.I16() | Tac.U16() | Tac.I32() | Tac.U32() => base.to32
                    case Tac.I64() | Tac.U64()                                               => base.to64
                }
        }
    }

    enum UnaryOp {
        case Neg, Not
    }

    enum BinaryOp { case Add, Sub, Mult, Div, UDiv, BitAnd, BitOr, BitXor, Lsl, Asr, Lsr }

    enum FBinaryOp {
        case FAdd, FSub, FMul, FDiv
    }

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
