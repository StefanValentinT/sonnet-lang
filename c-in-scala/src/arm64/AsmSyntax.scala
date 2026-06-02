package arm64

object Asm {
    case class Program(items: List[FunctionDef])

    case class FunctionDef(name: String, instructions: List[Instruction])

    abstract sealed class Instruction
    case class Mov(src: Operand, dest: Operand)                                  extends Instruction
    case class Load(src: StackSlot, dest: Register)                              extends Instruction
    case class Store(src: Register, dest: StackSlot)                             extends Instruction
    case class Unary(op: UnaryOp, operand: Operand)                              extends Instruction
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
    case class Imm(value: Int)         extends Operand
    case class Register(reg: Reg)      extends Operand
    case class PseudoReg(name: String) extends Operand
    case class StackSlot(offset: Int)  extends Operand

    enum Reg {
        // return value and parameter registers
        case W0, W1, W2, W3, W4, W5, W6, W7,
            // temporary registers
            W9, W10, W11,
            // zero register
            WZR,
            // link register
            X30
    }
    val paramRegisters = List(Asm.Reg.W0, Asm.Reg.W1, Asm.Reg.W2, Asm.Reg.W3, Asm.Reg.W4, Asm.Reg.W5, Asm.Reg.W6, Asm.Reg.W7)

    enum UnaryOp {
        case Neg, Not
    }

    enum BinaryOp { case Add, Sub, Mult, Div, BitAnd, BitOr, BitXor, Lsl, Asr }

    enum ConditionCode {
        case Equal,
            NotEqual,
            LessThan,
            LessOrEqual,
            GreaterThan,
            GreaterOrEqual
    }
}
