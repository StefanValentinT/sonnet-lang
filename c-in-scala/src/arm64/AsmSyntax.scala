package arm64

object Asm {
    case class Program(items: FunctionDef)

    case class FunctionDef(name: String, instructions: List[Instruction])

    abstract sealed class Instruction
    case class Mov(src: Operand, dest: Operand)                                  extends Instruction
    case class Load(src: StackSlot, dest: Register)                              extends Instruction
    case class Store(src: Register, dest: StackSlot)                             extends Instruction
    case class Unary(op: UnaryOp, operand: Operand)                              extends Instruction
    case class AllocateStack(size: Int)                                          extends Instruction
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
        case W0, W9, W10, W11, WZR
    }

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
