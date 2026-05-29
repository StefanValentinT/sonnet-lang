package arm64

object Asm {
    case class Program(items: FunctionDef)

    case class FunctionDef(name: String, instructions: List[Instruction])

    abstract sealed class Instruction
    case class Mov(src: Operand, dest: Operand)      extends Instruction
    case class Load(src: StackSlot, dest: Register)  extends Instruction
    case class Store(src: Register, dest: StackSlot) extends Instruction
    case class Unary(op: UnaryOp, operand: Operand)  extends Instruction
    case class AllocateStack(size: Int)              extends Instruction
    case class Ret()                                 extends Instruction

    abstract sealed class Operand
    case class Imm(value: Int)         extends Operand
    case class Register(reg: Reg)      extends Operand
    case class PseudoReg(name: String) extends Operand
    case class StackSlot(offset: Int)  extends Operand

    enum Reg {
        case W0, W9
    }

    enum UnaryOp {
        case Neg, Not
    }
}
