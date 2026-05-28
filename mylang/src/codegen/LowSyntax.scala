package codegen

case class LowProgram(functions: List[LowFunction])

case class LowFunction(name: String, instructions: List[LowInstruction])

class LowInstruction
case class Mov(src: LowOperand, dest: LowOperand) extends LowInstruction
case class Ret()                                  extends LowInstruction

class LowOperand
case class Immediate(value: Int) extends LowOperand
case class Register()            extends LowOperand
