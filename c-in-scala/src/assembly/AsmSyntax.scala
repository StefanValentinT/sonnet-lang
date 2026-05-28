package assembly

case class AsmProgram(item: AsmFunctionDef)

case class AsmFunctionDef(name: String, instructions: List[AsmInstruction])

abstract class AsmInstruction
case class Mov(src: AsmOperand, dest: AsmOperand) extends AsmInstruction
case class Ret()                                  extends AsmInstruction

abstract class AsmOperand
case class Imm(value: Int) extends AsmOperand
case class Register()      extends AsmOperand
