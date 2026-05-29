package arm64

import tac.Tac
import scala.collection.mutable.Map

def codegenProgram(p: Tac.Program): Asm.Program =
    Asm.Program(codegenFunction(p.items))

def codegenFunction(f: Tac.FunctionDef): Asm.FunctionDef =
    val asmInstructions = f.body.flatMap(codegenInstruction)
    Asm.FunctionDef(f.name, asmInstructions)

def codegenInstruction(ins: Tac.Instruction): List[Asm.Instruction] = ins match {
    case Tac.Return(value) =>
        List(
          Asm.Mov(codegenTacVal(value), Asm.Register(Asm.Reg.W0)),
          Asm.Ret()
        )

    case Tac.Unary(op, src, dest) =>
        val asmOp  = convertUnOp(op)
        val asmSrc = codegenTacVal(src)
        val asmDst = codegenTacVal(dest)

        List(
          Asm.Mov(asmSrc, asmDst),
          Asm.Unary(asmOp, asmDst)
        )
}

def codegenTacVal(v: Tac.Val): Asm.Operand = v match {
    case Tac.Constant(ival) => Asm.Imm(ival)
    case Tac.Var(name)      => Asm.PseudoReg(name)
}

private def convertUnOp(op: Tac.UnaryOp): Asm.UnaryOp = op match {
    case Tac.UnaryOp.Complement => Asm.UnaryOp.Not
    case Tac.UnaryOp.Negate     => Asm.UnaryOp.Neg
}

class PseudoRegisterReplacer {

    private val stackMap      = Map[String, Int]()
    private var currentOffset = 0

    def inProgram(p: Asm.Program): Asm.Program = {
        val newFun = replaceInFunction(p.items)
        Asm.Program(newFun)
    }

    private def replaceOperand(op: Asm.Operand): Asm.Operand = {
        op match {
            case Asm.PseudoReg(name) => {
                stackMap.get(name) match {
                    case Some(offset) => Asm.StackSlot(offset)
                    case None => {
                        currentOffset -= 4
                        stackMap.put(name, currentOffset)
                        Asm.StackSlot(currentOffset)
                    }
                }
            }
            case other => other
        }
    }

    private def replaceInFunction(f: Asm.FunctionDef): Asm.FunctionDef = {
        stackMap.clear()
        currentOffset = 0

        var newInstructions = f.instructions.flatMap {
            {
                case Asm.Mov(src, dest) => {
                    val newSrc  = replaceOperand(src)
                    val newDest = replaceOperand(dest)

                    (newSrc, newDest) match {
                        case (srcSlot: Asm.StackSlot, destSlot: Asm.StackSlot) => {
                            List(
                              Asm.Load(srcSlot, Asm.Register(Asm.Reg.W9)),
                              Asm.Store(Asm.Register(Asm.Reg.W9), destSlot)
                            )
                        }
                        case (srcSlot: Asm.StackSlot, regDest: Asm.Register) => {
                            List(Asm.Load(srcSlot, regDest))
                        }
                        case (regSrc: Asm.Register, destSlot: Asm.StackSlot) => {
                            List(Asm.Store(regSrc, destSlot))
                        }
                        case (imm: Asm.Imm, destSlot: Asm.StackSlot) => {
                            List(
                              Asm.Mov(imm, Asm.Register(Asm.Reg.W9)),
                              Asm.Store(Asm.Register(Asm.Reg.W9), destSlot)
                            )
                        }
                        case _ => {
                            List(Asm.Mov(newSrc, newDest))
                        }
                    }
                }
                case Asm.Unary(op, operand) => {
                    val newOperand = replaceOperand(operand)

                    newOperand match {
                        case slot: Asm.StackSlot => {
                            List(
                              Asm.Load(slot, Asm.Register(Asm.Reg.W9)),
                              Asm.Unary(op, Asm.Register(Asm.Reg.W9)),
                              Asm.Store(Asm.Register(Asm.Reg.W9), slot)
                            )
                        }
                        case _ => {
                            List(Asm.Unary(op, newOperand))
                        }
                    }
                }
                case other => {
                    List(other)
                }
            }
        }

        val totalBytes = currentOffset.abs

        if (totalBytes > 0) {
            newInstructions = Asm.AllocateStack(totalBytes) :: newInstructions
        }

        Asm.FunctionDef(f.name, newInstructions)
    }
}
