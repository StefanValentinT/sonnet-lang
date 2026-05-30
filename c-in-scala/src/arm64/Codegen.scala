package arm64

import tac.Tac
import scala.collection.mutable.{Map, ListBuffer}

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
    case Tac.Copy(src, dest) =>
        List(
          Asm.Mov(codegenTacVal(src), codegenTacVal(dest))
        )

    case Tac.Unary(Tac.UnaryOp.Not, src, dest) =>
        List(
          Asm.Compare(codegenTacVal(src), Asm.Register(Asm.Reg.WZR)),
          Asm.ConditionalSet(Asm.ConditionCode.Equal, codegenTacVal(dest))
        )

    case Tac.Unary(op, src, dest) => {
        val asmOp  = convertUnOp(op)
        val asmSrc = codegenTacVal(src)
        val asmDst = codegenTacVal(dest)

        List(
          Asm.Mov(asmSrc, asmDst),
          Asm.Unary(asmOp, asmDst)
        )
    }
    case Tac.Binary(Tac.BinaryOp.Remainder, src1, src2, dest) => {
        val s1 = codegenTacVal(src1)
        val s2 = codegenTacVal(src2)
        val d  = codegenTacVal(dest)

        val pseudoQuotient = d match {
            case Asm.PseudoReg(name) => Asm.PseudoReg(s"${name}_quot")
            case _                   => Asm.PseudoReg("rem_quot_temp")
        }

        List(
          Asm.Binary(Asm.BinaryOp.Div, s1, s2, pseudoQuotient),
          Asm.MultiplySubtract(pseudoQuotient, s2, s1, d)
        )
    }
    case Tac.Binary(op, src1, src2, dest) if isRelationalOp(op) => {
        List(
          Asm.Compare(codegenTacVal(src1), codegenTacVal(src2)),
          Asm.ConditionalSet(convertConditionCode(op), codegenTacVal(dest))
        )
    }
    case Tac.Binary(op, src1, src2, dest) =>
        List(
          Asm.Binary(convertBinOp(op), codegenTacVal(src1), codegenTacVal(src2), codegenTacVal(dest))
        )

    case Tac.Label(name) =>
        List(Asm.Label(name))

    case Tac.Jump(target) =>
        List(Asm.Branch(target.name))

    case Tac.JumpIfZero(cond, target) =>
        List(
          Asm.Compare(codegenTacVal(cond), Asm.Register(Asm.Reg.WZR)),
          Asm.ConditionalBranch(Asm.ConditionCode.Equal, target.name)
        )

    case Tac.JumpIfNotZero(cond, target) =>
        List(
          Asm.Compare(codegenTacVal(cond), Asm.Register(Asm.Reg.WZR)),
          Asm.ConditionalBranch(Asm.ConditionCode.NotEqual, target.name)
        )
}

private def convertConditionCode(op: Tac.BinaryOp): Asm.ConditionCode = op match {
    case Tac.BinaryOp.Equal          => Asm.ConditionCode.Equal
    case Tac.BinaryOp.NotEqual       => Asm.ConditionCode.NotEqual
    case Tac.BinaryOp.LessThan       => Asm.ConditionCode.LessThan
    case Tac.BinaryOp.LessOrEqual    => Asm.ConditionCode.LessOrEqual
    case Tac.BinaryOp.GreaterThan    => Asm.ConditionCode.GreaterThan
    case Tac.BinaryOp.GreaterOrEqual => Asm.ConditionCode.GreaterOrEqual
    case _                           => throw new RuntimeException(s"$op is not a relational comparison operator.")
}

def codegenTacVal(v: Tac.Val): Asm.Operand = v match {
    case Tac.Constant(ival) => Asm.Imm(ival)
    case Tac.Var(name)      => Asm.PseudoReg(name)
}

private def convertUnOp(op: Tac.UnaryOp): Asm.UnaryOp = op match {
    case Tac.UnaryOp.Complement => Asm.UnaryOp.Not
    case Tac.UnaryOp.Negate     => Asm.UnaryOp.Neg
}

private def convertBinOp(op: Tac.BinaryOp): Asm.BinaryOp = op match {
    case Tac.BinaryOp.Add      => Asm.BinaryOp.Add
    case Tac.BinaryOp.Subtract => Asm.BinaryOp.Sub
    case Tac.BinaryOp.Multiply => Asm.BinaryOp.Mult
    case Tac.BinaryOp.Divide   => Asm.BinaryOp.Div
    case _                     => throw new RuntimeException(s"Operation $op cannot be mapped directly to simple BinaryOp")
}

private def isRelationalOp(op: Tac.BinaryOp): Boolean = op match {
    case Tac.BinaryOp.Equal | Tac.BinaryOp.NotEqual | Tac.BinaryOp.LessThan | Tac.BinaryOp.LessOrEqual | Tac.BinaryOp.GreaterThan | Tac.BinaryOp.GreaterOrEqual => true
    case _                                                                                                                                                      => false
}

class PseudoRegisterReplacer {

    private val stackMap      = Map[String, Int]()
    private var currentOffset = 0

    // Ensures that an operand is in a register.
    // If that is not the case it is loaded into the egister specified by the second parameter.
    // It modifies the ListBuffer it receives to contain the instructions necessary to load
    // and returns a register operand that contains the operand.
    private def ensureReg(op: Asm.Operand, scratch: Asm.Reg, instr: ListBuffer[Asm.Instruction]): Asm.Register = op match {
        case r: Asm.Register => r
        case slot: Asm.StackSlot => {
            instr += Asm.Load(slot, Asm.Register(scratch))
            Asm.Register(scratch)
        }
        case imm: Asm.Imm => {
            instr += Asm.Mov(imm, Asm.Register(scratch))
            Asm.Register(scratch)
        }
        case _ => throw new RuntimeException("Unexpected operand")
    }

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
                case Asm.Binary(op, s1, s2, d) => {
                    val buffer = ListBuffer[Asm.Instruction]()
                    val regS1  = ensureReg(replaceOperand(s1), Asm.Reg.W9, buffer)
                    val regS2  = ensureReg(replaceOperand(s2), Asm.Reg.W10, buffer)
                    val finalD = replaceOperand(d)

                    val regD = finalD match {
                        case r: Asm.Register => r
                        case _               => Asm.Register(Asm.Reg.W11)
                    }

                    buffer += Asm.Binary(op, regS1, regS2, regD)
                    if (finalD.isInstanceOf[Asm.StackSlot]) {
                        buffer += Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot])
                    }
                    buffer.toList
                }
                case Asm.MultiplySubtract(s1, s2, s3, d) => {
                    val buffer = ListBuffer[Asm.Instruction]()
                    val regS1  = ensureReg(replaceOperand(s1), Asm.Reg.W9, buffer)
                    val regS2  = ensureReg(replaceOperand(s2), Asm.Reg.W10, buffer)
                    val regS3  = ensureReg(replaceOperand(s3), Asm.Reg.W11, buffer)
                    val finalD = replaceOperand(d)

                    val regD = if (finalD.isInstanceOf[Asm.Register]) finalD.asInstanceOf[Asm.Register] else Asm.Register(Asm.Reg.W9)

                    buffer += Asm.MultiplySubtract(regS1, regS2, regS3, regD)
                    if (finalD.isInstanceOf[Asm.StackSlot]) {
                        buffer += Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot])
                    }
                    buffer.toList
                }
                case Asm.Compare(s1, s2) => {
                    val buffer  = ListBuffer[Asm.Instruction]()
                    val regS1   = ensureReg(replaceOperand(s1), Asm.Reg.W9, buffer)
                    val finalS2 = replaceOperand(s2)
                    val fixedS2 = finalS2 match {
                        case slot: Asm.StackSlot => {
                            buffer += Asm.Load(slot, Asm.Register(Asm.Reg.W10))
                            Asm.Register(Asm.Reg.W10)
                        }
                        case allowed => allowed
                    }

                    buffer += Asm.Compare(regS1, fixedS2)
                    buffer.toList
                }

                case Asm.ConditionalSet(condition, destination) => {
                    val finalDest = replaceOperand(destination)
                    finalDest match {
                        case slot: Asm.StackSlot => {
                            List(
                              Asm.ConditionalSet(condition, Asm.Register(Asm.Reg.W9)),
                              Asm.Store(Asm.Register(Asm.Reg.W9), slot)
                            )
                        }
                        case _ => {
                            List(Asm.ConditionalSet(condition, finalDest))
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
