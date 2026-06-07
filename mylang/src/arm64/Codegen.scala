package arm64

import tac.Tac
import tac.getTacValType
import scala.collection.mutable.{Map, ListBuffer}
import arm64.Asm.Size

def codegenProgram(p: Tac.Program): Asm.Program = {
    val items = p.items.map {
        case f: Tac.FunctionDef => codegenFunction(f)
        case v: Tac.StaticVariable => {
            val s = v.typ match {
                case Tac.I32() => Asm.Size.Byte4
                case Tac.I64() => Asm.Size.Byte8
            }
            Asm.StaticVariable(v.name, v.isGlobal, s, v.init)
        }
    }
    Asm.Program(items)
}

def codegenFunction(f: Tac.FunctionDef): Asm.FunctionDef = {
    val paramRegisters = List(Asm.Reg.W0, Asm.Reg.W1, Asm.Reg.W2, Asm.Reg.W3, Asm.Reg.W4, Asm.Reg.W5, Asm.Reg.W6, Asm.Reg.W7)

    val paramMoves = f.params.zipWithIndex.flatMap { case (paramVar, index) =>
        val asmType = paramVar.typ match {
            case Tac.I32() => Asm.I32()
            case Tac.I64() => Asm.I64()
        }
        val size = Asm.Size.fromTType(paramVar.typ)
        if (index < 8) {
            List(Asm.Mov(Asm.Register(Asm.selectParamRegister(index, asmType)), Asm.PseudoReg(paramVar.value, size)))
        } else {
            val incomingOffset = ((index - 8) * 8) + 16
            List(Asm.Mov(Asm.StackSlot(incomingOffset, size), Asm.PseudoReg(paramVar.value, size)))
        }
    }
    val asmInstructions = paramMoves ++ f.body.flatMap(codegenInstruction)
    Asm.FunctionDef(f.name, f.isGlobal, asmInstructions)
}

def codegenInstruction(ins: Tac.Instruction): List[Asm.Instruction] = ins match {
    case Tac.Return(value) =>
        val t = targetType(getTacValType(value))
        List(
          Asm.Mov(codegenTacVal(value), Asm.Register(Asm.selectParamRegister(0, t))),
          Asm.Ret()
        )

    case Tac.Copy(src, dest) =>
        List(Asm.Mov(codegenTacVal(src), codegenTacVal(dest)))

    case Tac.SignExtend(src, dest) =>
        List(Asm.Sextw(codegenTacVal(src), codegenTacVal(dest)))

    case Tac.Truncate(src, dest) =>
        List(Asm.Mov(codegenTacVal(src), codegenTacVal(dest)))

    case Tac.Unary(Tac.UnaryOp.Not, src, dest) =>
        val t       = targetType(getTacValType(src))
        val zeroReg = if (t == Asm.I64()) Asm.Reg.XZR else Asm.Reg.WZR
        List(
          Asm.Compare(codegenTacVal(src), Asm.Register(zeroReg)),
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
        val t  = targetType(getTacValType(dest))
        val s1 = codegenTacVal(src1)
        val s2 = codegenTacVal(src2)
        val d  = codegenTacVal(dest)

        val pseudoQuotient = d match {
            case Asm.PseudoReg(name, typ) => Asm.PseudoReg(s"${name}_quot", typ)
            case _                        => Asm.PseudoReg("rem_quot_temp", Size.fromType(t))
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

    case Tac.Binary(op, src1, src2, dest) => {
        List(
          Asm.Binary(convertBinOp(op), codegenTacVal(src1), codegenTacVal(src2), codegenTacVal(dest))
        )
    }

    case Tac.Label(name) =>
        List(Asm.Label(name))

    case Tac.Jump(target) =>
        List(Asm.Branch(target.name))

    case Tac.JumpIfZero(cond, target) => {
        val t       = targetType(getTacValType(cond))
        val zeroReg = if (t == Asm.I64()) Asm.Reg.XZR else Asm.Reg.WZR
        List(
          Asm.Compare(codegenTacVal(cond), Asm.Register(zeroReg)),
          Asm.ConditionalBranch(Asm.ConditionCode.Equal, target.name)
        )
    }

    case Tac.JumpIfNotZero(cond, target) => {
        val t       = targetType(getTacValType(cond))
        val zeroReg = if (t == Asm.I64()) Asm.Reg.XZR else Asm.Reg.WZR
        List(
          Asm.Compare(codegenTacVal(cond), Asm.Register(zeroReg)),
          Asm.ConditionalBranch(Asm.ConditionCode.NotEqual, target.name)
        )
    }

    case Tac.FunctionCall(target, args, dest) => {
        val destType = targetType(getTacValType(dest))

        val argSetup = args.zipWithIndex.flatMap { case (argVal, index) =>
            val argType = targetType(getTacValType(argVal))
            if (index < 8) {
                List(Asm.Mov(codegenTacVal(argVal), Asm.Register(Asm.selectParamRegister(index, argType))))
            } else {
                List(Asm.Push(codegenTacVal(argVal)))
            }
        }

        val stackCleanup = if (args.length > 8) {
            val stackBytes   = (args.length - 8) * 8
            val alignedBytes = pad16(stackBytes)
            List(Asm.DeallocateStack(alignedBytes))
        } else {
            Nil
        }

        argSetup ++ List(Asm.Call(target)) ++ stackCleanup ++ List(
          Asm.Mov(Asm.Register(Asm.selectParamRegister(0, destType)), codegenTacVal(dest))
        )
    }
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

private def targetType(t: Tac.Type): Asm.Type = t match {
    case Tac.I32() => Asm.I32()
    case Tac.I64() => Asm.I64()
}

def codegenTacVal(v: Tac.Val): Asm.Operand = v match {
    case Tac.Constant(syntax.Const.I32Lit(ival)) => Asm.Imm32(ival)
    case Tac.Constant(syntax.Const.I64Lit(lval)) => Asm.Imm64(lval)
    case Tac.Var(name, tacTyp)                   => Asm.PseudoReg(name, Size.fromTType(tacTyp))
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
    case Tac.BinaryOp.BitAnd   => Asm.BinaryOp.BitAnd
    case Tac.BinaryOp.BitOr    => Asm.BinaryOp.BitOr
    case Tac.BinaryOp.BitXor   => Asm.BinaryOp.BitXor
    case Tac.BinaryOp.LShift   => Asm.BinaryOp.Lsl
    case Tac.BinaryOp.RShift   => Asm.BinaryOp.Asr
    case _                     => throw new RuntimeException(s"Operation $op cannot be mapped directly to simple BinaryOp")
}

private def isRelationalOp(op: Tac.BinaryOp): Boolean = op match {
    case Tac.BinaryOp.Equal | Tac.BinaryOp.NotEqual | Tac.BinaryOp.LessThan | Tac.BinaryOp.LessOrEqual | Tac.BinaryOp.GreaterThan | Tac.BinaryOp.GreaterOrEqual => true
    case _                                                                                                                                                      => false
}

object PseudoRegisterReplacer {
    private var globalSymbols = Map[String, Boolean]()

    private val stackMap      = Map[String, Int]()
    private var currentOffset = 0

    // Ensures that an operand is in a register.
    // If that is not the case it is loaded into the register specified by the second parameter.
    // It modifies the ListBuffer it receives to contain the instructions necessary to load
    // and returns a register operand that contains the operand.
    private def ensureReg(op: Asm.Operand, scratch: Asm.Reg, instr: ListBuffer[Asm.Instruction]): Asm.Register = op match {
        case r: Asm.Register => r
        case slot: Asm.StackSlot =>
            instr += Asm.Load(slot, Asm.Register(scratch))
            Asm.Register(scratch)
        case data: Asm.Data =>
            val scratch64 = scratch match {
                case Asm.Reg.W9 | Asm.Reg.X9   => Asm.Register(Asm.Reg.X9)
                case Asm.Reg.W10 | Asm.Reg.X10 => Asm.Register(Asm.Reg.X10)
                case Asm.Reg.W11 | Asm.Reg.X11 => Asm.Register(Asm.Reg.X11)
                case _                         => throw new RuntimeException(s"No 64-bit address mapping for scratch register $scratch")
            }
            instr += Asm.Adrp(scratch64, data.location)
            instr += Asm.LoadData(data, scratch64, Asm.Register(scratch))
            Asm.Register(scratch)
        case imm: Asm.Imm32 =>
            instr += Asm.Mov(imm, Asm.Register(scratch))
            Asm.Register(scratch)
        case imm: Asm.Imm64 =>
            instr += Asm.Mov(imm, Asm.Register(scratch))
            Asm.Register(scratch)
        case _ => throw new RuntimeException("Unexpected operand")
    }

    def inProgram(p: Asm.Program): Asm.Program = {
        p.items.foreach {
            case v: Asm.StaticVariable => globalSymbols.put(v.name, v.isGlobal)
            case _                     => ()
        }
        val newItems = p.items.map {
            case f: Asm.FunctionDef            => replaceInFunction(f)
            case staticVar: Asm.StaticVariable => staticVar
        }
        Asm.Program(newItems)
    }

    private def replaceOperand(op: Asm.Operand): Asm.Operand = {
        op match {
            case Asm.PseudoReg(name, size) => {
                if (globalSymbols.getOrElse(name, false)) {
                    Asm.Data(name, size)
                } else {
                    stackMap.get(name) match {
                        case Some(offset) => Asm.StackSlot(offset, size)
                        case None => {
                            currentOffset -= size.bytes
                            stackMap.put(name, currentOffset)
                            Asm.StackSlot(currentOffset, size)
                        }
                    }
                }
            }
            case other => other
        }
    }

    // fix the mov instruction to operate on registers and stackslots
    private def expandMov(src: Asm.Operand, dest: Asm.Operand): List[Asm.Instruction] = {
        val newSrc  = replaceOperand(src)
        val newDest = replaceOperand(dest)

        val size           = Asm.getOperandSize(newSrc)
        val scratchRegS    = if (size == Asm.Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
        val scratchAddrReg = Asm.Reg.X10

        (newSrc, newDest) match {
            case (srcMem, destMem)
                if (srcMem.isInstanceOf[Asm.StackSlot] || srcMem.isInstanceOf[Asm.Data]) &&
                    (destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data]) =>
                val buffer = ListBuffer[Asm.Instruction]()
                val regS   = ensureReg(srcMem, scratchRegS, buffer)
                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    buffer += Asm.Store(regS, destMem.asInstanceOf[Asm.StackSlot])
                } else {
                    val d = destMem.asInstanceOf[Asm.Data]
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(regS, d, Asm.Register(scratchAddrReg))
                }
                buffer.toList

            case (imm: Asm.Imm32, destMem) if destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data] =>
                val buffer = ListBuffer[Asm.Instruction]()
                buffer += Asm.Mov(imm, Asm.Register(scratchRegS))

                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    buffer += Asm.Store(Asm.Register(scratchRegS), destMem.asInstanceOf[Asm.StackSlot])
                } else {
                    val d = destMem.asInstanceOf[Asm.Data]
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(Asm.Register(scratchRegS), d, Asm.Register(scratchAddrReg))
                }
                buffer.toList

            case (imm: Asm.Imm64, destMem) if destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data] =>
                val buffer = ListBuffer[Asm.Instruction]()
                buffer += Asm.Mov(imm, Asm.Register(scratchRegS))

                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    buffer += Asm.Store(Asm.Register(scratchRegS), destMem.asInstanceOf[Asm.StackSlot])
                } else {
                    val d = destMem.asInstanceOf[Asm.Data]
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(Asm.Register(scratchRegS), d, Asm.Register(scratchAddrReg))
                }
                buffer.toList

            case (data: Asm.Data, regDest: Asm.Register) =>
                List(
                  Asm.Adrp(Asm.Register(scratchAddrReg), data.location),
                  Asm.LoadData(data, Asm.Register(scratchAddrReg), regDest)
                )

            case (slot: Asm.StackSlot, regDest: Asm.Register) =>
                List(Asm.Load(slot, regDest))

            case (regSrc: Asm.Register, data: Asm.Data) =>
                List(
                  Asm.Adrp(Asm.Register(scratchAddrReg), data.location),
                  Asm.StoreData(regSrc, data, Asm.Register(scratchAddrReg))
                )

            case (regSrc: Asm.Register, slot: Asm.StackSlot) =>
                List(Asm.Store(regSrc, slot))

            case _ =>
                List(Asm.Mov(newSrc, newDest))
        }
    }

    private def replaceInFunction(f: Asm.FunctionDef): Asm.FunctionDef = {
        stackMap.clear()
        currentOffset = 0

        f.instructions.foreach {
            case Asm.Mov(src, dest)                  => replaceOperand(src); replaceOperand(dest)
            case Asm.Unary(_, operand)               => replaceOperand(operand)
            case Asm.Binary(_, s1, s2, d)            => replaceOperand(s1); replaceOperand(s2); replaceOperand(d)
            case Asm.MultiplySubtract(s1, s2, s3, d) => replaceOperand(s1); replaceOperand(s2); replaceOperand(s3); replaceOperand(d)
            case Asm.Compare(s1, s2)                 => replaceOperand(s1); replaceOperand(s2)
            case Asm.ConditionalSet(_, destination)  => replaceOperand(destination)
            case Asm.Push(src)                       => replaceOperand(src)
            case _                                   => ()
        }

        val totalBytes   = currentOffset.abs
        val alignedBytes = if (totalBytes > 0) pad16(totalBytes) else 0

        var newInstructions = f.instructions.flatMap {
            {
                case Asm.Mov(src, dest) => expandMov(src, dest)
                case Asm.Unary(op, operand) => {
                    val newOperand = replaceOperand(operand)
                    val size       = Asm.getOperandSize(newOperand)
                    val scratchReg = if (size == Asm.Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9

                    newOperand match {
                        case slot: Asm.StackSlot => {
                            List(
                              Asm.Load(slot, Asm.Register(scratchReg)),
                              Asm.Unary(op, Asm.Register(scratchReg)),
                              Asm.Store(Asm.Register(scratchReg), slot)
                            )
                        }
                        case _ => List(Asm.Unary(op, newOperand))
                    }
                }
                case Asm.Binary(op, s1, s2, d) => {
                    val buffer    = ListBuffer[Asm.Instruction]()
                    val size      = Asm.getOperandSize(replaceOperand(s1))
                    val scratchS1 = if (size == Asm.Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
                    val scratchS2 = if (size == Asm.Size.Byte8) Asm.Reg.X10 else Asm.Reg.W10
                    val scratchD  = if (size == Asm.Size.Byte8) Asm.Reg.X11 else Asm.Reg.W11

                    val regS1  = ensureReg(replaceOperand(s1), scratchS1, buffer)
                    val regS2  = ensureReg(replaceOperand(s2), scratchS2, buffer)
                    val finalD = replaceOperand(d)

                    val regD = finalD match {
                        case r: Asm.Register => r
                        case _               => Asm.Register(scratchD)
                    }

                    buffer += Asm.Binary(op, regS1, regS2, regD)
                    if (finalD.isInstanceOf[Asm.StackSlot]) {
                        buffer += Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot])
                    }
                    buffer.toList
                }
                case Asm.MultiplySubtract(s1, s2, s3, d) => {
                    val buffer    = ListBuffer[Asm.Instruction]()
                    val size      = Asm.getOperandSize(replaceOperand(s1))
                    val scratchS1 = if (size == Asm.Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
                    val scratchS2 = if (size == Asm.Size.Byte8) Asm.Reg.X10 else Asm.Reg.W10
                    val scratchS3 = if (size == Asm.Size.Byte8) Asm.Reg.X11 else Asm.Reg.W11

                    val regS1  = ensureReg(replaceOperand(s1), scratchS1, buffer)
                    val regS2  = ensureReg(replaceOperand(s2), scratchS2, buffer)
                    val regS3  = ensureReg(replaceOperand(s3), scratchS3, buffer)
                    val finalD = replaceOperand(d)

                    val regD = if (finalD.isInstanceOf[Asm.Register]) finalD.asInstanceOf[Asm.Register] else Asm.Register(scratchS1)

                    buffer += Asm.MultiplySubtract(regS1, regS2, regS3, regD)
                    if (finalD.isInstanceOf[Asm.StackSlot]) {
                        buffer += Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot])
                    }
                    buffer.toList
                }
                case Asm.Compare(s1, s2) => {
                    val buffer    = ListBuffer[Asm.Instruction]()
                    val size      = Asm.getOperandSize(replaceOperand(s1))
                    val scratchS1 = if (size == Asm.Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
                    val scratchS2 = if (size == Asm.Size.Byte8) Asm.Reg.X10 else Asm.Reg.W10

                    val regS1   = ensureReg(replaceOperand(s1), scratchS1, buffer)
                    val finalS2 = replaceOperand(s2)
                    val fixedS2 = finalS2 match {
                        case slot: Asm.StackSlot => {
                            buffer += Asm.Load(slot, Asm.Register(scratchS2))
                            Asm.Register(scratchS2)
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
                        case _ => List(Asm.ConditionalSet(condition, finalDest))
                    }
                }
                case Asm.Call(target)          => List(Asm.Call(target))
                case Asm.DeallocateStack(size) => List(Asm.DeallocateStack(size))
                case Asm.Push(src) => {
                    val newSrc     = replaceOperand(src)
                    val size       = Asm.getOperandSize(newSrc)
                    val scratchReg = if (size == Asm.Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9

                    newSrc match {
                        case slot: Asm.StackSlot =>
                            List(
                              Asm.Load(slot, Asm.Register(scratchReg)),
                              Asm.Push(Asm.Register(scratchReg))
                            )
                        case other =>
                            List(Asm.Push(other))
                    }
                }
                case Asm.Ret() =>
                    if (alignedBytes > 0) {
                        List(Asm.DeallocateStack(alignedBytes), Asm.Ret())
                    } else {
                        List(Asm.Ret())
                    }
                case other => {
                    List(other)
                }

            }
        }

        if (alignedBytes > 0) {
            newInstructions = Asm.AllocateStack(alignedBytes) :: newInstructions
        }

        Asm.FunctionDef(f.name, f.isGlobal, newInstructions)
    }
}
