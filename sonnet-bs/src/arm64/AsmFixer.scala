package arm64

import arm64.Asm
import syntax.Size
import scala.collection.mutable.{Map, ListBuffer}

object PseudoRegisterReplacer {
    private var globalSymbols = Map[String, Boolean]()
    private val stackMap      = Map[String, Int]()
    private var currentOffset = 0

    private def registerOperandOffsets(op: Asm.Operand): Unit = op match {
        case Asm.PseudoReg(name, size) =>
            if (!globalSymbols.getOrElse(name, false) && !stackMap.contains(name)) {
                currentOffset -= size.bytes
                stackMap.put(name, currentOffset)
            }
        case _ => ()
    }

    private def replaceOperand(op: Asm.Operand): Asm.Operand = op match {
        case Asm.PseudoReg(name, size) =>
            if (globalSymbols.getOrElse(name, false)) {
                Asm.Data(name, size)
            } else {
                stackMap.get(name) match {
                    case Some(offset) => Asm.StackSlot(offset, size)
                    case None =>
                        currentOffset -= size.bytes
                        stackMap.put(name, currentOffset)
                        Asm.StackSlot(currentOffset, size)
                }
            }
        case other => other
    }

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
        case imm: Asm.Imm8  => instr += Asm.Mov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Imm16 => instr += Asm.Mov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Imm32 => instr += Asm.Mov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Imm64 => instr += Asm.Mov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case _              => throw new RuntimeException("Unexpected operand")
    }

    def inProgram(p: Asm.Program): Asm.Program = {
        globalSymbols.clear()
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

    private def expandMov(src: Asm.Operand, dest: Asm.Operand): List[Asm.Instruction] = {
        val newSrc  = replaceOperand(src)
        val newDest = replaceOperand(dest)
        val size    = Asm.getOperandSize(newSrc)

        val scratchRegS    = if (size == Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
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

            case (imm: Asm.Imm8, destMem) if destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data] =>
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

            case (imm: Asm.Imm16, destMem) if destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data] =>
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

    private def expandExtend(srcSize: Size, isSigned: Boolean, src: Asm.Operand, dest: Asm.Operand): List[Asm.Instruction] = {
        val resolvedSrc  = replaceOperand(src)
        val resolvedDest = replaceOperand(dest)
        val buffer       = ListBuffer[Asm.Instruction]()

        val actualSrcSize  = Asm.getOperandSize(resolvedSrc)
        val actualDestSize = Asm.getOperandSize(resolvedDest)

        val scratchSrc  = if (actualSrcSize == Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
        val scratchDest = if (actualDestSize == Size.Byte8) Asm.Reg.X10 else Asm.Reg.W10

        val regSrc = ensureReg(resolvedSrc, scratchSrc, buffer)

        val targetReg = resolvedDest match {
            case r: Asm.Register => r
            case _               => Asm.Register(scratchDest)
        }

        (srcSize, isSigned) match {
            case (Size.Byte1, true)  => buffer += Asm.Sextb(regSrc, targetReg)
            case (Size.Byte2, true)  => buffer += Asm.Sexth(regSrc, targetReg)
            case (Size.Byte4, true)  => buffer += Asm.Sextw(regSrc, targetReg)
            case (Size.Byte1, false) => buffer += Asm.Uxtb(regSrc, targetReg)
            case (Size.Byte2, false) => buffer += Asm.Uxth(regSrc, targetReg)
            case (Size.Byte4, false) => buffer += Asm.Uxtw(regSrc, targetReg)
            case _                   => throw new RuntimeException(s"Unsupported extend source size: $srcSize")
        }

        if (resolvedDest.isInstanceOf[Asm.StackSlot]) {
            buffer += Asm.Store(targetReg, resolvedDest.asInstanceOf[Asm.StackSlot])
        } else if (resolvedDest.isInstanceOf[Asm.Data]) {
            val d              = resolvedDest.asInstanceOf[Asm.Data]
            val scratchAddrReg = Asm.Reg.X11
            buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
            buffer += Asm.StoreData(targetReg, d, Asm.Register(scratchAddrReg))
        }

        buffer.toList
    }

    private def replaceInFunction(f: Asm.FunctionDef): Asm.FunctionDef = {
        stackMap.clear()
        currentOffset = 0

        f.instructions.foreach {
            case Asm.Mov(src, dest)                  => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.Sextb(src, dest)                => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.Sexth(src, dest)                => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.Sextw(src, dest)                => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.Uxtb(src, dest)                 => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.Uxth(src, dest)                 => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.Uxtw(src, dest)                 => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.Unary(_, operand)               => registerOperandOffsets(operand)
            case Asm.Binary(_, s1, s2, d)            => registerOperandOffsets(s1); registerOperandOffsets(s2); registerOperandOffsets(d)
            case Asm.MultiplySubtract(s1, s2, s3, d) => registerOperandOffsets(s1); registerOperandOffsets(s2); registerOperandOffsets(s3); registerOperandOffsets(d)
            case Asm.Compare(s1, s2)                 => registerOperandOffsets(s1); registerOperandOffsets(s2)
            case Asm.ConditionalSet(_, destination)  => registerOperandOffsets(destination)
            case Asm.Push(src)                       => registerOperandOffsets(src)
            case _                                   => ()
        }

        val totalBytes   = currentOffset.abs
        val alignedBytes = if (totalBytes > 0) arm64.pad16(totalBytes) else 0

        var newInstructions = f.instructions.flatMap {
            case Asm.Mov(src, dest) => expandMov(src, dest)

            case Asm.Sextb(src, dest) => expandExtend(Size.Byte1, true, src, dest)
            case Asm.Sexth(src, dest) => expandExtend(Size.Byte2, true, src, dest)
            case Asm.Sextw(src, dest) => expandExtend(Size.Byte4, true, src, dest)

            case Asm.Uxtb(src, dest) => expandExtend(Size.Byte1, false, src, dest)
            case Asm.Uxth(src, dest) => expandExtend(Size.Byte2, false, src, dest)
            case Asm.Uxtw(src, dest) => expandExtend(Size.Byte4, false, src, dest)

            case Asm.Unary(op, operand) => {
                val buffer     = ListBuffer[Asm.Instruction]()
                val newOperand = replaceOperand(operand)
                val size       = Asm.getOperandSize(newOperand)
                val scratchReg = if (size == Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9

                newOperand match {
                    case slot: Asm.StackSlot =>
                        buffer += Asm.Load(slot, Asm.Register(scratchReg))
                        buffer += Asm.Unary(op, Asm.Register(scratchReg))
                        buffer += Asm.Store(Asm.Register(scratchReg), slot)
                    case data: Asm.Data =>
                        val reg = ensureReg(data, scratchReg, buffer)
                        buffer += Asm.Unary(op, reg)
                        val scratchAddrReg = Asm.Reg.X10
                        buffer += Asm.Adrp(Asm.Register(scratchAddrReg), data.location)
                        buffer += Asm.StoreData(reg, data, Asm.Register(scratchAddrReg))
                    case _ =>
                        buffer += Asm.Unary(op, newOperand)
                }
                buffer.toList
            }
            case Asm.Binary(op, s1, s2, d) => {
                val buffer     = ListBuffer[Asm.Instruction]()
                val resolvedS1 = replaceOperand(s1)
                val resolvedS2 = replaceOperand(s2)
                val finalD     = replaceOperand(d)

                val size      = Asm.getOperandSize(resolvedS1)
                val scratchS1 = if (size == Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
                val scratchS2 = if (size == Size.Byte8) Asm.Reg.X10 else Asm.Reg.W10
                val scratchD  = if (size == Size.Byte8) Asm.Reg.X11 else Asm.Reg.W11

                val regS1 = ensureReg(resolvedS1, scratchS1, buffer)
                val regS2 = ensureReg(resolvedS2, scratchS2, buffer)
                val regD = finalD match {
                    case r: Asm.Register => r
                    case _               => Asm.Register(scratchD)
                }

                buffer += Asm.Binary(op, regS1, regS2, regD)
                if (finalD.isInstanceOf[Asm.StackSlot]) {
                    buffer += Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot])
                } else if (finalD.isInstanceOf[Asm.Data]) {
                    val d              = finalD.asInstanceOf[Asm.Data]
                    val scratchAddrReg = Asm.Reg.X11
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(regD, d, Asm.Register(scratchAddrReg))
                }
                buffer.toList
            }
            case Asm.MultiplySubtract(s1, s2, s3, d) => {
                val buffer     = ListBuffer[Asm.Instruction]()
                val resolvedS1 = replaceOperand(s1)
                val resolvedS2 = replaceOperand(s2)
                val resolvedS3 = replaceOperand(s3)
                val finalD     = replaceOperand(d)

                val size      = Asm.getOperandSize(resolvedS1)
                val scratchS1 = if (size == Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
                val scratchS2 = if (size == Size.Byte8) Asm.Reg.X10 else Asm.Reg.W10
                val scratchS3 = if (size == Size.Byte8) Asm.Reg.X11 else Asm.Reg.W11

                val regS1 = ensureReg(resolvedS1, scratchS1, buffer)
                val regS2 = ensureReg(resolvedS2, scratchS2, buffer)
                val regS3 = ensureReg(resolvedS3, scratchS3, buffer)
                val regD  = if (finalD.isInstanceOf[Asm.Register]) finalD.asInstanceOf[Asm.Register] else Asm.Register(scratchS1)

                buffer += Asm.MultiplySubtract(regS1, regS2, regS3, regD)
                if (finalD.isInstanceOf[Asm.StackSlot]) {
                    buffer += Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot])
                } else if (finalD.isInstanceOf[Asm.Data]) {
                    val d              = finalD.asInstanceOf[Asm.Data]
                    val scratchAddrReg = Asm.Reg.X11
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(regD, d, Asm.Register(scratchAddrReg))
                }
                buffer.toList
            }
            case Asm.Compare(s1, s2) => {
                val buffer     = ListBuffer[Asm.Instruction]()
                val resolvedS1 = replaceOperand(s1)
                val finalS2    = replaceOperand(s2)

                val size      = Asm.getOperandSize(resolvedS1)
                val scratchS1 = if (size == Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9
                val scratchS2 = if (size == Size.Byte8) Asm.Reg.X10 else Asm.Reg.W10

                val regS1 = ensureReg(resolvedS1, scratchS1, buffer)
                val regS2 = ensureReg(finalS2, scratchS2, buffer)

                buffer += Asm.Compare(regS1, regS2)
                buffer.toList
            }
            case Asm.ConditionalSet(condition, destination) => {
                val finalDest = replaceOperand(destination)
                finalDest match {
                    case slot: Asm.StackSlot =>
                        List(
                          Asm.ConditionalSet(condition, Asm.Register(Asm.Reg.W9)),
                          Asm.Store(Asm.Register(Asm.Reg.W9), slot)
                        )
                    case data: Asm.Data =>
                        val scratchAddrReg = Asm.Reg.X10
                        List(
                          Asm.ConditionalSet(condition, Asm.Register(Asm.Reg.W9)),
                          Asm.Adrp(Asm.Register(scratchAddrReg), data.location),
                          Asm.StoreData(Asm.Register(Asm.Reg.W9), data, Asm.Register(scratchAddrReg))
                        )
                    case _ => List(Asm.ConditionalSet(condition, finalDest))
                }
            }
            case Asm.Call(target)          => List(Asm.Call(target))
            case Asm.DeallocateStack(size) => List(Asm.DeallocateStack(size))
            case Asm.Push(src) => {
                val newSrc     = replaceOperand(src)
                val size       = Asm.getOperandSize(newSrc)
                val scratchReg = if (size == Size.Byte8) Asm.Reg.X9 else Asm.Reg.W9

                newSrc match {
                    case slot: Asm.StackSlot =>
                        List(
                          Asm.Load(slot, Asm.Register(scratchReg)),
                          Asm.Push(Asm.Register(scratchReg))
                        )
                    case data: Asm.Data =>
                        val buffer = ListBuffer[Asm.Instruction]()
                        val reg    = ensureReg(data, scratchReg, buffer)
                        buffer += Asm.Push(reg)
                        buffer.toList
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
            case other => List(other)
        }

        if (alignedBytes > 0) {
            newInstructions = Asm.AllocateStack(alignedBytes) :: newInstructions
        }

        Asm.FunctionDef(f.name, f.isGlobal, newInstructions)
    }
}
