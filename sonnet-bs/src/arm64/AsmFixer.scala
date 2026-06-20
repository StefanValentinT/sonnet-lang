package arm64

import arm64.Asm
import syntax.Size
import scala.collection.mutable.{Map, ListBuffer}

object PseudoRegisterReplacer {
    private var globalSymbols  = Map[String, Boolean]()
    private val stackMap       = Map[String, Int]()
    private var currentOffset  = 0
    private var floatLitCount  = 0
    private val pooledLiterals = ListBuffer[Asm.StaticVariable]()

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

    private def getScratchReg(size: Size, isFloat: Boolean, baseIndex: Int): Asm.Reg = {
        if (isFloat) {
            size match {
                case Size.Byte2 => if (baseIndex == 9) Asm.Reg.H9 else Asm.Reg.H10
                case Size.Byte4 => if (baseIndex == 9) Asm.Reg.S9 else Asm.Reg.S10
                case Size.Byte8 => if (baseIndex == 9) Asm.Reg.D9 else Asm.Reg.D10
                case _          => Asm.Reg.S9
            }
        } else {
            if (size == Size.Byte8) (if (baseIndex == 9) Asm.Reg.X9 else Asm.Reg.X10)
            else (if (baseIndex == 9) Asm.Reg.W9
                  else Asm.Reg.W10)
        }
    }

    private def ensureReg(op: Asm.Operand, scratch: Asm.Reg, instr: ListBuffer[Asm.Instruction]): Asm.Register = op match {
        case r: Asm.Register     => r
        case slot: Asm.StackSlot =>
            expandLoadStore(Asm.Load(slot, Asm.Register(scratch)), instr)
            Asm.Register(scratch)
        case data: Asm.Data =>
            val scratch64 = scratch match {
                case Asm.Reg.W9 | Asm.Reg.X9 | Asm.Reg.H9 | Asm.Reg.S9 | Asm.Reg.D9      => Asm.Register(Asm.Reg.X9)
                case Asm.Reg.W10 | Asm.Reg.X10 | Asm.Reg.H10 | Asm.Reg.S10 | Asm.Reg.D10 => Asm.Register(Asm.Reg.X10)
                case Asm.Reg.W11 | Asm.Reg.X11                                           => Asm.Register(Asm.Reg.X11)
                case _                                                                   => throw new RuntimeException(s"No 64-bit address mapping for scratch register $scratch")
            }
            instr += Asm.Adrp(scratch64, data.location)
            instr += Asm.LoadData(data, scratch64, Asm.Register(scratch))
            Asm.Register(scratch)
        case imm: Asm.Imm8       => instr += Asm.Mov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Imm16      => instr += Asm.Mov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Imm32      => instr += Asm.Mov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Imm64      => instr += Asm.Mov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Float16Lit => instr += Asm.FMov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Float32Lit => instr += Asm.FMov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case imm: Asm.Float64Lit => instr += Asm.FMov(imm, Asm.Register(scratch)); Asm.Register(scratch)
        case _                   => throw new RuntimeException("Unexpected operand")
    }

    private def expandLoadStore(instr: Asm.Instruction, buffer: ListBuffer[Asm.Instruction]): Unit = instr match {
        case Asm.Load(slot, dest) =>
            val resolvedSlot = replaceOperand(slot).asInstanceOf[Asm.StackSlot]
            if (resolvedSlot.offset >= -256 && resolvedSlot.offset <= 255) {
                buffer += Asm.Load(resolvedSlot, dest)
            } else {
                buffer += Asm.Mov(Asm.Imm64(resolvedSlot.offset), Asm.Register(Asm.Reg.X16))
                buffer += Asm.LoadIndexed(dest, Asm.Register(Asm.Reg.X29), Asm.Register(Asm.Reg.X16), resolvedSlot.size)
            }
        case Asm.Store(src, slot) =>
            val resolvedSlot = replaceOperand(slot).asInstanceOf[Asm.StackSlot]
            if (resolvedSlot.offset >= -256 && resolvedSlot.offset <= 255) {
                buffer += Asm.Store(src, resolvedSlot)
            } else {
                buffer += Asm.Mov(Asm.Imm64(resolvedSlot.offset), Asm.Register(Asm.Reg.X16))
                buffer += Asm.StoreIndexed(src, Asm.Register(Asm.Reg.X29), Asm.Register(Asm.Reg.X16), resolvedSlot.size)
            }
        case _ => ()
    }

    def inProgram(p: Asm.Program): Asm.Program = {
        globalSymbols.clear()
        pooledLiterals.clear()
        floatLitCount = 0
        p.items.foreach {
            case v: Asm.StaticVariable => globalSymbols.put(v.name, v.isGlobal)
            case _                     => ()
        }
        val newItems = p.items.map {
            case f: Asm.FunctionDef            => replaceInFunction(f)
            case staticVar: Asm.StaticVariable => staticVar
        }
        Asm.Program(newItems ++ pooledLiterals.toList)
    }

    private def expandMov(src: Asm.Operand, dest: Asm.Operand, isF: Boolean): List[Asm.Instruction] = {
        val newSrc  = replaceOperand(src)
        val newDest = replaceOperand(dest)
        val size    = Asm.getOperandSize(newSrc)

        val scratchRegS    = getScratchReg(size, isFloat = isF, baseIndex = 9)
        val scratchAddrReg = Asm.Reg.X10

        (newSrc, newDest) match {
            case (imm, regDest: Asm.Register) if isF && (imm.isInstanceOf[Asm.Float16Lit] || imm.isInstanceOf[Asm.Float32Lit] || imm.isInstanceOf[Asm.Float64Lit]) =>
                val buffer  = ListBuffer[Asm.Instruction]()
                val litName = s"f_lit_${floatLitCount}"
                floatLitCount += 1

                val constVal = imm match {
                    case Asm.Float16Lit(fval) => syntax.Const.F16Lit(BigDecimal(fval))
                    case Asm.Float32Lit(fval) => syntax.Const.F32Lit(BigDecimal(fval))
                    case Asm.Float64Lit(dval) => syntax.Const.F64Lit(dval)
                    case _                    => throw new RuntimeException("Unreachable")
                }

                pooledLiterals += Asm.StaticVariable(litName, isGlobal = false, size, constVal)
                val dataOp = Asm.Data(litName, size)

                buffer += Asm.Adrp(Asm.Register(scratchAddrReg), dataOp.location)
                buffer += Asm.LoadData(dataOp, Asm.Register(scratchAddrReg), regDest)
                buffer.toList

            case (imm, destMem)
                if (imm.isInstanceOf[Asm.Float16Lit] || imm.isInstanceOf[Asm.Float32Lit] || imm.isInstanceOf[Asm.Float64Lit]) &&
                    (destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data]) =>
                val buffer  = ListBuffer[Asm.Instruction]()
                val litName = s"f_lit_${floatLitCount}"
                floatLitCount += 1

                val constVal = imm match {
                    case Asm.Float16Lit(fval) => syntax.Const.F16Lit(BigDecimal(fval))
                    case Asm.Float32Lit(fval) => syntax.Const.F32Lit(BigDecimal(fval))
                    case Asm.Float64Lit(dval) => syntax.Const.F64Lit(dval)
                    case _                    => throw new RuntimeException("Unreachable")
                }

                pooledLiterals += Asm.StaticVariable(litName, isGlobal = false, size, constVal)
                val dataOp = Asm.Data(litName, size)

                buffer += Asm.Adrp(Asm.Register(scratchAddrReg), dataOp.location)
                buffer += Asm.LoadData(dataOp, Asm.Register(scratchAddrReg), Asm.Register(scratchRegS))

                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    expandLoadStore(Asm.Store(Asm.Register(scratchRegS), destMem.asInstanceOf[Asm.StackSlot]), buffer)
                } else {
                    val d                = destMem.asInstanceOf[Asm.Data]
                    val secondScratchGPR = Asm.Reg.X11
                    buffer += Asm.Adrp(Asm.Register(secondScratchGPR), d.location)
                    buffer += Asm.StoreData(Asm.Register(scratchRegS), d, Asm.Register(secondScratchGPR))
                }
                buffer.toList

            case (srcMem, destMem)
                if (srcMem.isInstanceOf[Asm.StackSlot] || srcMem.isInstanceOf[Asm.Data]) &&
                    (destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data]) =>
                val buffer = ListBuffer[Asm.Instruction]()
                val regS   = ensureReg(srcMem, scratchRegS, buffer)
                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    expandLoadStore(Asm.Store(regS, destMem.asInstanceOf[Asm.StackSlot]), buffer)
                } else {
                    val d = destMem.asInstanceOf[Asm.Data]
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(regS, d, Asm.Register(scratchAddrReg))
                }
                buffer.toList

            case (imm: Asm.Imm8, destMem) if destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data] =>
                val buffer = ListBuffer[Asm.Instruction]()
                if (isF) buffer += Asm.FMov(imm, Asm.Register(scratchRegS))
                else buffer += Asm.Mov(imm, Asm.Register(scratchRegS))
                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    expandLoadStore(Asm.Store(Asm.Register(scratchRegS), destMem.asInstanceOf[Asm.StackSlot]), buffer)
                } else {
                    val d = destMem.asInstanceOf[Asm.Data]
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(Asm.Register(scratchRegS), d, Asm.Register(scratchAddrReg))
                }
                buffer.toList

            case (imm: Asm.Imm16, destMem) if destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data] =>
                val buffer = ListBuffer[Asm.Instruction]()
                if (isF) buffer += Asm.FMov(imm, Asm.Register(scratchRegS))
                else buffer += Asm.Mov(imm, Asm.Register(scratchRegS))
                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    expandLoadStore(Asm.Store(Asm.Register(scratchRegS), destMem.asInstanceOf[Asm.StackSlot]), buffer)
                } else {
                    val d = destMem.asInstanceOf[Asm.Data]
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(Asm.Register(scratchRegS), d, Asm.Register(scratchAddrReg))
                }
                buffer.toList

            case (imm: Asm.Imm32, destMem) if destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data] =>
                val buffer = ListBuffer[Asm.Instruction]()
                if (isF) buffer += Asm.FMov(imm, Asm.Register(scratchRegS))
                else buffer += Asm.Mov(imm, Asm.Register(scratchRegS))
                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    expandLoadStore(Asm.Store(Asm.Register(scratchRegS), destMem.asInstanceOf[Asm.StackSlot]), buffer)
                } else {
                    val d = destMem.asInstanceOf[Asm.Data]
                    buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
                    buffer += Asm.StoreData(Asm.Register(scratchRegS), d, Asm.Register(scratchAddrReg))
                }
                buffer.toList

            case (imm: Asm.Imm64, destMem) if destMem.isInstanceOf[Asm.StackSlot] || destMem.isInstanceOf[Asm.Data] =>
                val buffer = ListBuffer[Asm.Instruction]()
                if (isF) buffer += Asm.FMov(imm, Asm.Register(scratchRegS))
                else buffer += Asm.Mov(imm, Asm.Register(scratchRegS))
                if (destMem.isInstanceOf[Asm.StackSlot]) {
                    expandLoadStore(Asm.Store(Asm.Register(scratchRegS), destMem.asInstanceOf[Asm.StackSlot]), buffer)
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
                val buffer = ListBuffer[Asm.Instruction]()
                expandLoadStore(Asm.Load(slot, regDest), buffer)
                buffer.toList

            case (regSrc: Asm.Register, slot: Asm.StackSlot) =>
                val buffer = ListBuffer[Asm.Instruction]()
                expandLoadStore(Asm.Store(regSrc, slot), buffer)
                buffer.toList

            case (regSrc: Asm.Register, data: Asm.Data) =>
                List(
                  Asm.Adrp(Asm.Register(scratchAddrReg), data.location),
                  Asm.StoreData(regSrc, data, Asm.Register(scratchAddrReg))
                )

            case _ =>
                if (isF) List(Asm.FMov(newSrc, newDest))
                else List(Asm.Mov(newSrc, newDest))
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
            expandLoadStore(Asm.Store(targetReg, resolvedDest.asInstanceOf[Asm.StackSlot]), buffer)
        } else if (resolvedDest.isInstanceOf[Asm.Data]) {
            val d              = resolvedDest.asInstanceOf[Asm.Data]
            val scratchAddrReg = Asm.Reg.X11
            buffer += Asm.Adrp(Asm.Register(scratchAddrReg), d.location)
            buffer += Asm.StoreData(targetReg, d, Asm.Register(scratchAddrReg))
        }

        buffer.toList
    }

    private def expandConversion(
        src: Asm.Operand,
        dest: Asm.Operand,
        srcFloat: Boolean,
        destFloat: Boolean,
        build: (Asm.Register, Asm.Register) => Asm.Instruction
    ): List[Asm.Instruction] = {
        val buffer       = ListBuffer[Asm.Instruction]()
        val resolvedSrc  = replaceOperand(src)
        val resolvedDest = replaceOperand(dest)

        val srcSize  = Asm.getOperandSize(resolvedSrc)
        val destSize = Asm.getOperandSize(resolvedDest)

        val regSrc = ensureReg(resolvedSrc, getScratchReg(srcSize, srcFloat, 9), buffer)
        val regDest =
            if (resolvedDest.isInstanceOf[Asm.Register]) resolvedDest.asInstanceOf[Asm.Register]
            else Asm.Register(getScratchReg(destSize, destFloat, 10))

        buffer += build(regSrc, regDest)
        if (resolvedDest.isInstanceOf[Asm.StackSlot]) {
            expandLoadStore(Asm.Store(regDest, resolvedDest.asInstanceOf[Asm.StackSlot]), buffer)
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
            case Asm.FMov(src, dest)                 => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.FBinary(_, s1, s2, d)           => registerOperandOffsets(s1); registerOperandOffsets(s2); registerOperandOffsets(d)
            case Asm.FCompare(s1, s2)                => registerOperandOffsets(s1); registerOperandOffsets(s2)
            case Asm.FpToFp(src, dest)               => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.SignedToFp(src, dest)           => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.UnsignedToFp(src, dest)         => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.FpToSigned(src, dest)           => registerOperandOffsets(src); registerOperandOffsets(dest)
            case Asm.FpToUnsigned(src, dest)         => registerOperandOffsets(src); registerOperandOffsets(dest)
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
            case Asm.Mov(src, dest)  => expandMov(src, dest, false)
            case Asm.FMov(src, dest) => expandMov(src, dest, true)

            case Asm.Load(src, dest) =>
                val buffer = ListBuffer[Asm.Instruction]()
                expandLoadStore(Asm.Load(src, dest), buffer)
                buffer.toList

            case Asm.Store(src, dest) =>
                val buffer = ListBuffer[Asm.Instruction]()
                expandLoadStore(Asm.Store(src, dest), buffer)
                buffer.toList

            case Asm.FBinary(op, s1, s2, d) =>
                val buffer     = ListBuffer[Asm.Instruction]()
                val resolvedS1 = replaceOperand(s1)
                val resolvedS2 = replaceOperand(s2)
                val finalD     = replaceOperand(d)
                val size       = Asm.getOperandSize(resolvedS1)

                val regS1 = ensureReg(resolvedS1, getScratchReg(size, isFloat = true, 9), buffer)
                val regS2 = ensureReg(resolvedS2, getScratchReg(size, isFloat = true, 10), buffer)
                val regD  = if (finalD.isInstanceOf[Asm.Register]) finalD.asInstanceOf[Asm.Register] else Asm.Register(getScratchReg(size, isFloat = true, 9))

                buffer += Asm.FBinary(op, regS1, regS2, regD)
                if (finalD.isInstanceOf[Asm.StackSlot]) {
                    expandLoadStore(Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot]), buffer)
                }
                buffer.toList

            case Asm.FCompare(s1, s2) =>
                val buffer     = ListBuffer[Asm.Instruction]()
                val resolvedS1 = replaceOperand(s1)
                val finalS2    = replaceOperand(s2)
                val size       = Asm.getOperandSize(resolvedS1)

                val regS1 = ensureReg(resolvedS1, getScratchReg(size, isFloat = true, 9), buffer)
                val regS2 = ensureReg(finalS2, getScratchReg(size, isFloat = true, 10), buffer)

                buffer += Asm.FCompare(regS1, regS2)
                buffer.toList

            case Asm.FpToFp(src, dest)       => expandConversion(src, dest, srcFloat = true, destFloat = true, Asm.FpToFp(_, _))
            case Asm.SignedToFp(src, dest)   => expandConversion(src, dest, srcFloat = false, destFloat = true, Asm.SignedToFp(_, _))
            case Asm.UnsignedToFp(src, dest) => expandConversion(src, dest, srcFloat = false, destFloat = true, Asm.UnsignedToFp(_, _))
            case Asm.FpToSigned(src, dest)   => expandConversion(src, dest, srcFloat = true, destFloat = false, Asm.FpToSigned(_, _))
            case Asm.FpToUnsigned(src, dest) => expandConversion(src, dest, srcFloat = true, destFloat = false, Asm.FpToUnsigned(_, _))

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
                        expandLoadStore(Asm.Load(slot, Asm.Register(scratchReg)), buffer)
                        buffer += Asm.Unary(op, Asm.Register(scratchReg))
                        expandLoadStore(Asm.Store(Asm.Register(scratchReg), slot), buffer)
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
                    expandLoadStore(Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot]), buffer)
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
                    expandLoadStore(Asm.Store(regD, finalD.asInstanceOf[Asm.StackSlot]), buffer)
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
                        val buffer = ListBuffer[Asm.Instruction]()
                        buffer += Asm.ConditionalSet(condition, Asm.Register(Asm.Reg.W9))
                        expandLoadStore(Asm.Store(Asm.Register(Asm.Reg.W9), slot), buffer)
                        buffer.toList
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
                        val buffer = ListBuffer[Asm.Instruction]()
                        expandLoadStore(Asm.Load(slot, Asm.Register(scratchReg)), buffer)
                        buffer += Asm.Push(Asm.Register(scratchReg))
                        buffer.toList
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
