package arm64

import syntax.Size
import syntax.Const

class Emitter() {
    val sb = StringBuilder()

    def inst(s: String) = sb.append("    ").append(s)

    def emitProgram(p: Asm.Program): String = {
        val staticVars                     = p.items.collect { case v: Asm.StaticVariable => v }
        val (initialized, zeroInitialized) = staticVars.partition(v => !v.init.isZero)

        if (initialized.nonEmpty) {
            sb.append(".data\n")
            initialized.foreach {
                case Asm.StaticVariable(name, isGlobal, alignment, init) => {
                    if (isGlobal) {
                        sb.append(s".global _${name}\n")
                    }

                    val asmDirective = init match {
                        case Const.F16Lit(_) => ".short"
                        case Const.F32Lit(_) => ".float"
                        case Const.F64Lit(_) => ".double"
                        case _ =>
                            alignment match {
                                case Size.Byte1 => ".byte"
                                case Size.Byte2 => ".short"
                                case Size.Byte4 => ".word"
                                case Size.Byte8 => ".quad"
                            }
                    }

                    sb.append(s".p2align ${calculateP2Align(alignment.bytes)}\n")
                    sb.append(s"_$name:\n")

                    val valueStr = init match {
                        // output the selfmade bit pattern as defined by IEEE-754
                        case Const.F16Lit(f) =>
                            val floatBits = java.lang.Float.floatToIntBits(f.floatValue)
                            val sign      = (floatBits >>> 16) & 0x8000
                            val exp       = (floatBits >>> 23) & 0xff
                            val fraction  = floatBits & 0x007fffff

                            val f16Bits = if (exp == 0) {
                                sign
                            } else if (exp == 0xff) {
                                sign | 0x7c00 | (if (fraction != 0) 0x0200 else 0) // NaN / Infinity
                            } else {
                                val newExp = exp - 127 + 15
                                if (newExp >= 31) {
                                    sign | 0x7c00
                                } else if (newExp <= 0) {
                                    sign
                                } else {
                                    sign | (newExp << 10) | (fraction >>> 13)
                                }
                            }
                            s"0x${Integer.toHexString(f16Bits & 0xffff)}"
                        case other => other.getValueStr
                    }

                    sb.append(s"    $asmDirective $valueStr\n\n")
                }
            }
        }
        if (zeroInitialized.nonEmpty) {
            sb.append(".bss\n")
            zeroInitialized.foreach {
                case Asm.StaticVariable(name, isGlobal, alignment, init) => {
                    if (isGlobal) {
                        sb.append(s".global _${name}\n")
                    }
                    sb.append(s".p2align ${calculateP2Align(alignment.bytes)}\n")
                    sb.append(s"_${name}:\n")
                    sb.append(s"    .space ${alignment.bytes}\n\n")
                }
            }
        }

        sb.append(".text\n")
        p.items.foreach {
            case f: Asm.FunctionDef => emitFunction(f)
            case _                  => ()
        }
        sb.toString()
    }

    def calculateP2Align(alignment: Int): Int = {
        (math.log10(alignment) / math.log10(2)).toInt
    }

    def emitFunction(f: Asm.FunctionDef) = {
        val n = f.name
        if f.isGlobal then sb.append(s".global _$n\n")
        sb.append(".p2align 2\n")
        sb.append(s"_$n:\n")
        inst("stp x29, x30, [sp, #-16]!\n")
        inst("mov x29, sp\n")
        f.instructions.foreach(i => {
            emitInstruction(i)
            sb.append("\n")
        })
    }

    def emitInstruction(i: Asm.Instruction) = i match {
        case Asm.AllocateStack(size) => inst(s"sub sp, sp, #${pad16(size)}")

        case Asm.Adrp(destReg, label) =>
            inst(s"adrp ${showOp(destReg)}, _${label}@GOTPAGE")

        case Asm.LoadData(data, baseReg, destReg) => {
            val isFP = destReg match {
                case Asm.Register(r) => isFloatRegister(r)
            }
            val op =
                if (isFP) "ldr"
                else
                    data.size match {
                        case Size.Byte1 => "ldrb"
                        case Size.Byte2 => "ldrh"
                        case _          => "ldr"
                    }
            inst(s"ldr ${showOp(baseReg)}, [${showOp(baseReg)}, _${data.location}@GOTPAGEOFF]\n")
            inst(s"$op ${showOp(destReg)}, [${showOp(baseReg)}]")
        }

        case Asm.StoreData(srcReg, data, baseReg) => {
            val isFP = srcReg match {
                case Asm.Register(r) => isFloatRegister(r)
            }
            val op =
                if (isFP) "str"
                else
                    data.size match {
                        case Size.Byte1 => "strb"
                        case Size.Byte2 => "strh"
                        case _          => "str"
                    }
            inst(s"ldr ${showOp(baseReg)}, [${showOp(baseReg)}, _${data.location}@GOTPAGEOFF]\n")
            inst(s"$op ${showOp(srcReg)}, [${showOp(baseReg)}]")
        }

        case Asm.Load(Asm.StackSlot(offset, size), dest) => {
            val is64BitDest = Asm.getOperandSize(dest) == Size.Byte8
            val isFP = dest match {
                case Asm.Register(r) => isFloatRegister(r)
            }
            val op =
                if (isFP) "ldr"
                else
                    size match {
                        case Size.Byte1 => "ldrsb"
                        case Size.Byte2 => "ldrsh"
                        case Size.Byte4 => if (is64BitDest) "ldrsw" else "ldr"
                        case Size.Byte8 => "ldr"
                    }
            val targetDestStr = dest match {
                case Asm.Register(r) if !isFP && !is64BitDest =>
                    r match {
                        case Asm.Reg.X9  => "w9"
                        case Asm.Reg.X10 => "w10"
                        case Asm.Reg.X11 => "w11"
                        case other       => showOp(dest)
                    }
                case _ => showOp(dest)
            }
            inst(s"$op $targetDestStr, [x29, #$offset]")
        }

        case Asm.Store(src, Asm.StackSlot(offset, size)) => {
            val isFP = src match {
                case Asm.Register(r) => isFloatRegister(r)
            }
            val op =
                if (isFP) "str"
                else
                    size match {
                        case Size.Byte1 => "strb"
                        case Size.Byte2 => "strh"
                        case _          => "str"
                    }
            val targetSrcStr = src match {
                case Asm.Register(r) if !isFP && (size == Size.Byte1 || size == Size.Byte2 || size == Size.Byte4) =>
                    r match {
                        case Asm.Reg.X9  => "w9"
                        case Asm.Reg.X10 => "w10"
                        case Asm.Reg.X11 => "w11"
                        case other       => showOp(src)
                    }
                case _ => showOp(src)
            }
            inst(s"$op $targetSrcStr, [x29, #$offset]")
        }

        case Asm.LoadIndexed(dest, baseReg, offsetReg, size) => {
            val is64BitDest = Asm.getOperandSize(dest) == Size.Byte8
            val isFP = dest match {
                case Asm.Register(r) => isFloatRegister(r)
            }
            val op =
                if (isFP) "ldr"
                else
                    size match {
                        case Size.Byte1 => "ldrsb"
                        case Size.Byte2 => "ldrsh"
                        case Size.Byte4 => if (is64BitDest) "ldrsw" else "ldr"
                        case Size.Byte8 => "ldr"
                    }
            val targetDestStr = dest match {
                case Asm.Register(r) if !isFP && !is64BitDest =>
                    r match {
                        case Asm.Reg.X9  => "w9"
                        case Asm.Reg.X10 => "w10"
                        case Asm.Reg.X11 => "w11"
                        case other       => showOp(dest)
                    }
                case _ => showOp(dest)
            }
            inst(s"$op $targetDestStr, [${showOp(baseReg)}, ${showOp(offsetReg)}]")
        }

        case Asm.StoreIndexed(src, baseReg, offsetReg, size) => {
            val isFP = src match {
                case Asm.Register(r) => isFloatRegister(r)
            }
            val op =
                if (isFP) "str"
                else
                    size match {
                        case Size.Byte1 => "strb"
                        case Size.Byte2 => "strh"
                        case _          => "str"
                    }
            val targetSrcStr = src match {
                case Asm.Register(r) if !isFP && (size == Size.Byte1 || size == Size.Byte2 || size == Size.Byte4) =>
                    r match {
                        case Asm.Reg.X9  => "w9"
                        case Asm.Reg.X10 => "w10"
                        case Asm.Reg.X11 => "w11"
                        case other       => showOp(src)
                    }
                case _ => showOp(src)
            }
            inst(s"$op $targetSrcStr, [${showOp(baseReg)}, ${showOp(offsetReg)}]")
        }

        case Asm.Push(operand) => {
            operand match {
                case Asm.Register(reg) => inst(s"str ${showOp(operand)}, [sp, #-16]!")
                case Asm.Imm8(ival) =>
                    inst(s"mov w9, #$ival\n")
                    inst("str w9, [sp, #-16]!")
                case Asm.Imm16(ival) =>
                    inst(s"mov w9, #$ival\n")
                    inst("str w9, [sp, #-16]!")
                case Asm.Imm32(ival) => {
                    inst(s"mov w9, #$ival\n")
                    inst("str w9, [sp, #-16]!")
                }
                case Asm.Imm64(ival) => {
                    inst(s"mov x9, #$ival\n")
                    inst("str x9, [sp, #-16]!")
                }
                case _ => throw new RuntimeException(s"Unsupported push operand type: $operand")
            }
        }

        case Asm.Mov(src, dest) => {
            src match {
                case Asm.Imm64(ival) if ival > (BigInt(2).pow(16) - 1) || ival < 0L =>
                    inst(s"ldr ${showOp(dest)}, =$ival")

                case Asm.Imm32(ival) if ival > (BigInt(2).pow(16) - 1) || ival < 0 =>
                    inst(s"ldr ${showOp(dest)}, =$ival")

                case _ =>
                    inst(s"mov ${showOp(dest)}, ${showOp(src)}")
            }
        }
        case Asm.FMov(src, dest) =>
            src match {
                case Asm.Float16Lit(fval) => inst(s"ldr ${showOp(dest)}, =$fval")
                case Asm.Float32Lit(fval) => inst(s"ldr ${showOp(dest)}, =$fval")
                case Asm.Float64Lit(dval) => inst(s"ldr ${showOp(dest)}, =$dval")
                case _                    => inst(s"fmov ${showOp(dest)}, ${showOp(src)}")
            }

        case Asm.Sextb(src, dest) => inst(s"sxtb ${showOp(dest)}, ${showOp(src)}")
        case Asm.Sexth(src, dest) => inst(s"sxth ${showOp(dest)}, ${showOp(src)}")
        case Asm.Sextw(src, dest) => inst(s"sxtw ${showOp(dest)}, ${showOp(src)}")

        case Asm.Uxtb(src, dest) => inst(s"uxtb ${showOp(dest)}, ${showOp(src)}")
        case Asm.Uxth(src, dest) => inst(s"uxth ${showOp(dest)}, ${showOp(src)}")
        case Asm.Uxtw(src, dest) => inst(s"uxtw ${showOp(dest)}, ${showOp(src)}")

        case Asm.FBinary(op, s1, s2, d) => {
            val mnemonic = op match {
                case Asm.FBinaryOp.FAdd => "fadd"
                case Asm.FBinaryOp.FSub => "fsub"
                case Asm.FBinaryOp.FMul => "fmul"
                case Asm.FBinaryOp.FDiv => "fdiv"
            }
            inst(s"$mnemonic ${showOp(d)}, ${showOp(s1)}, ${showOp(s2)}")
        }

        case Asm.FCompare(s1, s2) =>
            inst(s"fcmp ${showOp(s1)}, ${showOp(s2)}")

        case Asm.FpToFp(src, dest) =>
            inst(s"fcvt ${showOp(dest)}, ${showOp(src)}")

        case Asm.SignedToFp(src, dest) =>
            inst(s"scvtf ${showOp(dest)}, ${showOp(src)}")

        case Asm.UnsignedToFp(src, dest) =>
            inst(s"ucvtf ${showOp(dest)}, ${showOp(src)}")

        case Asm.FpToSigned(src, dest) =>
            inst(s"fcvtzs ${showOp(dest)}, ${showOp(src)}")

        case Asm.FpToUnsigned(src, dest) =>
            inst(s"fcvtzu ${showOp(dest)}, ${showOp(src)}")

        case Asm.Ret() => {
            inst("mov sp, x29\n")
            inst("ldp x29, x30, [sp], #16\n")
            inst("ret")
        }
        case Asm.Unary(op, operand) => {
            val mnemonic = if (op == Asm.UnaryOp.Neg) "neg" else "mvn"
            inst(s"$mnemonic ${showOp(operand)}, ${showOp(operand)}")
        }
        case Asm.Binary(op, s1, s2, d) => {
            val mnemonic = op match {
                case Asm.BinaryOp.Add    => "add"
                case Asm.BinaryOp.Sub    => "sub"
                case Asm.BinaryOp.Mult   => "mul"
                case Asm.BinaryOp.Div    => "sdiv"
                case Asm.BinaryOp.UDiv   => "udiv"
                case Asm.BinaryOp.BitAnd => "and"
                case Asm.BinaryOp.BitOr  => "orr"
                case Asm.BinaryOp.BitXor => "eor"
                case Asm.BinaryOp.Lsl    => "lsl"
                case Asm.BinaryOp.Asr    => "asr"
                case Asm.BinaryOp.Lsr    => "lsr"
            }
            inst(s"$mnemonic ${showOp(d)}, ${showOp(s1)}, ${showOp(s2)}")
        }
        case Asm.MultiplySubtract(s1, s2, s3, d) => {
            sb.append(s"    msub ${showOp(d)}, ${showOp(s1)}, ${showOp(s2)}, ${showOp(s3)}")
        }
        case Asm.Compare(s1, s2) => {
            inst(s"cmp ${showOp(s1)}, ${showOp(s2)}")
        }
        case Asm.ConditionalSet(condition, destination) => {
            inst(s"cset ${showOp(destination)}, ${showConditionCode(condition)}")
        }
        case Asm.ConditionalBranch(condition, targetLabel) => {
            inst(s"b.${showConditionCode(condition)} $targetLabel")
        }
        case Asm.Branch(targetLabel) => {
            inst(s"b $targetLabel")
        }
        case Asm.Label(name) => {
            sb.append(s"$name:")
        }

        case Asm.Call(target) => {
            inst(s"bl _$target")
        }
        case Asm.DeallocateStack(size) => {
            inst(s"add sp, sp, #${size}")
        }
    }

    private def isFloatRegister(reg: Asm.Reg): Boolean = reg match {
        case Asm.Reg.H0 | Asm.Reg.H1 | Asm.Reg.H2 | Asm.Reg.H3 | Asm.Reg.H4 | Asm.Reg.H5 | Asm.Reg.H6 | Asm.Reg.H7 | Asm.Reg.H9 | Asm.Reg.H10 | Asm.Reg.H11 => true
        case Asm.Reg.S0 | Asm.Reg.S1 | Asm.Reg.S2 | Asm.Reg.S3 | Asm.Reg.S4 | Asm.Reg.S5 | Asm.Reg.S6 | Asm.Reg.S7 | Asm.Reg.S9 | Asm.Reg.S10 | Asm.Reg.S11 => true
        case Asm.Reg.D0 | Asm.Reg.D1 | Asm.Reg.D2 | Asm.Reg.D3 | Asm.Reg.D4 | Asm.Reg.D5 | Asm.Reg.D6 | Asm.Reg.D7 | Asm.Reg.D9 | Asm.Reg.D10 | Asm.Reg.D11 => true
        case _                                                                                                                                              => false
    }

    def showOp(o: Asm.Operand): String = o match {
        case Asm.Imm8(ival)           => s"#$ival"
        case Asm.Imm16(ival)          => s"#$ival"
        case Asm.Imm32(ival)          => s"#$ival"
        case Asm.Imm64(ival)          => s"#$ival"
        case Asm.Float16Lit(fval)     => s"#$fval"
        case Asm.Float32Lit(fval)     => s"#$fval"
        case Asm.Float64Lit(dval)     => s"#$dval"
        case Asm.StackSlot(offset, _) => s"[x29, #$offset]"

        case Asm.Register(Asm.Reg.W0)  => "w0"
        case Asm.Register(Asm.Reg.W1)  => "w1"
        case Asm.Register(Asm.Reg.W2)  => "w2"
        case Asm.Register(Asm.Reg.W3)  => "w3"
        case Asm.Register(Asm.Reg.W4)  => "w4"
        case Asm.Register(Asm.Reg.W5)  => "w5"
        case Asm.Register(Asm.Reg.W6)  => "w6"
        case Asm.Register(Asm.Reg.W7)  => "w7"
        case Asm.Register(Asm.Reg.W9)  => "w9"
        case Asm.Register(Asm.Reg.W10) => "w10"
        case Asm.Register(Asm.Reg.W11) => "w11"
        case Asm.Register(Asm.Reg.WZR) => "wzr"

        case Asm.Register(Asm.Reg.X0)  => "x0"
        case Asm.Register(Asm.Reg.X1)  => "x1"
        case Asm.Register(Asm.Reg.X2)  => "x2"
        case Asm.Register(Asm.Reg.X3)  => "x3"
        case Asm.Register(Asm.Reg.X4)  => "x4"
        case Asm.Register(Asm.Reg.X5)  => "x5"
        case Asm.Register(Asm.Reg.X6)  => "x6"
        case Asm.Register(Asm.Reg.X7)  => "x7"
        case Asm.Register(Asm.Reg.X9)  => "x9"
        case Asm.Register(Asm.Reg.X10) => "x10"
        case Asm.Register(Asm.Reg.X11) => "x11"
        case Asm.Register(Asm.Reg.XZR) => "xzr"
        case Asm.Register(Asm.Reg.X16) => "x16"
        case Asm.Register(Asm.Reg.X17) => "x17"
        case Asm.Register(Asm.Reg.X29) => "x29"
        case Asm.Register(Asm.Reg.X30) => "x30"

        case Asm.Register(Asm.Reg.H0)  => "h0"
        case Asm.Register(Asm.Reg.H1)  => "h1"
        case Asm.Register(Asm.Reg.H2)  => "h2"
        case Asm.Register(Asm.Reg.H3)  => "h3"
        case Asm.Register(Asm.Reg.H4)  => "h4"
        case Asm.Register(Asm.Reg.H5)  => "h5"
        case Asm.Register(Asm.Reg.H6)  => "h6"
        case Asm.Register(Asm.Reg.H7)  => "h7"
        case Asm.Register(Asm.Reg.H9)  => "h9"
        case Asm.Register(Asm.Reg.H10) => "h10"
        case Asm.Register(Asm.Reg.H11) => "h11"

        case Asm.Register(Asm.Reg.S0)  => "s0"
        case Asm.Register(Asm.Reg.S1)  => "s1"
        case Asm.Register(Asm.Reg.S2)  => "s2"
        case Asm.Register(Asm.Reg.S3)  => "s3"
        case Asm.Register(Asm.Reg.S4)  => "s4"
        case Asm.Register(Asm.Reg.S5)  => "s5"
        case Asm.Register(Asm.Reg.S6)  => "s6"
        case Asm.Register(Asm.Reg.S7)  => "s7"
        case Asm.Register(Asm.Reg.S9)  => "s9"
        case Asm.Register(Asm.Reg.S10) => "s10"
        case Asm.Register(Asm.Reg.S11) => "s11"

        case Asm.Register(Asm.Reg.D0)  => "d0"
        case Asm.Register(Asm.Reg.D1)  => "d1"
        case Asm.Register(Asm.Reg.D2)  => "d2"
        case Asm.Register(Asm.Reg.D3)  => "d3"
        case Asm.Register(Asm.Reg.D4)  => "d4"
        case Asm.Register(Asm.Reg.D5)  => "d5"
        case Asm.Register(Asm.Reg.D6)  => "d6"
        case Asm.Register(Asm.Reg.D7)  => "d7"
        case Asm.Register(Asm.Reg.D9)  => "d9"
        case Asm.Register(Asm.Reg.D10) => "d10"
        case Asm.Register(Asm.Reg.D11) => "d11"

        case _ => throw new RuntimeException(s"Unexpected operand type: $o")
    }

    private def showConditionCode(cc: Asm.ConditionCode): String = cc match {
        case Asm.ConditionCode.Equal    => "eq"
        case Asm.ConditionCode.NotEqual => "ne"

        case Asm.ConditionCode.LessThan       => "lt"
        case Asm.ConditionCode.LessOrEqual    => "le"
        case Asm.ConditionCode.GreaterThan    => "gt"
        case Asm.ConditionCode.GreaterOrEqual => "ge"

        case Asm.ConditionCode.CarryClear  => "cc"
        case Asm.ConditionCode.LowerOrSame => "ls"
        case Asm.ConditionCode.CarrySet    => "cs"
        case Asm.ConditionCode.Higher      => "hi"
    }
}
