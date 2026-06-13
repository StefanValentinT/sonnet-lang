package arm64

import syntax.Size

class Emitter() {
    val sb = StringBuilder()

    def inst(s: String) = sb.append("    ").append(s)

    def emitProgram(p: Asm.Program): String = {
        val staticVars                     = p.items.collect { case v: Asm.StaticVariable => v }
        val (initialized, zeroInitialized) = staticVars.partition(v => v.init.getValue != 0)

        if (initialized.nonEmpty) {
            sb.append(".data\n")
            initialized.foreach {
                case Asm.StaticVariable(name, isGlobal, alignment, init) => {
                    if (isGlobal) {
                        sb.append(s".global _${name}\n")
                    }
                    val asmDirective = alignment match {
                        case Size.Byte1 => ".byte"
                        case Size.Byte2 => ".short"
                        case Size.Byte4 => ".word"
                        case Size.Byte8 => ".quad"
                    }
                    sb.append(s".p2align ${calculateP2Align(alignment.bytes)}\n")
                    sb.append(s"_$name:\n")

                    sb.append(s"    $asmDirective ${init.getValue}\n\n")
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

        case Asm.LoadData(data, baseReg, destReg) =>
            inst(s"ldr ${showOp(baseReg)}, [${showOp(baseReg)}, _${data.location}@GOTPAGEOFF]\n")
            inst(s"ldr ${showOp(destReg)}, [${showOp(baseReg)}]")

        case Asm.StoreData(srcReg, data, baseReg) =>
            inst(s"ldr ${showOp(baseReg)}, [${showOp(baseReg)}, _${data.location}@GOTPAGEOFF]\n")
            inst(s"str ${showOp(srcReg)}, [${showOp(baseReg)}]")

        case Asm.Load(Asm.StackSlot(offset, size), dest) => {
            val is64BitDest = Asm.getOperandSize(dest) == Size.Byte8
            val op = size match {
                case Size.Byte1 => "ldrsb"
                case Size.Byte2 => "ldrsh"
                case Size.Byte4 => if (is64BitDest) "ldrsw" else "ldr"
                case Size.Byte8 => "ldr"
            }
            val targetDestStr = size match {
                case Size.Byte1 | Size.Byte2 => showOp(Asm.Register(dest.reg.to32))
                case _                       => showOp(dest)
            }
            if (offset >= -256 && offset <= 255) {
                inst(s"$op $targetDestStr, [x29, #$offset]")
            } else {
                inst(s"mov x16, #$offset\n")
                inst(s"$op $targetDestStr, [x29, x16]")
            }
        }

        case Asm.Store(src, Asm.StackSlot(offset, size)) => {
            val op = size match {
                case Size.Byte1 => "strb"
                case Size.Byte2 => "strh"
                case Size.Byte4 => "str"
                case Size.Byte8 => "str"
            }
            val targetSrcStr = size match {
                case Size.Byte1 | Size.Byte2 => showOp(Asm.Register(src.reg.to32))
                case _                       => showOp(src)
            }
            if (offset >= -256 && offset <= 255) {
                inst(s"$op $targetSrcStr, [x29, #$offset]")
            } else {
                inst(s"mov x16, #$offset\n")
                inst(s"$op $targetSrcStr, [x29, x16]")
            }
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

        case Asm.Sextb(src, dest) => inst(s"sxtb ${showOp(dest)}, ${showOp(src)}")
        case Asm.Sexth(src, dest) => inst(s"sxth ${showOp(dest)}, ${showOp(src)}")
        case Asm.Sextw(src, dest) => inst(s"sxtw ${showOp(dest)}, ${showOp(src)}")

        case Asm.Uxtb(src, dest) => inst(s"uxtb ${showOp(dest)}, ${showOp(src)}")
        case Asm.Uxth(src, dest) => inst(s"uxth ${showOp(dest)}, ${showOp(src)}")
        case Asm.Uxtw(src, dest) => inst(s"uxtw ${showOp(dest)}, ${showOp(src)}")

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

    def showOp(o: Asm.Operand): String = o match {
        case Asm.Imm8(ival)            => s"#$ival"
        case Asm.Imm16(ival)           => s"#$ival"
        case Asm.Imm32(ival)           => s"#$ival"
        case Asm.Imm64(ival)           => s"#$ival"
        case Asm.StackSlot(offset, _)  => s"[x29, #$offset]"
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
        case Asm.Register(Asm.Reg.X30) => "x30"
        case _                         => throw new RuntimeException(s"Unexpected operand type: $o")
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
