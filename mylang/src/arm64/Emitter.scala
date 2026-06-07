package arm64

class Emitter() {
    val sb = StringBuilder()

    def inst(s: String) = sb.append("    ").append(s)

    def emitProgram(p: Asm.Program): String = {
        val staticVars                     = p.items.collect { case v: Asm.StaticVariable => v }
        val (initialized, zeroInitialized) = staticVars.partition(v => v.init != syntax.Const.I32Lit(0) && v.init != syntax.Const.I64Lit(0))

        if (initialized.nonEmpty) {
            sb.append(".data\n")
            initialized.foreach {
                case Asm.StaticVariable(name, isGlobal, alignment, init) => {
                    if (isGlobal) {
                        sb.append(s".global _${name}\n")
                    }
                    val asmDirective = alignment match {
                        case Asm.Size.Byte8 => ".quad"
                        case Asm.Size.Byte4 => ".word"
                    }
                    sb.append(s".p2align ${calculateP2Align(alignment.bytes)}\n")
                    sb.append(s"_$name:\n")

                    val numValue = init match {
                        case syntax.Const.I32Lit(n) => n
                        case syntax.Const.I64Lit(n) => n
                    }
                    sb.append(s"    $asmDirective $numValue\n\n")
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

        case Asm.Load(Asm.StackSlot(offset, _), dest) => inst(s"ldr ${showOp(dest)}, [x29, #$offset]")
        case Asm.Store(src, Asm.StackSlot(offset, _)) => inst(s"str ${showOp(src)}, [x29, #$offset]")
        case Asm.Mov(src, dest)                       => inst(s"mov ${showOp(dest)}, ${showOp(src)}")
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
                case Asm.BinaryOp.BitAnd => "and"
                case Asm.BinaryOp.BitOr  => "orr"
                case Asm.BinaryOp.BitXor => "eor"
                case Asm.BinaryOp.Lsl    => "lsl"
                case Asm.BinaryOp.Asr    => "asr"
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
        case Asm.Push(operand) => {
            operand match {
                case Asm.Register(reg) => inst(s"str ${showOp(operand)}, [sp, #-16]!")
                case Asm.Imm32(ival) => {
                    inst(s"mov w9, #$ival\n")
                    inst("str w9, [sp, #-16]!")
                }
                case Asm.Imm64(ival) => {
                    inst(s"mov x9, #$ival\n")
                    inst("str x9, [sp, #-16]!")
                }
                case _ => throw new RuntimeException("Unsupported push operand type")
            }
        }
    }

    def showOp(o: Asm.Operand): String = o match {
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
        case Asm.Register(Asm.Reg.X30) => "x30"
        case _                         => throw new RuntimeException(s"Unexpected operand type: $o")
    }

    private def showConditionCode(cc: Asm.ConditionCode): String = cc match {
        case Asm.ConditionCode.Equal          => "eq"
        case Asm.ConditionCode.NotEqual       => "ne"
        case Asm.ConditionCode.LessThan       => "lt"
        case Asm.ConditionCode.LessOrEqual    => "le"
        case Asm.ConditionCode.GreaterThan    => "gt"
        case Asm.ConditionCode.GreaterOrEqual => "ge"
    }
}
