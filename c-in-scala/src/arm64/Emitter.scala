package arm64

class Emitter() {
    val sb = StringBuilder()

    def inst(s: String) = sb.append("    ").append(s)

    def emitProgram(p: Asm.Program): String = {
        sb.append(".text\n")
        p.items.foreach(emitFunction)
        sb.toString()
    }

    def emitFunction(f: Asm.FunctionDef) = {
        val n = f.name
        sb.append(s".global _$n\n")
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
        case Asm.Load(src, dest)     => inst(s"ldr ${showOp(dest)}, ${showOp(src)}")
        case Asm.Store(src, dest)    => inst(s"str ${showOp(src)}, ${showOp(dest)}")
        case Asm.Mov(src, dest)      => inst(s"mov ${showOp(dest)}, ${showOp(src)}")
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
                case Asm.Imm(ival) => {
                    inst(s"mov w9, #$ival\n")
                    inst("str w9, [sp, #-16]!")
                }
                case _ => throw new RuntimeException("Unsupported push operand type")
            }
        }
    }

    def showOp(o: Asm.Operand): String = o match {
        case Asm.Imm(ival)             => s"#$ival"
        case Asm.StackSlot(offset)     => s"[x29, #$offset]"
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
