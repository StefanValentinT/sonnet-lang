package arm64

class Emitter() {
    val sb = StringBuilder()

    def inst(s: String) = sb.append("    ").append(s)

    def emitProgram(p: Asm.Program): String = {
        sb.append(".text\n")
        emitFunction(p.items)
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

    def pad16(size: Int): Int = (size + 15) & ~15

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
    }

    def showOp(o: Asm.Operand): String = o match {
        case Asm.Imm(ival)            => s"#$ival"
        case Asm.StackSlot(offset)    => s"[x29, #$offset]"
        case Asm.Register(Asm.Reg.W0) => "w0"
        case Asm.Register(Asm.Reg.W9) => "w9"
        case _                        => throw new RuntimeException("Unexpected operand type")
    }
}
