package assembly

import syntax.*

class Emitter() {
    var sb = StringBuilder()

    def emitProgram(p: AsmProgram): String = {
        sb.append(".text\n")
        emitFunction(p.item)
        sb.toString()
    }

    def emitFunction(f: AsmFunctionDef) = {
        // everythign is global - remove later
        val n = f.name
        sb.append(s".global _$n\n")
        sb.append(s"_$n:\n")
        f.instructions.foreach(i => {
            emitInstruction(i)
            sb.append("\n")
        })
    }

    def emitInstruction(i: AsmInstruction) = i match {
        case Mov(src, dest) => {
            sb.append("mov ");
            emitOperand(dest);
            sb.append(", ");
            emitOperand(src)
        }
        case Ret() => sb.append("ret")
    }

    def emitOperand(o: AsmOperand) = o match {
        case Imm(ival)  => { sb.append("#"); sb.append(ival) }
        case Register() => sb.append("w0")
    }

}
