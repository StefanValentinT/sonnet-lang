package codegen

import syntax.*
import app.CompilerError

class CodeGenError(detail: String) extends CompilerError("Backend Code Generator", detail)

object LambdaCounter {
    private var counter: Int = 0
    def next(): Int = {
        counter += 1;
        counter
    }
    def get: Int = counter
}

object CodeGen {
    def genLowCode(prog: Program): LowProgram = {
        val lowFuncs = prog.items.flatMap {
            case Definition(Var("main"), syntax.Function(_, body)) => Some(genFunction("main", body))

            case Definition(Var(name), syntax.Function(_, body)) =>
                Some(genFunction(name, body))

            case _ => None
        }
        LowProgram(lowFuncs)
    }

    private def genFunction(name: String, body: Expression): LowFunction = {
        val instrs = body match {
            case IntLit(value, _) =>
                List(
                  Mov(Register(), Immediate(value)),
                  Ret()
                )
            case _ =>
                throw new CodeGenError("Functions can only return an int literal for now.")
        }
        LowFunction(name, instrs)
    }

    def emitLowCode(l: LowProgram): String = {
        val sb = new StringBuilder()

        sb.append(".text\n")
        sb.append(".align 2\n\n")

        for (f <- l.functions) {
            val asmName = s"_${f.name}"

            sb.append(s".global $asmName\n")
            sb.append(s"$asmName:\n")

            for (instr <- f.instructions) {
                sb.append("    ")
                instr match {
                    case Mov(dest, src) => {
                        val ds = emitOperand(dest)
                        val ss = emitOperand(src)
                        sb.append(s"mov $ds, $ss\n")
                    }

                    case Ret() => sb.append(s"ret\n")
                }
            }
            sb.append("\n")
        }
        sb.toString()
    }

    def emitOperand(o: LowOperand): String = o match {
        case Immediate(value) => s"#${value}"
        case Register()       => "w0"
    }
}
