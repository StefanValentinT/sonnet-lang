package app

import io.{FileScanner, FileWriter}
import syntax.*
import pprint.pprintln
import codegen.*
import arm64.*
import tac.*

open class CompilerError(who: String, detail: String = null)
    extends RuntimeException({
        val baseMessage = s"$who determined that compilation must be aborted."
        if (detail != null) s"$baseMessage\nReasoning: $detail" else baseMessage
    })

object App {

    def main(args: Array[String]): Unit = {
        if args.size == 0 then { printUsage(); return () }

        val mode = args(0)

        mode match {
            case "help" => {
                printUsage()
            }

            case "repl" => {
                println("Starting REPL.")
            }

            case filename => {
                val fileContent = FileScanner.readFile(filename)
                println(s"Compiling $filename:")
                val ast      = Parser.fromString(fileContent).parse()
                val tacAst   = TacEmitter(ast).emitProgramTac()
                val asmAst   = codegenProgram(tacAst)
                val fixedAst = PseudoRegisterReplacer().inProgram(asmAst)
                pprintln(fixedAst)
                val asmString = Emitter().emitProgram(fixedAst)
                println(asmString)
                FileWriter.writeFile(filename, asmString)
            }
        }
    }

    private def printUsage(): Unit = {
        println("Usage:")
        println("\tTo compile:       sonnetc <filename>")
        println("\tTo get this:      sonnetc help")
        println("\tTo get the REPL:  sonnetc repl")
    }
}
