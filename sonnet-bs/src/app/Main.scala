package app

import io.{FileScanner, FileWriter, CliParser, CompilerArgs}
import syntax.*
import pprint.pprintln
import arm64.*
import qbe.*
import tac.*
import episteme.*
import java.nio.file.{Paths, Path}

open class CompilerError(who: String, detail: String = null)
    extends RuntimeException({
        val baseMessage = s"$who determined that compilation must be aborted."
        if (detail != null) s"$baseMessage\nReasoning: $detail" else baseMessage
    })

object App {

    def main(args: Array[String]): Unit = {
        CliParser.parse(args) match {
            case None        => printUsage()
            case Some(cArgs) => execute(cArgs)
        }
    }

    private def execute(cfg: CompilerArgs): Unit = {
        cfg.filename match {
            case "help" => printUsage()
            case "repl" => println("Starting REPL.")
            case filename => {
                val sourcePath = Paths.get(filename).toAbsolutePath.normalize()
                val sourceDir  = sourcePath.getParent
                val baseName   = sourcePath.getFileName.toString.replaceAll("\\.[^.]+$", "")

                val asmPath = sourceDir.resolve("build").resolve(s"$baseName.qbe")

                val fileContent = FileScanner.readFile(sourcePath.toString)
                if (cfg.isVerbose) println(s"Compiling $filename:")

                val ast = Parser.fromString(fileContent).parse()
                if (cfg.isVerbose) pprintln(ast)

                /*
                val importAst  = ImportResolver().resolve(ast, sourcePath.toString)
                val fixedAst   = VariableResolver.resolveProgram(importAst)
                val labeledAst = LoopLabeler.labelProgram(fixedAst)
                if (cfg.isVerbose) pprintln(fixedAst)
                val typedAst = TypeChecker.typecheckProgram(labeledAst)
                if (cfg.isVerbose) pprintln(typedAst)

                val tacAst = TacEmitter(typedAst).emitProgramTac()
                if (cfg.isVerbose) pprintln(tacAst)

                /*
                var asmAst = Codegener().codegenProgram(tacAst)
                asmAst = PseudoRegisterReplacer.inProgram(asmAst)
                if (cfg.isVerbose) pprintln(asmAst)
                val asmString = Emitter().emitProgram(asmAst)
                if (cfg.isVerbose) println(asmString)
                 */

                val qbeString = QbeEmitter().emitTac(tacAst)
                if (cfg.isVerbose) println(qbeString)

                FileWriter.writeFile(asmPath, qbeString)
                 */

                val finalExecPath = cfg.outputExecPath.getOrElse(sourceDir.resolve(baseName))

                if (cfg.isVerbose) {
                    println(s"Output generated at: $asmPath")
                }
            }
        }
    }

    private def printUsage(): Unit = {
        println("Usage:")
        println("\tTo compile:       sonnetc <filename> [-o <output_exec>] [-v | --verbose]")
        println("\tTo get this:      sonnetc help")
        println("\tTo get the REPL:  sonnetc repl")
    }
}
