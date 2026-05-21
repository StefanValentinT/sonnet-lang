package app

import java.nio.file.{Files, Paths}
import java.util.Scanner
import parser.Parser
import scanner.FileScanner
import syntax.Node
import token.tokenize
import eval.Evaluator
import pprint.pprintln

class CompilerError(message: String) extends RuntimeException(message) {
    def this(who: String, detail: String) = {
        this({
            val baseMessage = s"$who determined that compilation must be aborted."
            if (detail != null) {
                s"$baseMessage\n Reasoning: $detail"
            } else {
                baseMessage
            }
        })
    }
}

object App {

    def main(args: Array[String]): Unit = {
        val scanner             = new FileScanner()
        val mode                = args(0).toLowerCase()
        val ctx                 = CompilerContext.fromArgs(args.slice(3, args.size))
        var filename: String    = ""
        var fileContent: String = ""

        mode match {

            case "run" => {
                filename = args(1)
                fileContent = scanner.readFile(filename)
                println(s"Running $filename:")
                val res = eval(fileContent, ctx)
                println(res)
            }

            case "repl" => {
                println("Starting REPL.")
            }

            case "help" => {
                printUsage()
            }

            case _ => {
                println(s"Mode '$mode' not recognized. Use mode help to get an overview of availaible modes.")
            }
        }
    }

    def eval(prog: String, ctx: CompilerContext): Node = {
        val tokens = tokenize(prog)
        val ast    = Parser().parse(tokens)
        val res    = Evaluator(ctx).evaluate(ast)
        res
    }

    private def printUsage(): Unit = {
        println("Usage:")
        println("\tTo format:   gradle run --args=\"fmt filename.st\"")
        println("\tTo run:      gradle run --args=\"run filename.st\"")
        println("\tTo get this: gradle run --args=\"help\"")
    }
}
