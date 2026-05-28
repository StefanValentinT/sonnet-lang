package app

class CompilerContext(val verbose: Boolean) {
    def printStackFrames(): Boolean = verbose
}
object CompilerContext {
    def fromArgs(args: Array[String]) = {
        val v = args.exists((v: String) => (v == "-v" || v == "--verbose"))
        new CompilerContext(verbose = v)
    }

    def default() = CompilerContext(verbose = false)
}
