package io

import java.nio.file.{Path, Paths}

case class CompilerArgs(
    filename: String,
    outputExecPath: Option[Path],
    isVerbose: Boolean
)

object CliParser {
    def parse(args: Array[String]): Option[CompilerArgs] = {
        val verboseFlags = Set("-v", "--verbose")
        val isVerbose    = args.exists(verboseFlags.contains)
        val filtered     = args.filterNot(verboseFlags.contains)

        val oIndex = filtered.indexOf("-o")

        val (outputExec, remaining) = if (oIndex != -1 && oIndex + 1 < filtered.length) {
            val path    = Some(Paths.get(filtered(oIndex + 1)).toAbsolutePath.normalize())
            val patched = filtered.patch(oIndex, Nil, 2)
            (path, patched)
        } else {
            (None, filtered)
        }

        if (remaining.isEmpty) None
        else Some(CompilerArgs(remaining(0), outputExec, isVerbose))
    }
}
