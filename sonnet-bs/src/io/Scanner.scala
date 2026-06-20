package io

import java.nio.file.{Files, Path}
import java.io.IOException
import app.CompilerError

class IOError(detail: String) extends CompilerError("Input-Output-System", detail)

object FileScanner {

    def readFile(filePath: String): String = {
        try {
            Files.readString(Path.of(filePath))
        } catch {
            case e @ (_: IOException | _: NullPointerException) =>
                throw new IOError(s"Input impossible. Reason: ${e.getMessage}")
        }
    }
}
