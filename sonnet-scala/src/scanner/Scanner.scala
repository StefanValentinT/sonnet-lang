package scanner

import java.nio.file.{Files, Path}
import java.io.IOException

class ScannerError extends RuntimeException

class FileScanner {

    def readFile(filePath: String): String = {
        try {
            Files.readString(Path.of(filePath))
        } catch {
            case _: IOException | _: NullPointerException => throw new ScannerError()
        }
    }
}
