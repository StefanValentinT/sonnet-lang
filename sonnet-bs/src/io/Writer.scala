package io

import java.nio.file.{Files, Path, Paths}
import java.io.IOException

object FileWriter {
    def writeFile(targetPath: Path, content: String): Unit = {
        try {
            val parentDir = targetPath.getParent
            if (parentDir != null) {
                Files.createDirectories(parentDir)
            }
            Files.writeString(targetPath, content)
        } catch {
            case _: IOException | _: NullPointerException =>
                throw new IOError(s"Output failed writing to: $targetPath")
        }
    }
}
