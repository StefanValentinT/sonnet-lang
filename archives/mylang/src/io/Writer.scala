package io

import java.nio.file.{Files, Path, Paths}
import java.io.IOException

object FileWriter {
    def writeFile(filePath: String, content: String): Unit = {
        val fileName = Paths.get(filePath).getFileName.toString
        val asmFileName = if (fileName.contains(".")) {
            fileName.replaceAll("\\.[^.]+$", ".s")
        } else {
            s"$fileName.s"
        }
        val finalAsmPath = Paths.get("build", asmFileName)
        try {
            val parentDir = finalAsmPath.getParent
            if (parentDir != null && !Files.exists(parentDir)) {
                Files.createDirectories(parentDir)
            }

            Files.writeString(finalAsmPath, content)
        } catch {
            case _: IOException | _: NullPointerException => throw new IOError("Output failed.")
        }
    }

}
