package episteme

import syntax.*
import io.FileScanner
import app.CompilerError
import scala.collection.mutable.Set
import java.nio.file.{Paths, Path}

class ImportResolverError(detail: String) extends CompilerError("Import Resolution Pass", detail)

class ImportResolver {
    private val visitedPaths = Set[String]()

    def resolve(p: Program, initialFilePath: String): Program = {
        visitedPaths.clear()

        val initialPath =
            try {
                Paths.get(initialFilePath).toAbsolutePath.normalize()
            } catch {
                case _: Exception => throw ImportResolverError(s"Invalid initial file path: $initialFilePath")
            }

        visitedPaths.add(initialPath.toString)

        val initialDir     = initialPath.getParent
        val flattenedItems = p.items.flatMap(item => resolveItem(item, initialDir))
        Program(flattenedItems)
    }

    def resolveItem(item: TopLevelItem, currentDir: Path): List[TopLevelItem] = item match {
        case Import(path) => {
            val canonicalPath =
                try {
                    if (Paths.get(path).isAbsolute) {
                        Paths.get(path).normalize().toString
                    } else {
                        currentDir.resolve(path).normalize().toAbsolutePath.toString
                    }
                } catch {
                    case _: Exception => throw ImportResolverError(s"Invalid import file path specified: $path")
                }

            if (visitedPaths.contains(canonicalPath)) {
                List.empty
            } else {
                visitedPaths.add(canonicalPath)

                val nextPath = Paths.get(canonicalPath)
                val nextDir  = nextPath.getParent

                val sourceCode      = FileScanner.readFile(canonicalPath)
                val importedProgram = Parser.fromString(sourceCode).parse()
                importedProgram.items.flatMap(item => resolveItem(item, nextDir))
            }
        }
        case other => List(other)
    }
}
