package episteme

import syntax.*
import io.FileScanner
import app.CompilerError
import scala.collection.mutable.Set
import java.nio.file.Paths

class ImportResolverError(detail: String) extends CompilerError("Import Resolution Pass", detail)

// The simplest possible, recursively descending import resolver.
// Will be replaced later with a proper module system.
class ImportResolver {
    private val visitedPaths = Set[String]()

    def resolve(p: Program): Program = {
        visitedPaths.clear()

        val flattenedItems = p.items.flatMap(resolveItem)
        Program(flattenedItems)
    }

    def resolveItem(item: TopLevelItem): List[TopLevelItem] = item match {
        case Import(path) => {
            val canonicalPath =
                try {
                    Paths.get(path).toAbsolutePath.normalize().toString
                } catch {
                    case _: Exception => throw ImportResolverError(s"Invalid import file path specified: $path")
                }

            if (visitedPaths.contains(canonicalPath)) {
                List.empty
            } else {
                visitedPaths.add(canonicalPath)
                val sourceCode      = FileScanner.readFile(canonicalPath)
                val importedProgram = Parser.fromString(sourceCode).parse()
                importedProgram.items.flatMap(resolveItem)
            }
        }
        case other => List(other)
    }
}
