package episteme

import syntax.*
import scala.collection.mutable.Map
import app.CompilerError

class EpistemicError(detail: String) extends CompilerError("Semantic Compiler Pass", detail)

case class MapEntry(newName: String, fromCurrentBlock: Boolean)

object VariableResolver {
    private var varCounter = 0

    def makeUnique(orig: String): String = {
        varCounter += 1
        s"${orig}_${varCounter}"
    }

    def resolveProgram(p: Program): Program = {
        Program(resolveFunctionDef(p.items))
    }

    def resolveFunctionDef(f: FunctionDef): FunctionDef = {
        val initialMap = Map[String, MapEntry]()
        FunctionDef(f.name, resolveExpression(f.body, initialMap))
    }

    def resolveStatement(stmt: Statement, variableMap: Map[String, MapEntry]): Statement = {
        stmt match {
            case ExpressionStmt(exp) => ExpressionStmt(resolveExpression(exp, variableMap))
            case Declaration(vNode, init) => {
                val name = vNode.name
                if (variableMap.contains(name) && variableMap(name).fromCurrentBlock) {
                    throw EpistemicError(s"Duplicate variable declaration: $name")
                }
                val uniqueName = makeUnique(name)
                variableMap.put(name, MapEntry(uniqueName, true))

                val resolvedInit = init.map(e => resolveExpression(e, variableMap))
                Declaration(Var(uniqueName), resolvedInit)
            }
        }
    }

    def resolveExpression(exp: Expression, variableMap: Map[String, MapEntry]): Expression = {
        exp match {
            case Block(stmts, exp) => {
                val innerMap      = copyVariableMap(variableMap)
                val resolvedStmts = stmts.map(s => resolveStatement(s, innerMap))
                val resolvedExp   = if exp.isDefined then Some(resolveExpression(exp.get, innerMap)) else None
                Block(resolvedStmts, resolvedExp)
            }
            case e @ Continue(_)        => e
            case e @ Break(_)           => e
            case While(cond, exp, l)    => While(resolveExpression(cond, variableMap), resolveExpression(exp, variableMap), l)
            case If(cond, thenB, elseB) => If(resolveExpression(cond, variableMap), resolveExpression(thenB, variableMap), if elseB.isDefined then Some(resolveExpression(elseB.get, variableMap)) else None)
            case Constant(value)        => Constant(value)
            case Unary(op, e)           => Unary(op, resolveExpression(e, variableMap))
            case Return(exp)            => Return(resolveExpression(exp, variableMap))
            case Binary(op, exp1, exp2) => Binary(op, resolveExpression(exp1, variableMap), resolveExpression(exp2, variableMap))
            case Var(value) => {
                variableMap.get(value) match {
                    case Some(MapEntry(uniqueName, c)) => Var(uniqueName)
                    case None                          => throw EpistemicError(s"Undeclared variable: $value")
                }
            }
            case Assignment(target, value) => {
                target match {
                    case Var(name) =>
                        if (!variableMap.contains(name)) {
                            throw EpistemicError(s"Undeclared variable assignment: $name")
                        }
                        Assignment(Var(variableMap(name).newName), resolveExpression(value, variableMap))
                    case _ =>
                        throw EpistemicError("Invalid l-value; target must be a variable node. This could also indicate that a compound operator was falsely used in a declaration.")
                }
            }
        }
    }

    def copyVariableMap(currentMap: Map[String, MapEntry]): Map[String, MapEntry] = {
        val newMap = Map[String, MapEntry]()
        currentMap.foreach { case (key, entry) =>
            newMap.put(key, entry.copy(fromCurrentBlock = false))
        }
        newMap
    }

}
