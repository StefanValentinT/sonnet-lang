package episteme

import syntax.*
import scala.collection.mutable.Map
import app.CompilerError

class EpistemicError(detail: String) extends CompilerError("Semantic Compiler Pass", detail)

object VariableResolver {
    private val variableMap = Map[String, String]()
    private var varCounter  = 0

    def makeUnique(orig: String): String = {
        varCounter += 1
        s"${orig}_${varCounter}"
    }

    def resolveProgram(p: Program): Program = {
        Program(resolveFunctionDef(p.items))
    }

    def resolveFunctionDef(f: FunctionDef): FunctionDef = {
        FunctionDef(f.name, f.body.map(resolveStatement))
    }

    def resolveStatement(stmt: Statement): Statement = {
        stmt match {
            case Return(exp)         => Return(resolveExpression(exp))
            case ExpressionStmt(exp) => ExpressionStmt(resolveExpression(exp))
            case Declaration(vNode, init) => {
                val name = vNode.name
                if (variableMap.contains(name)) {
                    throw EpistemicError(s"Duplicate variable declaration: $name")
                }
                val uniqueName = makeUnique(name)
                variableMap.put(name, uniqueName)

                val resolvedInit = init.map(resolveExpression)
                Declaration(Var(uniqueName), resolvedInit)
            }
        }
    }

    def resolveExpression(exp: Expression): Expression = {
        exp match {
            case Constant(value)        => Constant(value)
            case Unary(op, e)           => Unary(op, resolveExpression(e))
            case Binary(op, exp1, exp2) => Binary(op, resolveExpression(exp1), resolveExpression(exp2))
            case Var(value) => {
                variableMap.get(value) match {
                    case Some(uniqueName) => Var(uniqueName)
                    case None             => throw EpistemicError(s"Undeclared variable: $value")
                }
            }
            case Assignment(target, value) => {
                target match {
                    case Var(name) =>
                        if (!variableMap.contains(name)) {
                            throw EpistemicError(s"Undeclared variable assignment: $name")
                        }
                        Assignment(Var(variableMap(name)), resolveExpression(value))
                    case _ =>
                        throw EpistemicError("Invalid l-value; target must be a variable node. This could also indicate that a compound operator was falsely used in a declaration.")
                }
            }
        }
    }
}
