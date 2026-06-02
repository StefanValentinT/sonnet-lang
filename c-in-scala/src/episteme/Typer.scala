package episteme

import syntax.*
import scala.collection.mutable.Map
import app.CompilerError

case class SymbolEntry(typ: Type, isDefined: Boolean)

object TypeChecker {

    private val symbols = Map[String, SymbolEntry]()

    def typecheckProgram(p: Program): Unit = {
        p.items.foreach {
            case d: Declaration => typecheckGlobalDeclaration(d)
            case f: FunctionDef => typecheckFunctionDef(f)
        }
    }

    private def typecheckGlobalDeclaration(decl: Declaration): Unit = {
        val funType = FunType(decl.argCount)

        if (symbols.contains(decl.name)) {
            val oldEntry = symbols(decl.name)
            if (oldEntry.typ != funType) {
                throw EpistemicError(s"Incompatible function declarations for ${decl.name}.")
            }
        } else {
            symbols.put(decl.name, SymbolEntry(funType, isDefined = false))
        }
    }

    private def typecheckFunctionDef(f: FunctionDef): Unit = {
        val funType        = FunType(f.params.length)
        var alreadyDefined = false
        if (symbols.contains(f.name)) {
            val oldEntry = symbols(f.name)
            if (oldEntry.typ != funType) {
                throw EpistemicError(s"Incompatible function definition type for ${f.name}.")
            }
            alreadyDefined = oldEntry.isDefined
        }
        if (alreadyDefined) {
            throw EpistemicError(s"Function ${f.name} is defined more than once.")
        }
        symbols.put(f.name, SymbolEntry(funType, isDefined = true))
        val outerSymbolsSnapshot = symbols.toMap
        for (param <- f.params) {
            symbols.put(param, SymbolEntry(I32(), isDefined = true))
        }
        typecheckExpression(f.body)
        symbols.clear()
        symbols.addAll(outerSymbolsSnapshot)
    }

    private def typecheckStatement(stmt: Statement): Unit = {
        stmt match {
            case ExpressionStmt(exp) =>
                typecheckExpression(exp)

            case VarDeclaration(vNode, init) =>
                symbols.put(vNode.name, SymbolEntry(I32(), isDefined = true))
                vNode.setType(I32())

                init.foreach(e => typecheckExpression(e))
        }
    }

    private def typecheckExpression(exp: Expression): Unit = {
        exp match {
            case Constant(_) =>
                exp.setType(I32())

            case Var(name) =>
                symbols.get(name) match {
                    case Some(SymbolEntry(I32(), _)) =>
                        exp.setType(I32())
                    case Some(SymbolEntry(FunType(_), _)) =>
                        throw EpistemicError(s"Function name $name used for a variable.")
                    case None =>
                        throw EpistemicError(s"This should have been rejected by the variable resolver.")
                }

            case FunctionCall(target, args) =>
                symbols.get(target) match {
                    case Some(SymbolEntry(FunType(paramCount), _)) =>
                        if (paramCount != args.length) {
                            throw EpistemicError(s"Function $target called with wrong number of arguments. Expected $paramCount, got ${args.length}.")
                        }
                        args.foreach(arg => typecheckExpression(arg))
                        exp.setType(I32())
                    case Some(SymbolEntry(I32(), _)) =>
                        throw EpistemicError(s"Variable '$target' used as function.")
                    case None =>
                        throw EpistemicError("This should have been rejected by the variable resolver.")
                }

            case Assignment(target, value) =>
                typecheckExpression(target)
                typecheckExpression(value)
                if (target.getType != value.getType) {
                    throw EpistemicError("Type mismatch inside assignment operation.")
                }
                exp.setType(target.getType)

            case Unary(_, e) =>
                typecheckExpression(e)
                if (e.getType != I32()) throw EpistemicError("Unary operator expects integer value.")
                exp.setType(I32())

            case Binary(_, exp1, exp2) =>
                typecheckExpression(exp1)
                typecheckExpression(exp2)
                if (exp1.getType != I32() || exp2.getType != I32()) {
                    throw EpistemicError("Binary operation arguments must evaluate to integers.")
                }
                exp.setType(I32())

            case If(cond, thenBranch, elseBranch) =>
                typecheckExpression(cond)
                typecheckExpression(thenBranch)
                elseBranch.foreach(e => typecheckExpression(e))
                exp.setType(I32())

            case While(cond, body, _) =>
                typecheckExpression(cond)
                typecheckExpression(body)
                exp.setType(I32())

            case Block(statements, blockExp) =>
                val preBlockSnapshot = symbols.toMap

                statements.foreach(s => typecheckStatement(s))
                blockExp match {
                    case Some(e) =>
                        typecheckExpression(e)
                        exp.setType(e.getType)
                    case None =>
                        exp.setType(I32())
                }

                symbols.clear()
                symbols.addAll(preBlockSnapshot)

            case Return(e) =>
                typecheckExpression(e)
                exp.setType(e.getType)

            case Break(_) =>
                exp.setType(I32())

            case Continue(_) =>
                exp.setType(I32())
        }
    }
}
