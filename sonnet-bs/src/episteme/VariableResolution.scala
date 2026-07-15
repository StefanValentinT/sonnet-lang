package episteme

import syntax.*
import scala.collection.mutable.Map
import app.CompilerError
import scala.collection.mutable.ListBuffer
import pprint.pprintln

class EpistemicError(detail: String) extends CompilerError("Semantic Compiler Pass", detail)

case class MapEntry(newName: String, fromCurrentBlock: Boolean, hasLinkage: Boolean)

object VariableResolver {
    private var varCounter = 0

    def makeUnique(orig: String): String = {
        varCounter += 1
        s"${orig}_${varCounter}"
    }

    def resolveProgram(p: Program): Program = {
        val globalVariableMap = Map[String, MapEntry]()
        val resolvedItems = p.items.map { case d: Declaration =>
            resolveGlobalDeclaration(d, globalVariableMap)
        }
        Program(resolvedItems)
    }

    def resolveGlobalDeclaration(decl: Declaration, variableMap: Map[String, MapEntry]): TopLevelItem = {
        if (variableMap.contains(decl.name)) {
            val prevEntry = variableMap(decl.name)
            if (prevEntry.fromCurrentBlock && !prevEntry.hasLinkage) {
                throw EpistemicError(s"Duplicate declaration of name: ${decl.name}")
            }
        }
        variableMap.put(decl.name, MapEntry(decl.name, true, true))
        val resolvedInit = decl.init.map(e => resolveExpression(e, variableMap))
        decl
    }

    def resolveStatement(stmt: Statement, variableMap: Map[String, MapEntry]): Statement = {
        stmt match {
            case ExpressionStmt(exp) => ExpressionStmt(resolveExpression(exp, variableMap))
            case VarDeclaration(name, typ, init) => {
                if (variableMap.contains(name) && variableMap(name).fromCurrentBlock) {
                    throw EpistemicError(s"Duplicate variable declaration: $name")
                }
                val uniqueName = makeUnique(name)
                variableMap.put(name, MapEntry(uniqueName, true, false))

                val resolvedInit = init.map(e => resolveExpression(e, variableMap))
                VarDeclaration(uniqueName, typ, resolvedInit)
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
            case Function(params, retType, body) => {
                val globalOnlyMap = Map[String, MapEntry]()
                variableMap.foreach { case (key, entry) =>
                    if (entry.hasLinkage) {
                        globalOnlyMap.put(key, entry.copy(fromCurrentBlock = false))
                    }
                }

                val resolvedParams = new ListBuffer[Formal]()

                for (param <- params) {
                    val (paramName, paramTypeOpt) = param match {
                        case Var(name)             => (name, None)
                        case Typed(Var(name), typ) => (name, Some(typ))
                        case other                 => throw EpistemicError(s"Invalid formals in function: $other")
                    }
                    if (globalOnlyMap.contains(paramName) && globalOnlyMap(paramName).fromCurrentBlock) {
                        throw EpistemicError(s"Duplicate parameter declaration: $paramName")
                    }
                    val uniqueParamName = makeUnique(paramName)
                    globalOnlyMap.put(paramName, MapEntry(uniqueParamName, true, false))
                    val resolvedFormal = paramTypeOpt match {
                        case Some(typ) => Typed(Var(uniqueParamName), typ)
                        case None      => Var(uniqueParamName)
                    }
                    resolvedParams.append(resolvedFormal)
                }

                val resolvedBody = resolveExpression(body, globalOnlyMap)
                Function(resolvedParams.toList, retType, resolvedBody)
            }
            case e @ Continue(_)        => e
            case e @ Break(_)           => e
            case t @ TrueExpr()         => t
            case f @ FalseExpr()        => f
            case While(cond, exp, l)    => While(resolveExpression(cond, variableMap), resolveExpression(exp, variableMap), l)
            case If(cond, thenB, elseB) => If(resolveExpression(cond, variableMap), resolveExpression(thenB, variableMap), if elseB.isDefined then Some(resolveExpression(elseB.get, variableMap)) else None)
            case Constant(value)        => Constant(value)
            case Unary(op, e)           => Unary(op, resolveExpression(e, variableMap))
            case Cast(exp, typ)         => Cast(resolveExpression(exp, variableMap), typ)
            case Return(exp)            => Return(resolveExpression(exp, variableMap))
            case Binary(op, exp1, exp2) => Binary(op, resolveExpression(exp1, variableMap), resolveExpression(exp2, variableMap))
            case Var(value) => {
                variableMap.get(value) match {
                    case Some(MapEntry(uniqueName, _, _)) => Var(uniqueName)
                    case None                             => throw EpistemicError(s"Undeclared variable: $value")
                }
            }
            case ArrayLit(values, typ) =>
                ArrayLit(values.map(e => resolveExpression(e, variableMap)), typ)
            case Ref(exp)   => Ref(resolveExpression(exp, variableMap))
            case Deref(exp) => Deref(resolveExpression(exp, variableMap))
            case FunctionCall(target, args) =>
                variableMap.get(target) match {
                    case Some(MapEntry(uniqueName, _, _)) => FunctionCall(uniqueName, args.map(resolveExpression(_, variableMap)))
                    case None =>
                        throw EpistemicError(s"Undeclared function: $target")
                }
            case Assignment(target, value) => {
                val resolvedTarget = resolveExpression(target, variableMap)
                val resolvedValue  = resolveExpression(value, variableMap)
                Assignment(resolvedTarget, resolvedValue)
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
