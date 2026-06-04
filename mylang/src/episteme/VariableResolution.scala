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
        val resolvedItems = p.items.map {
            case d: Declaration          => resolveGlobalDeclaration(d, globalVariableMap)
            case f: FunctionDef          => resolveFunctionDef(f, globalVariableMap)
            case v: GlobalVarDeclaration => resolveGlobalVarDeclaration(v, globalVariableMap)
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
        decl
    }

    def resolveGlobalVarDeclaration(v: GlobalVarDeclaration, variableMap: Map[String, MapEntry]): GlobalVarDeclaration = {
        if (variableMap.contains(v.name)) {
            val prevEntry = variableMap(v.name)
            if (prevEntry.fromCurrentBlock && !prevEntry.hasLinkage) {
                throw EpistemicError(s"Duplicate declaration of global variable: ${v.name}")
            }
        }
        variableMap.put(v.name, MapEntry(v.name, true, true))
        val resolvedInit = v.init.map(e => resolveExpression(e, variableMap))

        GlobalVarDeclaration(v.name, resolvedInit, v.linkage)
    }

    def resolveFunctionDef(f: FunctionDef, variableMap: Map[String, MapEntry]): FunctionDef = {
        if (variableMap.contains(f.name)) {
            val prevEntry = variableMap(f.name)
            if (prevEntry.fromCurrentBlock && !prevEntry.hasLinkage) {
                throw EpistemicError(s"Duplicate declaration: ${f.name} conflicts with a local variable.")
            }
        }
        variableMap.put(f.name, MapEntry(f.name, true, true))

        val innerMap       = copyVariableMap(variableMap)
        val resolvedParams = new ListBuffer[String]()

        for (param <- f.params) {
            if (innerMap.contains(param) && innerMap(param).fromCurrentBlock) {
                throw EpistemicError(s"Duplicate parameter declaration: $param")
            }
            val uniqueParamName = makeUnique(param)
            innerMap.put(param, MapEntry(uniqueParamName, true, false))
            resolvedParams.append(uniqueParamName)
        }

        val resolvedBody = resolveExpression(f.body, innerMap)
        FunctionDef(f.name, resolvedParams.toList, resolvedBody, f.linkage)
    }

    def resolveStatement(stmt: Statement, variableMap: Map[String, MapEntry]): Statement = {
        stmt match {
            case ExpressionStmt(exp) => ExpressionStmt(resolveExpression(exp, variableMap))
            case VarDeclaration(name, init) => {
                if (variableMap.contains(name) && variableMap(name).fromCurrentBlock) {
                    throw EpistemicError(s"Duplicate variable declaration: $name")
                }
                val uniqueName = makeUnique(name)
                variableMap.put(name, MapEntry(uniqueName, true, false))

                val resolvedInit = init.map(e => resolveExpression(e, variableMap))
                VarDeclaration(uniqueName, resolvedInit)
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
                    case Some(MapEntry(uniqueName, _, _)) => Var(uniqueName)
                    case None                             => throw EpistemicError(s"Undeclared variable: $value")
                }
            }
            case FunctionCall(target, args) =>
                variableMap.get(target) match {
                    case Some(MapEntry(uniqueName, _, _)) => FunctionCall(uniqueName, args.map(resolveExpression(_, variableMap)))
                    case None =>
                        throw EpistemicError(s"Undeclared function: $target")
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
