package episteme

import syntax.*
import scala.collection.mutable.Map
import app.CompilerError

object Typed {
    case class Program(items: List[TopLevelItem])

    abstract sealed class TopLevelItem
    case class Declaration(name: String, typ: Type)                                                                          extends TopLevelItem
    case class FunctionDef(name: String, params: List[(String, Type)], returnType: Type, body: Expression, linkage: Linkage) extends TopLevelItem
    case class GlobalVarDeclaration(name: String, init: Option[Expression], typ: Type, linkage: Linkage)                     extends TopLevelItem

    abstract sealed class Statement
    case class VarDeclaration(name: String, init: Option[Expression]) extends Statement
    case class ExpressionStmt(exp: Expression)                        extends Statement

    abstract sealed class Expression
    case class Constant(value: Int, typ: Type)                                                         extends Expression
    case class Var(name: String, typ: Type)                                                            extends Expression
    case class Unary(op: UnaryOp, exp: Expression, typ: Type)                                          extends Expression
    case class Binary(op: BinaryOp, exp1: Expression, exp2: Expression, typ: Type)                     extends Expression
    case class Assignment(target: Expression, value: Expression, typ: Type)                            extends Expression
    case class If(cond: Expression, thenBranch: Expression, elseBranch: Option[Expression], typ: Type) extends Expression
    case class Return(exp: Expression, typ: Type)                                                      extends Expression
    case class Break(label: String, typ: Type)                                                         extends Expression
    case class Continue(label: String, typ: Type)                                                      extends Expression
    case class While(cond: Expression, body: Expression, label: String, typ: Type)                     extends Expression
    case class FunctionCall(target: String, args: List[Expression], typ: Type)                         extends Expression
    case class Block(statements: List[Statement], exp: Option[Expression], typ: Type)                  extends Expression
}

val CheckedError = EpistemicError("This has been checked in variable resolution pass.")

case class SymbolEntry(typ: Type, isDefined: Boolean)

object TypeChecker {
    private val symbols = Map[String, Type]()

    def typecheckProgram(p: Program): Typed.Program = {
        val typedItems = p.items.map({ (item) =>
            item match {
                case d: Declaration          => typecheckGlobalDeclaration(d)
                case f: FunctionDef          => typecheckFunctionDef(f)
                case v: GlobalVarDeclaration => typecheckGlobalVarDeclaration(v)
            }
        })
        Typed.Program(typedItems)
    }

    private def typecheckGlobalDeclaration(decl: Declaration): Typed.Declaration = {
        symbols.put(decl.name, decl.typ)
        Typed.Declaration(decl.name, decl.typ)
    }

    private def typecheckGlobalVarDeclaration(v: GlobalVarDeclaration): Typed.GlobalVarDeclaration = {
        val varType = I32()
        symbols.put(v.name, varType)
        val typedInit = v.init.map({ (e) =>
            val typedExpr = typecheckExpression(e)
            if (getTypedType(typedExpr) != varType) {
                throw EpistemicError("Global variable initializer type mismatch.")
            }
            typedExpr
        })
        Typed.GlobalVarDeclaration(v.name, typedInit, varType, v.linkage)
    }

    private def typecheckFunctionDef(f: FunctionDef): Typed.FunctionDef = {
        val funType = FunType(f.params.length)
        symbols.put(f.name, funType)

        val typedParams = f.params.map({ (param) =>
            symbols.put(param, I32())
            (param, I32())
        })

        val typedBody = typecheckExpression(f.body)
        Typed.FunctionDef(f.name, typedParams, I32(), typedBody, f.linkage)
    }

    private def typecheckStatement(stmt: Statement): Typed.Statement = {
        stmt match {
            case ExpressionStmt(exp) =>
                Typed.ExpressionStmt(typecheckExpression(exp))
            case VarDeclaration(name, init) => {
                symbols.put(name, I32())
                val typedInit = init.map({ (e) =>
                    val typedExpr = typecheckExpression(e)
                    if (getTypedType(typedExpr) != I32()) {
                        throw EpistemicError("Variable initializer must evaluate to an integer.")
                    }
                    typedExpr
                })
                Typed.VarDeclaration(name, typedInit)
            }
        }
    }

    private def typecheckExpression(exp: Expression): Typed.Expression = {
        exp match {
            case Constant(value) =>
                Typed.Constant(value, I32())
            case Var(name) =>
                symbols.get(name) match {
                    case Some(FunType(_)) => throw CheckedError
                    case Some(t)          => Typed.Var(name, t)
                    case None             => throw CheckedError
                }
            case FunctionCall(target, args) =>
                symbols.get(target) match {
                    case Some(FunType(paramCount)) => {
                        if (paramCount != args.length) {
                            throw EpistemicError(s"Function $target called with wrong number of arguments. Expected $paramCount, got ${args.length}.")
                        }
                        val typedArgs = args.map({ (a) => typecheckExpression(a) })
                        Typed.FunctionCall(target, typedArgs, I32())
                    }
                    case Some(I32()) => throw CheckedError
                    case None        => throw CheckedError
                }
            case Assignment(target, value) => {
                val typedTarget = typecheckExpression(target)
                val typedValue  = typecheckExpression(value)
                if (getTypedType(typedTarget) != getTypedType(typedValue)) {
                    throw EpistemicError("Type mismatch inside assignment operation.")
                }
                Typed.Assignment(typedTarget, typedValue, getTypedType(typedTarget))
            }
            case Unary(op, e) => {
                val typedExpr = typecheckExpression(e)
                if (getTypedType(typedExpr) != I32()) {
                    throw EpistemicError("Unary operator expects integer value.")
                }
                Typed.Unary(op, typedExpr, I32())
            }
            case Binary(op, exp1, exp2) => {
                val typedE1 = typecheckExpression(exp1)
                val typedE2 = typecheckExpression(exp2)
                if (getTypedType(typedE1) != I32() || getTypedType(typedE2) != I32()) {
                    throw EpistemicError("Binary operation arguments must evaluate to integers.")
                }
                Typed.Binary(op, typedE1, typedE2, I32())
            }
            case If(cond, thenBranch, elseBranch) => {
                val typedCond = typecheckExpression(cond)
                val typedThen = typecheckExpression(thenBranch)
                val typedElse = elseBranch.map({ (e) => typecheckExpression(e) })
                if (getTypedType(typedCond) != I32()) {
                    throw EpistemicError("If condition statement must evaluate to an integer.")
                }
                Typed.If(typedCond, typedThen, typedElse, I32())
            }
            case While(cond, body, label) => {
                val typedCond = typecheckExpression(cond)
                val typedBody = typecheckExpression(body)
                Typed.While(typedCond, typedBody, label, I32())
            }
            case Block(statements, blockExp) => {
                val typedStmts = statements.map({ (s) => typecheckStatement(s) })
                val (typedBlockExp, blockType) = blockExp match {
                    case Some(e) => {
                        val typedE = typecheckExpression(e)
                        (Some(typedE), getTypedType(typedE))
                    }
                    case None => (None, I32())
                }
                Typed.Block(typedStmts, typedBlockExp, blockType)
            }
            case Return(e) => {
                val typedExpr = typecheckExpression(e)
                Typed.Return(typedExpr, getTypedType(typedExpr))
            }
            case Break(label) =>
                Typed.Break(label, I32())
            case Continue(label) =>
                Typed.Continue(label, I32())
        }
    }

    private def getTypedType(expr: Typed.Expression): Type =
        expr match {
            case Typed.Constant(_, t)        => t
            case Typed.Var(_, t)             => t
            case Typed.Unary(_, _, t)        => t
            case Typed.Binary(_, _, _, t)    => t
            case Typed.Assignment(_, _, t)   => t
            case Typed.If(_, _, _, t)        => t
            case Typed.Return(_, t)          => t
            case Typed.Break(_, t)           => t
            case Typed.Continue(_, t)        => t
            case Typed.While(_, _, _, t)     => t
            case Typed.FunctionCall(_, _, t) => t
            case Typed.Block(_, _, t)        => t
        }
}
