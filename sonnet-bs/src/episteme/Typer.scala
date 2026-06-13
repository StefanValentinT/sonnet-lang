package episteme

import syntax.*
import scala.collection.mutable.Map
import app.CompilerError
import scala.compiletime.ops.boolean

object Typed {
    case class Program(items: List[TopLevelItem])

    abstract sealed class TopLevelItem
    case class Declaration(name: String, typ: Type)                                                                          extends TopLevelItem
    case class FunctionDef(name: String, params: List[(String, Type)], returnType: Type, body: Expression, linkage: Linkage) extends TopLevelItem
    case class GlobalVarDeclaration(name: String, init: Option[Expression], typ: Type, linkage: Linkage)                     extends TopLevelItem

    abstract sealed class Statement
    case class VarDeclaration(name: String, typ: Type, init: Option[Expression]) extends Statement
    case class ExpressionStmt(exp: Expression)                                   extends Statement

    abstract sealed class Expression
    case class Constant(const: Const, typ: Type)                                                       extends Expression
    case class Var(name: String, typ: Type)                                                            extends Expression
    case class Unary(op: UnaryOp, exp: Expression, typ: Type)                                          extends Expression
    case class Cast(exp: Expression, targetType: Type)                                                 extends Expression
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

def getTypedType(expr: Typed.Expression): Type =
    expr match {
        case Typed.Constant(_, t)        => t
        case Typed.Cast(_, t)            => t
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

val CheckedError = EpistemicError("This has been checked in variable resolution pass.")

case class SymbolEntry(typ: Type, isDefined: Boolean)

object TypeChecker {
    private val symbols = Map[String, Type]()

    def typecheckProgram(p: Program): Typed.Program = {
        symbols.clear()
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
        symbols.put(v.name, v.typ)
        val typedInit = v.init.map({ (e) =>
            val typedExpr = typecheckExpression(e)
            val itsType   = getTypedType(typedExpr)
            if (itsType != v.typ) {
                throw EpistemicError(s"Global variable '${v.name}' type mismatch. Expected ${v.typ}, but got ${itsType}.")
            }
            typedExpr
        })
        Typed.GlobalVarDeclaration(v.name, typedInit, v.typ, v.linkage)
    }

    private def typecheckFunctionDef(f: FunctionDef): Typed.FunctionDef = {
        val (paramTypes, returnType) = f.typ match {
            case FunType(ps, ret) => (ps, ret)
            case t                => throw EpistemicError(s"Function ${f.name} has an invalid type signature.")
        }

        symbols.get(f.name) match {
            // Has been declared previously
            case Some(FunType(declParams, declRet)) =>
                if (declParams != paramTypes || declRet != returnType) {
                    throw EpistemicError(s"Function ${f.name} signature redefinition mismatch. Declared as ${FunType(declParams, declRet)}, defined as ${FunType(paramTypes, returnType)}.")
                }
            case Some(_) =>
                throw EpistemicError(s"Identifier ${f.name} redefined as a function.")
            case None =>
                ()
        }

        symbols.put(f.name, FunType(paramTypes, returnType))

        val typedParams = f.params
            .zip(paramTypes)
            .map({ case (paramName, paramType) =>
                symbols.put(paramName, paramType)
                (paramName, paramType)
            })

        val typedBody = typecheckExpression(f.body)

        if (getTypedType(typedBody) != returnType) {
            throw EpistemicError(s"Function ${f.name} return type mismatch. Expected $returnType, got ${getTypedType(typedBody)}")
        }

        Typed.FunctionDef(f.name, typedParams, returnType, typedBody, f.linkage)
    }

    private def typecheckStatement(stmt: Statement): Typed.Statement = {
        stmt match {
            case ExpressionStmt(exp) =>
                Typed.ExpressionStmt(typecheckExpression(exp))
            case VarDeclaration(name, typ, init) => {
                symbols.put(name, typ)
                val typedInit = init.map((e) => {
                    val typedExpr = typecheckExpression(e)
                    val itsType   = getTypedType(typedExpr)
                    if (itsType != typ) {
                        throw EpistemicError(s"Local variable '$name' type mismatch. Expected $typ, got ${itsType}")
                    }
                    typedExpr
                })
                Typed.VarDeclaration(name, typ, typedInit)
            }
        }
    }

    private def typecheckExpression(exp: Expression): Typed.Expression = {
        exp match {
            case Constant(Const.I8Lit(value)) =>
                Typed.Constant(Const.I8Lit(value), I8())
            case Constant(Const.I16Lit(value)) =>
                Typed.Constant(Const.I16Lit(value), I16())
            case Constant(Const.I32Lit(value)) =>
                Typed.Constant(Const.I32Lit(value), I32())
            case Constant(Const.I64Lit(value)) =>
                Typed.Constant(Const.I64Lit(value), I64())

            case Constant(Const.U8Lit(value)) =>
                Typed.Constant(Const.U8Lit(value), U8())
            case Constant(Const.U16Lit(value)) =>
                Typed.Constant(Const.U16Lit(value), U16())
            case Constant(Const.U32Lit(value)) =>
                Typed.Constant(Const.U32Lit(value), U32())
            case Constant(Const.U64Lit(value)) =>
                Typed.Constant(Const.U64Lit(value), U64())
            case Var(name) =>
                symbols.get(name) match {
                    case Some(FunType(_, _)) => throw CheckedError
                    case Some(t)             => Typed.Var(name, t)
                    case None                => throw CheckedError
                }
            case FunctionCall(target, args) =>
                symbols.get(target) match {
                    case Some(FunType(paramTypes, retType)) => {
                        if (paramTypes.length != args.length) {
                            throw EpistemicError(s"Function $target expected ${paramTypes.length} arguments, got ${args.length}.")
                        }
                        val typedArgs = args.map(typecheckExpression)
                        typedArgs
                            .zip(paramTypes)
                            .foreach({ case (arg, expectedType) =>
                                if (getTypedType(arg) != expectedType) {
                                    throw EpistemicError(s"Argument type mismatch for function $target. Expected $expectedType, got ${getTypedType(arg)}.")
                                }
                            })
                        Typed.FunctionCall(target, typedArgs, retType)
                    }
                    case _ => throw EpistemicError(s"Identifier '$target' is not a callable function.")
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
                val t         = getTypedType(typedExpr)
                if (!isNumericType(t)) {
                    throw EpistemicError("Unary operator expects integer value.")
                }
                Typed.Unary(op, typedExpr, t)
            }
            case Binary(op, exp1, exp2) => {
                val typedE1 = typecheckExpression(exp1)
                val typedE2 = typecheckExpression(exp2)
                val t1      = getTypedType(typedE1)
                val t2      = getTypedType(typedE2)
                if (t1 != t2) {
                    throw EpistemicError(s"Type mismatch in binary operation: $t1 and $t2 do not match.")
                }
                if (!isNumericType(t1)) {
                    throw EpistemicError(s"Binary operator requires numeric types, found: $t1")
                }
                if (isComparisonOp(op)) {
                    Typed.Binary(op, typedE1, typedE2, I32())
                } else {
                    Typed.Binary(op, typedE1, typedE2, t1)
                }
            }
            case Cast(exp, targetType) => Typed.Cast(typecheckExpression(exp), targetType)
            case If(cond, thenBranch, elseBranch) => {
                val typedCond = typecheckExpression(cond)
                val typedThen = typecheckExpression(thenBranch)
                val typedElse = elseBranch.map({ (e) => typecheckExpression(e) })
                if (getTypedType(typedCond) != I32()) {
                    throw EpistemicError("If condition statement must evaluate to an I32.")
                }
                typedElse match {
                    case Some(e) if getTypedType(typedThen) != getTypedType(e) =>
                        throw EpistemicError("Then and Else branches must yield the same type.")
                    case _ => ()
                }
                Typed.If(typedCond, typedThen, typedElse, getTypedType(typedThen))
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

    private def isNumericType(t: Type): Boolean = t match {
        case I8() | I16() | I32() | I64() | U8() | U16() | U32() | U64() => true
        case _                                                           => false
    }

    private def isComparisonOp(op: BinaryOp): Boolean =
        op match {
            case BinaryOp.Equal          => true
            case BinaryOp.NotEqual       => true
            case BinaryOp.LessThan       => true
            case BinaryOp.GreaterThan    => true
            case BinaryOp.LessOrEqual    => true
            case BinaryOp.GreaterOrEqual => true
            case _                       => false
        }
}
