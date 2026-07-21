package episteme

import syntax.*
import scala.collection.mutable.Map
import app.CompilerError
import scala.compiletime.ops.boolean

object Typed {
    case class Program(items: List[TopLevelItem])

    abstract sealed class TopLevelItem
    case class Declaration(name: String, typ: Type, init: Option[Expression], linkage: Linkage) extends TopLevelItem

    abstract sealed class Statement
    case class VarDeclaration(name: String, typ: Type, init: Option[Expression]) extends Statement
    case class ExpressionStmt(exp: Expression)                                   extends Statement

    abstract sealed class Expression
    case class Constant(const: Const, typ: Type)                                                       extends Expression
    case class ArrayLit(values: List[Expression], typ: ArrayType)                                      extends Expression
    case class Function(params: List[(String, Type)], returnType: Type, body: Expression)              extends TopLevelItem
    case class Var(name: String, typ: Type)                                                            extends Expression
    case class Ref(exp: Expression, typ: Type)                                                         extends Expression
    case class Deref(exp: Expression, typ: Type)                                                       extends Expression
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
    case class TrueExpr()                                                                              extends Expression
    case class FalseExpr()                                                                             extends Expression
}

def getTypedType(expr: Typed.Expression): Type =
    expr match {
        case Typed.Constant(_, t)        => t
        case Typed.ArrayLit(_, t)        => t
        case Typed.Cast(_, t)            => t
        case Typed.Var(_, t)             => t
        case Typed.Ref(_, t)             => t
        case Typed.Deref(_, t)           => t
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
        case Typed.TrueExpr()            => Bool()
        case Typed.FalseExpr()           => Bool()
    }

val CheckedError = EpistemicError("This has been checked in variable resolution pass.")

case class SymbolEntry(typ: Type, isDefined: Boolean)

object TypeChecker {
    private val symbols = Map[String, Type]()

    def typecheckProgram(p: Program): Typed.Program = {
        symbols.clear()

        p.items.foreach {
            case Declaration(name, Some(t), _, _) => symbols.put(name, t)
            case Declaration(name, None, Some(init), _) =>
                symbols.put(name, getTypedType(infer(init)))
            case syntax.Declaration(name, None, None, _) =>
                throw EpistemicError(s"Global declaration '$name' needs a type or initializer.")
            case _ => ()
        }

        val typedItems = p.items.flatMap { item =>
            item match {
                case d: syntax.Declaration => Some(typecheckDeclaration(d))
                case syntax.Import(_)      => None
            }
        }
        Typed.Program(typedItems)
    }

    private def typecheckDeclaration(v: Declaration): Typed.Declaration = {
        val resolvedType = v.typ match {
            case Some(t) => t
            case None =>
                v.init match {
                    case Some(initExpr) => getTypedType(infer(initExpr))
                    case None           => throw EpistemicError(s"Declaration of '${v.name}' needs either an explicit type or an initializer.")
                }
        }

        symbols.put(v.name, resolvedType)
        val typedInit = v.init.map { e => check(e, resolvedType) }
        Typed.Declaration(v.name, resolvedType, typedInit, v.linkage)
    }

    private def typecheckFunctionDef(f: FunctionDef): Typed.FunctionDef = {
        val (paramTypes, returnType) = f.typ match {
            case FunType(ps, ret) => (ps, ret)
            case t                => throw EpistemicError(s"Function ${f.name} has an invalid type signature.")
        }

        symbols.get(f.name) match {
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
    
    private def typecheckStatement(stmt: syntax.Statement): Typed.Statement =
        stmt match {
            case syntax.ExpressionStmt(exp) =>
                Typed.ExpressionStmt(infer(exp))
            case syntax.VarDeclaration(name, typOpt, init) => {
                val resolvedType = typOpt match {
                    case Some(t) => t
                    case None =>
                        init match {
                            case Some(i) => getTypedType(infer(i))
                            case None    => throw EpistemicError(s"Local variable '$name' needs a type or initializer.")
                        }
                }
                symbols.put(name, resolvedType)
                val typedInit = init.map { e => check(e, resolvedType) }
                Typed.VarDeclaration(name, resolvedType, typedInit)
            }
        }

    private def infer(exp: Expression): Typed.Expression =
        exp match {
            case Constant(Const.I8Lit(value)) =>
                Typed.Constant(Const.I8Lit(value), I8())
            case Constant(Const.I16Lit(value)) =>
                Typed.Constant(Const.I16Lit(value), I16())
            case Constant(Const.I32Lit(value)) =>
                Typed.Constant(Const.I32Lit(value), I32())
            case Constant(Const.I64Lit(value)) =>
                Typed.Constant(Const.I64Lit(value), I64())

            case Constant(Const.F16Lit(value)) =>
                Typed.Constant(Const.F16Lit(value), F16())
            case Constant(Const.F32Lit(value)) =>
                Typed.Constant(Const.F32Lit(value), F32())
            case Constant(Const.F64Lit(value)) =>
                Typed.Constant(Const.F64Lit(value), F64())

            case Constant(Const.U8Lit(value)) =>
                Typed.Constant(Const.U8Lit(value), U8())
            case Constant(Const.U16Lit(value)) =>
                Typed.Constant(Const.U16Lit(value), U16())
            case Constant(Const.U32Lit(value)) =>
                Typed.Constant(Const.U32Lit(value), U32())
            case Constant(Const.U64Lit(value)) =>
                Typed.Constant(Const.U64Lit(value), U64())

            case TrueExpr()  => Typed.TrueExpr()
            case FalseExpr() => Typed.FalseExpr()

            case Cast(exp, targetType) => Typed.Cast(infer(exp), targetType)

            case Var(name) =>
                symbols.get(name) match {
                    case Some(t) => Typed.Var(name, t)
                    case None    => throw CheckedError
                }
            case FunctionCall(target, args) =>
                symbols.get(target) match {
                    case Some(FunType(paramTypes, retType)) => {
                        if (paramTypes.length != args.length) {
                            throw EpistemicError(s"Function $target expected ${paramTypes.length} arguments, got ${args.length}.")
                        }
                        val typedArgs = args.map(typecheckExpression)
                        val typedArgs = args.zip(paramTypes).map { case (arg, expectedType) =>
                            check(arg, expectedType)
                        }
                        Typed.FunctionCall(target, typedArgs, retType)
                    }
                    case _ => throw EpistemicError(s"Identifier '$target' is not a callable function.")
                }
            case Assignment(target, value) => {
                target match {
                    case Var(_) | Deref(_) => ()
                    case _                 => throw EpistemicError("Invalid l-value: Target of assignment must be a variable or a dereferenced pointer.")
                }

                val typedTarget = infer(target)
                val targetType  = getTypedType(typedTarget)
                val typedValue  = check(value, targetType)

                Typed.Assignment(typedTarget, typedValue, targetType)
            }
            case Unary(op, e) => {
                val typedExpr = infer(e)
                val t         = getTypedType(typedExpr)
                op match {
                    case UnaryOp.Complement | UnaryOp.Negate => if !(isIntegerType(t)) then throw EpistemicError(s"Complement requires integer type, found: $t.")
                    case UnaryOp.Not                         => if t != Bool() then throw EpistemicError(s"Logical not requires bool type, found: $t.")
                }
                Typed.Unary(op, typedExpr, t)
            }
            case Binary(op, exp1, exp2) => {
                val typedE1 = infer(exp1)
                val typedE2 = infer(exp2)
                val t1      = getTypedType(typedE1)
                val t2      = getTypedType(typedE2)
                if (t1 != t2) {
                    val e = EpistemicError(s"Type mismatch in binary operation: $t1 and $t2 do not match.")
                    t1 match {
                        case Pointer(r) =>
                            t2 match {
                                case U64() => Typed.Binary(op, typedE1, typedE2, Pointer(r))
                                case _     => throw e
                            }
                        case U64() =>
                            t2 match {
                                case Pointer(r) => Typed.Binary(op, typedE1, typedE2, Pointer(r))
                                case _          => throw e
                            }
                        case _ => throw e
                    }
                } else {
                    if (!isNumericType(t1)) {
                        throw EpistemicError(s"Binary operator requires numeric types, found: $t1.")
                    }
                    op match {
                        case BinaryOp.Equal | BinaryOp.NotEqual | BinaryOp.LessThan | BinaryOp.LessOrEqual | BinaryOp.GreaterThan | BinaryOp.GreaterOrEqual => if (!isNumericType(t1)) then throw EpistemicError(s"Relational operator requires numeric types, found: $t1.")
                        case BinaryOp.Add | BinaryOp.Divide | BinaryOp.Subtract | BinaryOp.Multiply                                                         => if (!isNumericType(t1)) then throw EpistemicError(s"Binary operator requires numeric types, found: $t1.")
                        case BinaryOp.Remainder | BinaryOp.BitAnd | BinaryOp.BitOr | BinaryOp.BitXor | BinaryOp.LShift | BinaryOp.RShift                    => if !(isIntegerType(t1)) then throw EpistemicError(s"Remainder and Bit operators requires integer types, found: $t1.")
                        case BinaryOp.And | BinaryOp.Or                                                                                                     => if t1 != Bool() then throw EpistemicError(s"Logical operators require bool type, found: $t1.")
                    }
                    if (isComparisonOp(op)) {
                        Typed.Binary(op, typedE1, typedE2, Bool())
                    } else {
                        Typed.Binary(op, typedE1, typedE2, t1)
                    }
                }
            }
            case If(cond, thenBranch, elseBranch) => {
                val typedCond = check(cond, Bool())
                val typedThen = infer(thenBranch)
                val thenType  = getTypedType(typedThen)
                val typedElse = elseBranch.map(e => check(e, thenType))

                Typed.If(typedCond, typedThen, typedElse, thenType)
            }
            case While(cond, body, label) => {
                val typedCond = check(cond, Bool())
                val typedBody = infer(body)
                Typed.While(typedCond, typedBody, label, I32())
            }
            case Block(statements, blockExp) => {
                val typedStmts = statements.map(typecheckStatement)
                val (typedBlockExp, blockType) = blockExp match {
                    case Some(e) => {
                        val typedE = infer(e)
                        (Some(typedE), getTypedType(typedE))
                    }
                    case None => (None, I32())
                }
                Typed.Block(typedStmts, typedBlockExp, blockType)
            }
            case Return(e) => {
                val typedExpr = infer(e)
                Typed.Return(typedExpr, getTypedType(typedExpr))
            }
            case Ref(e) => {
                infer(e) match {
                    case v @ Typed.Var(_, ArrayType(elem, _)) =>
                        Typed.Ref(v, Pointer(elem))
                    case v @ Typed.Var(_, _) =>
                        Typed.Ref(v, Pointer(getTypedType(v)))
                    case _ =>
                        throw new EpistemicError("Cannot reference non-variable.")
                }
            }
            case Deref(e) => {
                val typedExpr = infer(e)
                getTypedType(typedExpr) match {
                    case Pointer(innerType) =>
                        Typed.Deref(typedExpr, innerType)

                    case _ =>
                        throw new EpistemicError("Cannot dereference non-pointer.")
                }
            }
            case Break(label) =>
                Typed.Break(label, I32())
            case Continue(label) =>
                Typed.Continue(label, I32())
        }

    private def check(exp: Expression, t: Type): Typed.Expression = {
        val at = infer(exp)
        if getTypedType(at) == t then at else throw EpistemicError(s"Expression does not have expected type $t")
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
