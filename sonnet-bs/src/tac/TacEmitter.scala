package tac

import episteme.Typed
import scala.collection.mutable.ListBuffer
import tac.Tac.JumpIfZero
import pprint.pprintln
import app.CompilerError
import syntax.*

class TacEmitterError(detail: String) extends CompilerError("Three-address-code generator", detail)

class TacEmitter(prog: Typed.Program) {
    private var tempCounter  = 0
    private var labelCounter = 0
    private val instructions = ListBuffer[Tac.Instruction]()

    private def newTemp(t: Tac.Type): Tac.Var = {
        tempCounter += 1
        Tac.Var(s"t$tempCounter", t)
    }

    private def newLabel(): Tac.Label = {
        labelCounter += 1
        Tac.Label(s"l$labelCounter")
    }

    private def convertUnOp(op: UnaryOp): Tac.UnaryOp = op match {
        case UnaryOp.Complement => Tac.UnaryOp.Complement
        case UnaryOp.Negate     => Tac.UnaryOp.Negate
        case UnaryOp.Not        => Tac.UnaryOp.Not
    }

    private def convertBinOp(op: BinaryOp): Tac.BinaryOp = op match {
        case BinaryOp.Add            => Tac.BinaryOp.Add
        case BinaryOp.Subtract       => Tac.BinaryOp.Subtract
        case BinaryOp.Multiply       => Tac.BinaryOp.Multiply
        case BinaryOp.Divide         => Tac.BinaryOp.Divide
        case BinaryOp.Remainder      => Tac.BinaryOp.Remainder
        case BinaryOp.Equal          => Tac.BinaryOp.Equal
        case BinaryOp.NotEqual       => Tac.BinaryOp.NotEqual
        case BinaryOp.GreaterThan    => Tac.BinaryOp.GreaterThan
        case BinaryOp.LessThan       => Tac.BinaryOp.LessThan
        case BinaryOp.GreaterOrEqual => Tac.BinaryOp.GreaterOrEqual
        case BinaryOp.LessOrEqual    => Tac.BinaryOp.LessOrEqual
        case BinaryOp.BitAnd         => Tac.BinaryOp.BitAnd
        case BinaryOp.BitOr          => Tac.BinaryOp.BitOr
        case BinaryOp.BitXor         => Tac.BinaryOp.BitXor
        case BinaryOp.LShift         => Tac.BinaryOp.LShift
        case BinaryOp.RShift         => Tac.BinaryOp.RShift
    }

    private def defaultVal(t: Type): Tac.Constant = t match {
        case I8()  => Tac.Constant(Const.I8Lit(0))
        case I16() => Tac.Constant(Const.I16Lit(0))
        case I32() => Tac.Constant(Const.I32Lit(0))
        case I64() => Tac.Constant(Const.I64Lit(0))

        case U8()  => Tac.Constant(Const.U8Lit(0))
        case U16() => Tac.Constant(Const.U16Lit(0))
        case U32() => Tac.Constant(Const.U32Lit(0))
        case U64() => Tac.Constant(Const.U64Lit(0))

        case F16() => Tac.Constant(Const.F16Lit(BigDecimal(0)))
        case F32() => Tac.Constant(Const.F32Lit(BigDecimal(0)))
        case F64() => Tac.Constant(Const.F64Lit(BigDecimal(0)))
    }

    private def convertType(t: Type): Tac.Type = t match {
        case I8()  => Tac.I8()
        case I16() => Tac.I16()
        case I32() => Tac.I32()
        case I64() => Tac.I64()

        case U8()  => Tac.U8()
        case U16() => Tac.U16()
        case U32() => Tac.U32()
        case U64() => Tac.U64()

        case F16() => Tac.F16()
        case F32() => Tac.F32()
        case F64() => Tac.F64()

        case _ => throw TacEmitterError("Not all types are supported in the backend yet.")
    }

    private def isGlobal(linkage: Linkage): Boolean =
        linkage match {
            case Linkage.Public  => true
            case Linkage.Private => false
        }

    def emitExpressionTac(e: Typed.Expression): Tac.Val = e match {
        case Typed.Constant(value, typ) =>
            Tac.Constant(value)
        case Typed.Var(value, typ) =>
            Tac.Var(value, convertType(typ))
        case Typed.Assignment(Typed.Var(v, varType), rhs, typ) => {
            val res = emitExpressionTac(rhs)
            instructions += Tac.Copy(res, Tac.Var(v, convertType(varType)))
            Tac.Var(v, convertType(varType))
        }
        case Typed.Block(statements, exp, typ) => {
            statements.foreach(emitStatementTac)
            if exp.isDefined then emitExpressionTac(exp.get) else defaultVal(typ)
        }
        case Typed.If(cond, thenB, None, typ) => {
            val c         = emitExpressionTac(cond)
            val dest      = newTemp(convertType(typ))
            val elseLabel = newLabel()
            val endLabel  = newLabel()
            instructions += Tac.JumpIfZero(c, elseLabel)
            val v1 = emitExpressionTac(thenB)
            instructions += Tac.Copy(v1, dest)
            instructions += Tac.Jump(endLabel)
            instructions += elseLabel
            instructions += Tac.Copy(defaultVal(typ), dest)
            instructions += endLabel
            dest
        }
        case Typed.If(cond, thenB, Some(elseB), typ) => {
            val c         = emitExpressionTac(cond)
            val dest      = newTemp(convertType(typ))
            val elseLabel = newLabel()
            val endLabel  = newLabel()
            instructions += Tac.JumpIfZero(c, elseLabel)
            val v1 = emitExpressionTac(thenB)
            instructions += Tac.Copy(v1, dest)
            instructions += Tac.Jump(endLabel)
            instructions += elseLabel
            val v2 = emitExpressionTac(elseB)
            instructions += Tac.Copy(v2, dest)
            instructions += endLabel
            dest
        }
        case Typed.Unary(op, exp, typ) => {
            val srcVal  = emitExpressionTac(exp)
            val destVar = newTemp(convertType(typ))
            instructions += Tac.Unary(convertUnOp(op), srcVal, destVar)
            destVar
        }

        case Typed.Binary(BinaryOp.And, e1, e2, typ) => {
            val v1         = emitExpressionTac(e1)
            val falseLabel = newLabel()
            val endLabel   = newLabel()
            instructions += Tac.JumpIfZero(v1, falseLabel)
            val v2 = emitExpressionTac(e2)
            instructions += Tac.JumpIfZero(v2, falseLabel)
            val dest = newTemp(Tac.I32())
            instructions += Tac.Copy(Tac.Constant(Const.I32Lit(1)), dest)
            instructions += Tac.Jump(endLabel)
            instructions += falseLabel
            instructions += Tac.Copy(Tac.Constant(Const.I32Lit(0)), dest)
            instructions += endLabel
            dest
        }

        case Typed.Binary(BinaryOp.Or, e1, e2, typ) => {
            val v1         = emitExpressionTac(e1)
            val falseLabel = newLabel()
            val endLabel   = newLabel()
            instructions += Tac.JumpIfNotZero(v1, falseLabel)
            val v2 = emitExpressionTac(e2)
            instructions += Tac.JumpIfNotZero(v2, falseLabel)
            val dest = newTemp(Tac.I32())
            instructions += Tac.Copy(Tac.Constant(Const.I32Lit(1)), dest)
            instructions += Tac.Jump(endLabel)
            instructions += falseLabel
            instructions += Tac.Copy(Tac.Constant(Const.I32Lit(0)), dest)
            instructions += endLabel
            dest
        }

        case Typed.Binary(op, e1, e2, typ) => {
            val v1   = emitExpressionTac(e1)
            val v2   = emitExpressionTac(e2)
            val dest = newTemp(convertType(typ))
            instructions += Tac.Binary(convertBinOp(op), v1, v2, dest)
            dest
        }

        case Typed.While(cond, body, label, typ) => {
            val breakLabel    = Tac.Label(s"break_$label")
            val continueLabel = Tac.Label(s"continue_$label")
            instructions += continueLabel
            val v = emitExpressionTac(cond)
            instructions += Tac.JumpIfZero(v, breakLabel)
            val b = emitExpressionTac(body)
            instructions += Tac.Jump(continueLabel)
            instructions += breakLabel
            Tac.Constant(Const.I32Lit(0))
        }

        case Typed.Cast(exp, targetType) => {
            val res      = emitExpressionTac(exp)
            val srcType  = getTacValType(res)
            val destType = convertType(targetType)
            val dest     = newTemp(destType)

            val srcSize  = Size.fromTacType(srcType).bits
            val destSize = Size.fromTacType(destType).bits

            (isFloat(srcType), isFloat(destType)) match {
                // float -> float
                case (true, true) => {
                    if (srcType == destType) return res
                    instructions += Tac.FloatToFloat(res, dest)
                    dest
                }
                // float -> int
                case (true, false) => {
                    if (isSigned(destType)) {
                        instructions += Tac.FloatToSignedInt(res, dest)
                    } else {
                        instructions += Tac.FloatToUnsignedInt(res, dest)
                    }
                    dest
                }
                // int -> float
                case (false, true) => {
                    if (isSigned(srcType)) {
                        instructions += Tac.SignedIntToFloat(res, dest)
                    } else {
                        instructions += Tac.UnsignedIntToFloat(res, dest)
                    }
                    dest
                }

                // int -> int
                case (false, false) => {
                    if (destSize == srcSize) {
                        res
                    } else if (destSize < srcSize) {
                        instructions += Tac.Truncate(res, dest)
                        dest
                    } else if (isSigned(srcType)) {
                        instructions += Tac.SignExtend(res, dest)
                        dest
                    } else {
                        instructions += Tac.ZeroExtend(res, dest)
                        dest
                    }
                }
            }
        }

        case Typed.Break(label, typ) => {
            instructions += Tac.Jump(Tac.Label(s"break_$label"))
            Tac.Constant(Const.I32Lit(0))
        }
        case Typed.Continue(label, typ) => {
            instructions += Tac.Jump(Tac.Label(s"continue_$label"))
            Tac.Constant(Const.I32Lit(0))
        }
        case Typed.Return(exp, typ) => {
            val resultVal = emitExpressionTac(exp)
            instructions += Tac.Return(resultVal)
            Tac.Constant(Const.I32Lit(0))
        }

        case Typed.FunctionCall(target, args, typ) => {
            val dest = newTemp(convertType(typ))
            instructions += Tac.FunctionCall(target, args.map(emitExpressionTac), dest)
            dest
        }
    }

    def emitStatementTac(s: Typed.Statement): Unit = s match {
        case Typed.ExpressionStmt(exp) => {
            emitExpressionTac(exp)
        }
        case Typed.VarDeclaration(name, typ, initializerOpt) =>
            initializerOpt match {
                case Some(initExpr) =>
                    val res = emitExpressionTac(initExpr)
                    instructions += Tac.Copy(res, Tac.Var(name, convertType(typ)))
                case None =>
            }
    }

    def emitFunctionDef(funcDef: Typed.FunctionDef): Tac.FunctionDef = {
        instructions.clear()
        val ret = emitExpressionTac(funcDef.body)
        instructions += Tac.Return(ret)

        val params = funcDef.params.map({ case (pName, typ) => Tac.Var(pName, convertType(typ)) })
        Tac.FunctionDef(funcDef.name, isGlobal(funcDef.linkage), params, instructions.toList)
    }

    def emitProgramTac(): Tac.Program = {
        val topItems = ListBuffer[Tac.TopLevelItem]()
        prog.items.foreach({ (item) =>
            item match {
                case f: Typed.FunctionDef =>
                    topItems.append(emitFunctionDef(f))
                case Typed.GlobalVarDeclaration(name, init, typ, linkage) => {

                    val initConst = init match {
                        case Some(Typed.Constant(value, _)) => value
                        // initialization with default value
                        case None =>
                            typ match {
                                case _ if isNumericType(typ) => defaultVal(typ).value
                                case v                       => throw TacEmitterError(s"No default initiliaziation for $v defined by this implementation.")
                            }
                        case _ => throw TacEmitterError("Initializer element is not a compile-time constant.")
                    }
                    topItems.append(Tac.StaticVariable(name, isGlobal(linkage), convertType(typ), initConst))
                }
                case _: Typed.Declaration => ()
            }
        })
        Tac.Program(topItems.toList)
    }
}
