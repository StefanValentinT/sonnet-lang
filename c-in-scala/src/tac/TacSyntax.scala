package tac

import syntax.*
import scala.collection.mutable.ListBuffer
import tac.Tac.JumpIfZero
import pprint.pprintln

object Tac {
    case class Program(items: List[FunctionDef])

    case class FunctionDef(name: String, params: List[String], body: List[Instruction])

    abstract sealed class Instruction
    case class Return(value: Val)                                          extends Instruction
    case class Unary(unaryOp: UnaryOp, src: Val, dest: Val)                extends Instruction
    case class Binary(binaryOp: BinaryOp, src1: Val, src2: Val, dest: Val) extends Instruction
    case class Copy(src: Val, dest: Val)                                   extends Instruction
    case class Label(name: String)                                         extends Instruction
    case class Jump(target: Label)                                         extends Instruction
    case class JumpIfZero(cond: Val, target: Label)                        extends Instruction
    case class JumpIfNotZero(cond: Val, target: Label)                     extends Instruction
    case class FunctionCall(target: String, args: List[Val], dest: Val)    extends Instruction

    abstract sealed class Val
    case class Constant(value: Int) extends Val
    case class Var(value: String)   extends Val

    enum UnaryOp {
        case Complement, Negate, Not
    }

    enum BinaryOp {
        case Add, Subtract, Multiply, Divide, Remainder,
            And, Or, Equal, NotEqual, LessThan, LessOrEqual,
            GreaterThan, GreaterOrEqual,
            BitAnd, BitOr, BitXor, LShift, RShift
    }

}

class TacEmitter(prog: Program) {

    private var tempCounter  = 0
    private var labelCounter = 0
    private val instructions = ListBuffer[Tac.Instruction]()

    private def newTemp(): Tac.Var = {
        tempCounter += 1
        Tac.Var(s"t$tempCounter")
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

    def emitExpressionTac(e: Expression): Tac.Val = e match {
        case Constant(value) =>
            Tac.Constant(value)
        case Var(value) =>
            Tac.Var(value)
        case Assignment(Var(v), rhs) => {
            val res = emitExpressionTac(rhs)
            instructions += Tac.Copy(res, Tac.Var(v))
            Tac.Var(v)
        }
        case Block(statements, exp) => {
            statements.foreach(emitStatementTac)
            if exp.isDefined then emitExpressionTac(exp.get) else Tac.Constant(0)
        }
        case If(cond, thenB, None) => {
            val c         = emitExpressionTac(cond)
            val dest      = newTemp()
            val elseLabel = newLabel()
            val endLabel  = newLabel()
            instructions += Tac.JumpIfZero(c, elseLabel)
            val v1 = emitExpressionTac(thenB)
            instructions += Tac.Copy(v1, dest)
            instructions += Tac.Jump(endLabel)
            instructions += elseLabel
            instructions += Tac.Copy(Tac.Constant(0), dest)
            instructions += endLabel
            dest
        }
        case If(cond, thenB, Some(elseB)) => {
            val c         = emitExpressionTac(cond)
            val dest      = newTemp()
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
        case Return(exp) => {
            val resultVal = emitExpressionTac(exp)
            instructions += Tac.Return(resultVal)
            Tac.Constant(0)
        }
        case Unary(op, exp) => {
            val srcVal  = emitExpressionTac(exp)
            val destVar = newTemp()
            instructions += Tac.Unary(convertUnOp(op), srcVal, destVar)
            destVar
        }

        case Binary(BinaryOp.And, e1, e2) => {
            val v1         = emitExpressionTac(e1)
            val falseLabel = newLabel()
            val endLabel   = newLabel()
            instructions += Tac.JumpIfZero(v1, falseLabel)
            val v2 = emitExpressionTac(e2)
            instructions += Tac.JumpIfZero(v2, falseLabel)
            val dest = newTemp()
            instructions += Tac.Copy(Tac.Constant(1), dest)
            instructions += Tac.Jump(endLabel)
            instructions += falseLabel
            instructions += Tac.Copy(Tac.Constant(0), dest)
            instructions += endLabel
            dest
        }

        case Binary(BinaryOp.Or, e1, e2) => {
            val v1         = emitExpressionTac(e1)
            val falseLabel = newLabel()
            val endLabel   = newLabel()
            instructions += Tac.JumpIfNotZero(v1, falseLabel)
            val v2 = emitExpressionTac(e2)
            instructions += Tac.JumpIfNotZero(v2, falseLabel)
            val dest = newTemp()
            instructions += Tac.Copy(Tac.Constant(1), dest)
            instructions += Tac.Jump(endLabel)
            instructions += falseLabel
            instructions += Tac.Copy(Tac.Constant(0), dest)
            instructions += endLabel
            dest
        }

        case Binary(op, e1, e2) => {
            val v1   = emitExpressionTac(e1)
            val v2   = emitExpressionTac(e2)
            val dest = newTemp()
            instructions += Tac.Binary(convertBinOp(op), v1, v2, dest)
            dest
        }

        case While(cond, body, label) => {
            val breakLabel    = Tac.Label(s"break_$label")
            val continueLabel = Tac.Label(s"continue_$label")
            instructions += continueLabel
            val v = emitExpressionTac(cond)
            instructions += Tac.JumpIfZero(v, breakLabel)
            val b = emitExpressionTac(body)
            instructions += Tac.Jump(continueLabel)
            instructions += breakLabel
            Tac.Constant(0)
        }

        case Break(label) => {
            instructions += Tac.Jump(Tac.Label(s"break_$label"))
            Tac.Constant(0)
        }
        case Continue(label) => {
            instructions += Tac.Jump(Tac.Label(s"continue_$label"))
            Tac.Constant(0)
        }
        case FunctionCall(target, args) => {
            val dest = newTemp()
            instructions += Tac.FunctionCall(target, args.map(emitExpressionTac), dest)
            dest
        }
    }

    def emitStatementTac(s: Statement): Unit = s match {
        case ExpressionStmt(exp) => {
            emitExpressionTac(exp)
        }
        case VarDeclaration(Var(name), initializerOpt) =>
            initializerOpt match {
                case Some(initExpr) =>
                    val res = emitExpressionTac(initExpr)
                    instructions += Tac.Copy(res, Tac.Var(name))
                case None =>
            }
    }

    def emitFunctionDef(funcDef: FunctionDef): Tac.FunctionDef = {
        instructions.clear()
        val ret = emitExpressionTac(funcDef.body)
        instructions += Tac.Return(ret)
        Tac.FunctionDef(funcDef.name, funcDef.params, instructions.toList)
    }

    def emitProgramTac(): Tac.Program = {
        val funs = ListBuffer[Tac.FunctionDef]()
        prog.items.foreach {
            case f: FunctionDef => funs.append(emitFunctionDef(f))
            case _              => ()
        }
        Tac.Program(funs.toList)
    }
}
