package tac

import syntax.*
import scala.collection.mutable.ListBuffer

object Tac {
    case class Program(items: FunctionDef)

    case class FunctionDef(name: String, body: List[Instruction])

    abstract sealed class Instruction
    case class Return(value: Val)                                          extends Instruction
    case class Unary(unaryOp: UnaryOp, src: Val, dest: Val)                extends Instruction
    case class Binary(binaryOp: BinaryOp, src1: Val, src2: Val, dest: Val) extends Instruction
    case class Copy(src: Val, dest: Val)                                   extends Instruction
    case class Label(name: String)                                         extends Instruction
    case class Jump(target: Label)                                         extends Instruction
    case class JumpIfZero(cond: Val, target: Label)                        extends Instruction
    case class JumpIfNotZero(cond: Val, target: Label)                     extends Instruction

    abstract sealed class Val
    case class Constant(value: Int) extends Val
    case class Var(value: String)   extends Val

    enum UnaryOp {
        case Complement, Negate, Not
    }

    enum BinaryOp {
        case Add, Subtract, Multiply, Divide, Remainder,
            And, Or, Equal, NotEqual, LessThan, LessOrEqual,
            GreaterThan, GreaterOrEqual
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
        case BinaryOp.Add       => Tac.BinaryOp.Add
        case BinaryOp.Subtract  => Tac.BinaryOp.Subtract
        case BinaryOp.Multiply  => Tac.BinaryOp.Multiply
        case BinaryOp.Divide    => Tac.BinaryOp.Divide
        case BinaryOp.Remainder => Tac.BinaryOp.Remainder
        // case BinaryOp.And            => Tac.BinaryOp.And
        // case BinaryOp.Or             => Tac.BinaryOp.Or
        case BinaryOp.Equal          => Tac.BinaryOp.Equal
        case BinaryOp.NotEqual       => Tac.BinaryOp.NotEqual
        case BinaryOp.GreaterThan    => Tac.BinaryOp.GreaterThan
        case BinaryOp.LessThan       => Tac.BinaryOp.LessThan
        case BinaryOp.GreaterOrEqual => Tac.BinaryOp.GreaterOrEqual
        case BinaryOp.LessOrEqual    => Tac.BinaryOp.LessOrEqual
    }

    def emitExpressionTac(e: Expression): Tac.Val = e match {
        case Constant(value) =>
            Tac.Constant(value)

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

    }

    def emitStatementTac(s: Statement): Unit = s match {
        case Return(exp) => {
            val resultVal = emitExpressionTac(exp)
            instructions += Tac.Return(resultVal)
        }
    }

    def emitProgramTac(): Tac.Program = {
        val funcDef = prog.items
        instructions.clear()

        emitStatementTac(funcDef.body)

        Tac.Program(Tac.FunctionDef(funcDef.name, instructions.toList))
    }
}
