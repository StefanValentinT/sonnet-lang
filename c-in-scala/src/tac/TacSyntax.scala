package tac

import syntax.*
import scala.collection.mutable.ListBuffer

object Tac {
    case class Program(items: FunctionDef)

    case class FunctionDef(name: String, body: List[Instruction])

    abstract sealed class Instruction
    case class Return(value: Val)                           extends Instruction
    case class Unary(unaryOp: UnaryOp, src: Val, dest: Val) extends Instruction

    abstract sealed class Val
    case class Constant(value: Int) extends Val
    case class Var(value: String)   extends Val

    enum UnaryOp {
        case Complement, Negate
    }

}

class TacEmitter(prog: Program) {

    private var tempCounter  = 0
    private val instructions = ListBuffer[Tac.Instruction]()

    private def newTemp(): Tac.Var = {
        tempCounter += 1
        Tac.Var(s"t$tempCounter")
    }

    private def convertUnOp(op: UnaryOp): Tac.UnaryOp = op match {
        case UnaryOp.Complement => Tac.UnaryOp.Complement
        case UnaryOp.Negate     => Tac.UnaryOp.Negate
    }

    def emitExpressionTac(e: Expression): Tac.Val = e match {
        case Constant(value) =>
            Tac.Constant(value)

        case Unary(op, exp) =>
            val srcVal  = emitExpressionTac(exp)
            val destVar = newTemp()
            instructions += Tac.Unary(convertUnOp(op), srcVal, destVar)
            destVar
    }

    def emitStatementTac(s: Statement): Unit = s match {
        case Return(exp) =>
            val resultVal = emitExpressionTac(exp)
            instructions += Tac.Return(resultVal)
    }

    def emitProgramTac(): Tac.Program = {
        val funcDef = prog.items
        instructions.clear()

        emitStatementTac(funcDef.body)

        Tac.Program(Tac.FunctionDef(funcDef.name, instructions.toList))
    }
}
