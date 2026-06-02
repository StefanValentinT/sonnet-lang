package episteme

import syntax.*
import scala.collection.mutable.Map
import app.CompilerError

object LoopLabeler {
    private var loopCounter = 0

    private def makeLabel(): String = {
        loopCounter += 1
        s"loop_${loopCounter}"
    }

    def labelProgram(p: Program): Program = {
        Program(
          p.items.map(e =>
              e match {
                  case d: Declaration => d
                  case f: FunctionDef => labelFunctionDef(f)
              }
          )
        )
    }

    def labelFunctionDef(f: FunctionDef): FunctionDef = {
        FunctionDef(f.name, f.params, labelExpression(f.body, None))
    }

    def labelStatement(stmt: Statement, currentLabel: Option[String]): Statement = {
        stmt match {
            case ExpressionStmt(exp) =>
                ExpressionStmt(labelExpression(exp, currentLabel))

            case VarDeclaration(vNode, Some(initExp)) =>
                val labeledInit = labelExpression(initExp, currentLabel)
                VarDeclaration(vNode, Some(labeledInit))

            case VarDeclaration(vNode, None) =>
                VarDeclaration(vNode, None)
        }
    }

    def labelExpression(exp: Expression, currentLabel: Option[String]): Expression = {
        exp match {

            case While(cond, body, _) => {
                val newLabel    = makeLabel()
                val labeledCond = labelExpression(cond, Some(newLabel))
                val labeledBody = labelExpression(body, Some(newLabel))

                While(labeledCond, labeledBody, newLabel)
            }

            case Break(_) => {
                currentLabel match {
                    case Some(label) => Break(label)
                    case None        => throw EpistemicError("break statement outside of loop")
                }
            }

            case Continue(_) => {
                currentLabel match {
                    case Some(label) => Continue(label)
                    case None        => throw EpistemicError("continue statement outside of loop")
                }
            }

            case Block(stmts, finalExp) => {
                val labeledStmts = stmts.map(s => labelStatement(s, currentLabel))
                val labeledExp   = finalExp.map(e => labelExpression(e, currentLabel))
                Block(labeledStmts, labeledExp)
            }

            case If(cond, thenBranch, elseBranch) => {
                val labeledCond = labelExpression(cond, currentLabel)
                val labeledThen = labelExpression(thenBranch, currentLabel)
                val labeledElse = elseBranch.map(e => labelExpression(e, currentLabel))
                If(labeledCond, labeledThen, labeledElse)
            }

            case Assignment(target, value) => {
                val labeledTarget = labelExpression(target, currentLabel)
                val labeledValue  = labelExpression(value, currentLabel)
                Assignment(labeledTarget, labeledValue)
            }

            case Unary(op, e) =>
                Unary(op, labelExpression(e, currentLabel))

            case Binary(op, exp1, exp2) =>
                Binary(op, labelExpression(exp1, currentLabel), labelExpression(exp2, currentLabel))

            case Return(e) =>
                Return(labelExpression(e, currentLabel))
            case FunctionCall(target, args) => FunctionCall(target, args.map(labelExpression(_, currentLabel)))
            case c @ Constant(_)            => c
            case v @ Var(_)                 => v
        }
    }
}
