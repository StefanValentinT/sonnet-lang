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
                  case d: Declaration          => d
                  case f: FunctionDef          => labelFunctionDef(f)
                  case v: GlobalVarDeclaration => labelGlobalVarDeclaration(v)
              }
          )
        )
    }

    def labelFunctionDef(f: FunctionDef): FunctionDef = {
        FunctionDef(f.name, f.params, f.typ, labelExpression(f.body, None), f.linkage)
    }
    def labelGlobalVarDeclaration(v: GlobalVarDeclaration): GlobalVarDeclaration = {
        val labeledInit = v.init.map(e => labelExpression(e, None))
        GlobalVarDeclaration(v.name, v.typ, labeledInit, v.linkage)
    }

    def labelStatement(stmt: Statement, currentLabel: Option[String]): Statement = {
        stmt match {
            case ExpressionStmt(exp) =>
                ExpressionStmt(labelExpression(exp, currentLabel))

            case VarDeclaration(vNode, typ, Some(initExp)) =>
                val labeledInit = labelExpression(initExp, currentLabel)
                VarDeclaration(vNode, typ, Some(labeledInit))

            case VarDeclaration(vNode, typ, None) =>
                VarDeclaration(vNode, typ, None)
        }
    }

    def labelExpression(exp: Expression, currentLabel: Option[String]): Expression = {
        exp match {
            case Cast(exp, targetType) => Cast(labelExpression(exp, currentLabel), targetType)

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
            case t @ TrueExpr()             => t
            case f @ FalseExpr()            => f
        }
    }
}
