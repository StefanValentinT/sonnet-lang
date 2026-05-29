package syntax

import collection.mutable.Map

case class Program(items: FunctionDef)

case class FunctionDef(name: String, body: Statement)

abstract sealed class Statement
case class Return(exp: Expression) extends Statement

abstract sealed class Expression
case class Constant(value: Int)                extends Expression
case class Unary(op: UnaryOp, exp: Expression) extends Expression

enum UnaryOp {
    case Complement, Negate
}
