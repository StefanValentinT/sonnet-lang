package syntax

import collection.mutable.Map

case class Program(items: FunctionDef)

case class FunctionDef(name: String, body: List[Statement])

abstract sealed class Statement
case class Return(exp: Expression)         extends Statement
case class ExpressionStmt(exp: Expression) extends Statement

abstract sealed class Expression
case class Constant(value: Int)                                     extends Expression
case class Var(name: String)                                        extends Expression
case class Unary(op: UnaryOp, exp: Expression)                      extends Expression
case class Binary(op: BinaryOp, exp1: Expression, exp2: Expression) extends Expression
case class Assignment(target: Expression, value: Expression)        extends Expression
case class Declaration(name: Var, init: Option[Expression])         extends Expression

enum UnaryOp {
    case Complement, Negate, Not
}

enum BinaryOp {
    case Add, Subtract, Multiply, Divide, Remainder,
        And, Or, Equal, NotEqual, LessThan, LessOrEqual, GreaterThan, GreaterOrEqual,
        BitAnd, BitOr, BitXor, LShift, RShift
}
