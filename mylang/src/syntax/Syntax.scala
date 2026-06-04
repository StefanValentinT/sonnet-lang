package syntax

import collection.mutable.Map

case class Program(items: List[TopLevelItem])

abstract sealed class TopLevelItem
case class Declaration(name: String, typ: Type)                                                extends TopLevelItem
case class FunctionDef(name: String, params: List[String], body: Expression, linkage: Linkage) extends TopLevelItem
case class GlobalVarDeclaration(name: String, init: Option[Expression], linkage: Linkage)      extends TopLevelItem

enum Linkage {
    case Public, Private
}

abstract sealed class Statement
case class VarDeclaration(name: String, init: Option[Expression]) extends Statement
case class ExpressionStmt(exp: Expression)                        extends Statement

abstract sealed class Expression
case class Constant(value: Int)                                                         extends Expression
case class Var(name: String)                                                            extends Expression
case class Unary(op: UnaryOp, exp: Expression)                                          extends Expression
case class Binary(op: BinaryOp, exp1: Expression, exp2: Expression)                     extends Expression
case class Assignment(target: Expression, value: Expression)                            extends Expression
case class If(cond: Expression, thenBranch: Expression, elseBranch: Option[Expression]) extends Expression
case class Return(exp: Expression)                                                      extends Expression
case class Break(label: String)                                                         extends Expression
case class Continue(label: String)                                                      extends Expression
case class While(cond: Expression, body: Expression, label: String)                     extends Expression
case class FunctionCall(target: String, args: List[Expression])                         extends Expression
// creates a new scope so no circular dependency between expression and statement
case class Block(statements: List[Statement], exp: Option[Expression]) extends Expression

abstract sealed class Type
case class I32()                  extends Type
case class FunType(argCount: Int) extends Type

enum UnaryOp {
    case Complement, Negate, Not
}

enum BinaryOp {
    case Add, Subtract, Multiply, Divide, Remainder,
        And, Or, Equal, NotEqual, LessThan, LessOrEqual, GreaterThan, GreaterOrEqual,
        BitAnd, BitOr, BitXor, LShift, RShift
}
