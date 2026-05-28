package syntax

import collection.mutable.Map

case class Program(items: FunctionDef)

case class FunctionDef(name: String, body: Statement)

abstract class Statement
case class Return(exp: Expression) extends Statement

abstract class Expression
case class Constant(value: Int) extends Expression
