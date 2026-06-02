package syntax

import collection.mutable.Map

case class Program(items: List[TopLevelitem])

class TopLevelitem
case class Definition(name: Var, value: Expression) extends TopLevelitem
case class Declaration(name: Var, value: Type)      extends TopLevelitem
case class TypeDefinition(name: Var, value: Type)   extends TopLevelitem

class Expression
class SimpleExpression extends Expression

case class Var(value: String)                         extends SimpleExpression
case class IntLit(value: Int, intType: PrimitiveType) extends SimpleExpression // 10i8
case class StringLit(value: String)                   extends SimpleExpression

// 1 + 2 gets desugared to +(1, 2)
case class Function(params: List[(Var, Option[Type])], body: Expression)        extends Expression
case class TypedExpression(expr: Expression, typ: Type)                         extends Expression
case class VarDef(target: Var, value: Expression)                               extends Expression
case class ValDef(target: Var, value: Expression)                               extends Expression
case class SetVar(target: Var, value: Expression)                               extends Expression
case class Compound(body: List[Expression], last: Expression)                   extends Expression
case class If(cond: Expression, thenBranch: Expression, elseBranch: Expression) extends Expression
case class While(cond: Expression, body: Expression)                            extends Expression
case class FieldSelection(target: Expression, select: Var)                      extends Expression
case class CaseOf(target: Expression, branches: List[(Pattern, Expression)])       extends Expression
case class Application(target: Expression, args: List[Expression])                 extends Expression
case class ObjectInstantiation(blueprint: Type, elements: List[(Var, Expression)]) extends Expression
case class UnionInstantiation(blueprint: Type, elements: List[(Var, Expression)])  extends Expression
case class AddressOf(target: Var)                                                  extends Expression
case class Deref(target: Expression)                                               extends Expression
case class SetPointer(target: Var, to: Expression)                                 extends Expression

class Pattern
case class ExpressionPattern(pat: SimpleExpression) extends Pattern
case class WildCard()                               extends Pattern

class Type
case class TypeVar(name: String)                extends Type
case class ForAll(typeVar: TypeVar, body: Type) extends Type
case class TypeName(name: String)               extends Type
case class PointerType(ref: Type)               extends Type
case class InstantiatedType(of: Type)           extends Type
// I8 I16 I32 164 U8 U16 U32 U64 F16 F32 F64 String
// Unit Void (the bottom of the type system) Any (top) are not parsed as primtives
case class PrimitiveType(name: String)                            extends Type
case class FunctionType(paramTypes: List[Type], returnType: Type) extends Type
case class ObjectType(elements: List[(Var, Type)])                extends Type
case class UnionType(elements: List[(Var, Type)])                 extends Type
case class AppliedType(base: TypeName, typeArgs: List[Type])      extends Type
case object Top                                                   extends Type
case object Bot                                                   extends Type
