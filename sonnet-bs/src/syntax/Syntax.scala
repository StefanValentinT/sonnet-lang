package syntax

import collection.mutable.Map
import tac.Tac

case class Program(items: List[TopLevelItem])

abstract sealed class TopLevelItem
case class Declaration(name: String, typ: Type)                                                              extends TopLevelItem
case class FunctionDef(name: String, params: List[String], typ: FunType, body: Expression, linkage: Linkage) extends TopLevelItem
case class GlobalVarDeclaration(name: String, typ: Type, init: Option[Expression], linkage: Linkage)         extends TopLevelItem

enum Linkage {
    case Public, Private
}

abstract sealed class Statement
case class VarDeclaration(name: String, typ: Type, init: Option[Expression]) extends Statement
case class ExpressionStmt(exp: Expression)                                   extends Statement

abstract sealed class Expression
case class Constant(const: Const)                                                       extends Expression
case class Var(name: String)                                                            extends Expression
case class Unary(op: UnaryOp, exp: Expression)                                          extends Expression
case class Cast(exp: Expression, targetType: Type)                                      extends Expression
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
case class I8()                                   extends Type
case class I16()                                  extends Type
case class I32()                                  extends Type
case class I64()                                  extends Type
case class U8()                                   extends Type
case class U16()                                  extends Type
case class U32()                                  extends Type
case class U64()                                  extends Type
case class FunType(params: List[Type], ret: Type) extends Type

enum Const {
    case I8Lit(value: BigInt)
    case I16Lit(value: BigInt)
    case I32Lit(value: BigInt)
    case I64Lit(value: BigInt)

    case U8Lit(value: BigInt)
    case U16Lit(value: BigInt)
    case U32Lit(value: BigInt)
    case U64Lit(value: BigInt)

    def getValue: BigInt = this match {
        case syntax.Const.I8Lit(n)  => n
        case syntax.Const.I16Lit(n) => n
        case syntax.Const.I32Lit(n) => n
        case syntax.Const.I64Lit(n) => n
        case syntax.Const.U8Lit(n)  => n
        case syntax.Const.U16Lit(n) => n
        case syntax.Const.U32Lit(n) => n
        case syntax.Const.U64Lit(n) => n
    }
}

enum UnaryOp {
    case Complement, Negate, Not
}

enum BinaryOp {
    case Add, Subtract, Multiply, Divide, Remainder,
        And, Or, Equal, NotEqual, LessThan, LessOrEqual, GreaterThan, GreaterOrEqual,
        BitAnd, BitOr, BitXor, LShift, RShift
}

enum Size {
    case Byte1, Byte2, Byte4, Byte8

    def bytes: Int = this match {
        case Byte1 => 1
        case Byte2 => 2
        case Byte4 => 4
        case Byte8 => 8
    }

    def bits: Int = this.bytes * 8
}

object Size {
    def fromTacType(t: Tac.Type): Size =
        t match {
            case Tac.I8()  => Byte1
            case Tac.I16() => Byte2
            case Tac.I32() => Byte4
            case Tac.I64() => Byte8

            case Tac.U8()  => Byte1
            case Tac.U16() => Byte2
            case Tac.U32() => Byte4
            case Tac.U64() => Byte8
        }
}
