package syntax

import collection.mutable.Map
import tac.Tac

case class Program(items: List[TopLevelItem])

abstract sealed class TopLevelItem
case class Import(path: String)                                                                     extends TopLevelItem
case class Declaration(name: String, typ: Option[Type], init: Option[Expression], linkage: Linkage) extends TopLevelItem

enum Linkage {
    case Public, Private
}

abstract sealed class Statement
case class VarDeclaration(name: String, typ: Option[Type], init: Option[Expression]) extends Statement
case class ExpressionStmt(exp: Expression)                                           extends Statement

trait Formal

abstract sealed class Expression
case class Constant(const: Const)                                                       extends Expression
case class ArrayLit(values: List[Expression], typ: ArrayType)                           extends Expression
case class Function(params: List[Formal], retType: Option[Type], body: Expression)      extends Expression
case class Var(name: String)                                                            extends Expression with Formal
case class Ref(exp: Expression)                                                         extends Expression
case class Deref(exp: Expression)                                                       extends Expression
case class Unary(op: UnaryOp, exp: Expression)                                          extends Expression
case class Typed(exp: Expression, typ: Type)                                            extends Expression with Formal
case class Cast(exp: Expression, targetType: Type)                                      extends Expression
case class Binary(op: BinaryOp, exp1: Expression, exp2: Expression)                     extends Expression
case class Assignment(target: Expression, value: Expression)                            extends Expression
case class If(cond: Expression, thenBranch: Expression, elseBranch: Option[Expression]) extends Expression
case class Return(exp: Expression)                                                      extends Expression
case class Break(label: String)                                                         extends Expression
case class Continue(label: String)                                                      extends Expression
case class While(cond: Expression, body: Expression, label: String)                     extends Expression
case class TrueExpr()                                                                   extends Expression
case class FalseExpr()                                                                  extends Expression
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
case class F16()                                  extends Type
case class F32()                                  extends Type
case class F64()                                  extends Type
case class Bool()                                 extends Type
case class ArrayType(elem: Type, size: BigInt)    extends Type
case class Pointer(ref: Type)                     extends Type
case class FunType(params: List[Type], ret: Type) extends Type
case class TypeVar(name: String) extends Type
case class Inter(left: Type, right: Type) extends Type
case class Quantified(over: String, typ: Type) extends Type

enum Const {
    case I8Lit(value: BigInt)
    case I16Lit(value: BigInt)
    case I32Lit(value: BigInt)
    case I64Lit(value: BigInt)

    case U8Lit(value: BigInt)
    case U16Lit(value: BigInt)
    case U32Lit(value: BigInt)
    case U64Lit(value: BigInt)

    case F16Lit(value: BigDecimal)
    case F32Lit(value: BigDecimal)
    case F64Lit(value: BigDecimal)

    def isZero: Boolean = this match {
        case I8Lit(n)  => n == 0
        case I16Lit(n) => n == 0
        case I32Lit(n) => n == 0
        case I64Lit(n) => n == 0
        case U8Lit(n)  => n == 0
        case U16Lit(n) => n == 0
        case U32Lit(n) => n == 0
        case U64Lit(n) => n == 0
        case F16Lit(f) => f == BigDecimal(0)
        case F32Lit(f) => f == BigDecimal(0)
        case F64Lit(f) => f == BigDecimal(0)
    }

    def getValueStr: String = this match {
        case I8Lit(n)  => n.toString
        case I16Lit(n) => n.toString
        case I32Lit(n) => n.toString
        case I64Lit(n) => n.toString
        case U8Lit(n)  => n.toString
        case U16Lit(n) => n.toString
        case U32Lit(n) => n.toString
        case U64Lit(n) => n.toString
        case F16Lit(f) => f.toString
        case F32Lit(f) => f.toString
        case F64Lit(f) => f.toString
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
            case Tac.I8() | Tac.U8()               => Byte1
            case Tac.I16() | Tac.U16() | Tac.F16() => Byte2
            case Tac.I32() | Tac.U32() | Tac.F32() => Byte4
            case Tac.I64() | Tac.U64() | Tac.F64() => Byte8
        }
}

def isNumericType(t: Type): Boolean = t match {
    case I8() | I16() | I32() | I64() | U8() | U16() | U32() | U64() | F16() | F32() | F64() => true
    case _                                                                                   => false
}

def isIntegerType(t: Type): Boolean = t match {
    case I8() | I16() | I32() | I64() | U8() | U16() | U32() | U64() => true
    case _                                                           => false
}
