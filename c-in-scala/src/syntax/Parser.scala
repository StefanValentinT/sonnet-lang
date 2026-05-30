package syntax

import app.CompilerError
import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer
import pprint.pprintln

class ParserError(detail: String) extends CompilerError("Parser", detail)

object Parser {
    def fromString(s: String): Parser = Parser(Tokenizer(s))
}

class Parser(tokenizer: Tokenizer) {

    def parse(): Program = {
        Program(parseFunctionDef())
    }

    def parseFunctionDef(): FunctionDef = {
        expect(KwInt())
        val name = expect[TokIdent].value
        expect(LParen())
        expect(KwVoid())
        expect(RParen())
        expect(LBrace())
        val stmt = parseStatement()
        expect(RBrace())
        FunctionDef(name, stmt)
    }

    def parseStatement(): Statement = {
        expect(KwReturn())
        val rv = parseExpression(0)
        expect(OpSemicolon())
        Return(rv)
    }

    def precedence(t: Token): Int = t match {
        case OpOr()             => 30
        case OpAnd()            => 40
        case OpEqual()          => 50
        case OpNotEqual()       => 50
        case OpLessThan()       => 60
        case OpGreaterThan()    => 60
        case OpLessOrEqual()    => 60
        case OpGreaterOrEqual() => 60
        case OpPlus()           => 70
        case OpMinus()          => 70
        case OpRem()            => 80
        case OpMul()            => 80
        case OpDiv()            => 80
    }

    def parseExpression(minPrec: Int): Expression = {
        var left         = parseFactor()
        var nextTokenOpt = tokenizer.peek()
        while nextTokenOpt.isDefined &&
            Set[Token](OpPlus(), OpMinus(), OpMul(), OpDiv(), OpRem(), OpAnd(), OpOr(), OpEqual(), OpNotEqual(), OpGreaterThan(), OpLessThan(), OpLessOrEqual(), OpGreaterOrEqual()).contains(nextTokenOpt.get) &&
            precedence(nextTokenOpt.get) >= minPrec
        do {

            val opToken = tokenizer.next().get
            val cPrec   = precedence(opToken)

            val op = opToken match {
                case OpPlus()  => BinaryOp.Add
                case OpMinus() => BinaryOp.Subtract
                case OpMul()   => BinaryOp.Multiply
                case OpDiv()   => BinaryOp.Divide
                case OpRem()   => BinaryOp.Remainder

                case OpOr()             => BinaryOp.Or
                case OpAnd()            => BinaryOp.And
                case OpEqual()          => BinaryOp.Equal
                case OpNotEqual()       => BinaryOp.NotEqual
                case OpLessThan()       => BinaryOp.LessThan
                case OpGreaterThan()    => BinaryOp.GreaterThan
                case OpLessOrEqual()    => BinaryOp.LessOrEqual
                case OpGreaterOrEqual() => BinaryOp.GreaterOrEqual
            }

            val right = parseExpression(cPrec + 1)
            left = Binary(op, left, right)

            nextTokenOpt = tokenizer.peek()
        }
        left
    }

    def parseFactor(): Expression = {
        tokenizer.next() match {
            case Some(TokIntLit(value)) => Constant(value)
            case Some(OpTilde())        => Unary(UnaryOp.Complement, parseFactor())
            case Some(OpMinus())        => Unary(UnaryOp.Negate, parseFactor())
            case Some(OpNot())          => Unary(UnaryOp.Not, parseFactor())
            case Some(LParen()) => {
                val in = parseExpression(0)
                expect(RParen())
                in
            }
            case _ => throw ParserError("Malformed Factor.")
        }
    }

    def expect[T <: Token](using tag: ClassTag[T]): T = {
        tokenizer.next() match {
            case Some(t: T) => t
            case Some(other) =>
                throw ParserError(s"Expected ${tag.runtimeClass.getSimpleName} but got $other")
            case None =>
                throw ParserError(s"Expected ${tag.runtimeClass.getSimpleName} but reached EOF")
        }
    }

    def expect(expectedValue: Token): Token = {
        tokenizer.next() match {
            case Some(t) if t == expectedValue => t
            case Some(other) =>
                throw ParserError(s"Expected $expectedValue but got $other")
            case None =>
                throw ParserError(s"Expected $expectedValue but got ")
        }
    }
}
