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
        case OpPlus()  => 10
        case OpMinus() => 10
        case OpRem()   => 20
        case OpMul()   => 20
        case OpDiv()   => 20
    }

    def parseExpression(minPrec: Int): Expression = {
        var left         = parseFactor()
        var nextTokenOpt = tokenizer.peek()
        while nextTokenOpt.isDefined &&
            Set[Token](OpPlus(), OpMinus(), OpMul(), OpDiv(), OpRem()).contains(nextTokenOpt.get) &&
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
