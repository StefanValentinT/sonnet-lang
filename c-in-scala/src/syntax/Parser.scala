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
        val rv = parseExpression()
        expect(OpSemicolon())
        Return(rv)
    }

    def parseExpression(): Expression = {
        tokenizer.next() match {
            case Some(TokIntLit(value)) => Constant(value)
            case Some(OpTilde())        => Unary(UnaryOp.Complement, parseExpression())
            case Some(OpMinus())        => Unary(UnaryOp.Negate, parseExpression())
            case Some(LParen()) => {
                val in = parseExpression()
                expect(RParen())
                in
            }
            case _ => throw ParserError("Incomplete Expression.")
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
