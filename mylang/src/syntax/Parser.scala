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
        var defs = ListBuffer[Definition]()
        while (tokenizer.peek() != None) do {
            if tokenizer.peek() == Some(KwDef()) then defs.addOne(parseDefinition())
            tokenizer.next()
        }
        Program(
          items = defs.toList
        )
    }

    def parseDefinition(): Definition = {
        expect(KwDef())
        val name = expect[TokLowerIdent].value
        expect(LParen())
        expect(RParen())
        expect(OpEq())
        val value = Function(List(), parseExpression())
        Definition(name = Var(name), value)
    }

    def parseExpression(): Expression = {
        parseAdditive()
    }

    private def parseAdditive(): Expression = {
        var expr = parseMultiplicative()

        while (tokenizer.peek() == Some(OpPlus()) || tokenizer.peek() == Some(OpMinus())) do {
            val opToken = tokenizer.next().get
            val opSymbol = opToken match {
                case OpPlus()  => "+"
                case OpMinus() => "-"
                case _         => ""
            }
            val right = parseMultiplicative()
            expr = Application(Var(opSymbol), List(expr, right))
        }
        expr
    }

    private def parseMultiplicative(): Expression = {
        var expr = parsePrimary()

        while (tokenizer.peek() == Some(OpMul()) || tokenizer.peek() == Some(OpDiv())) do {
            val opToken = tokenizer.next().get
            val opSymbol = opToken match {
                case OpMul() => "*"
                case OpDiv() => "/"
                case _       => ""
            }
            val right = parsePrimary()
            expr = Application(Var(opSymbol), List(expr, right))
        }
        expr
    }

    private def parsePrimary(): Expression = {
        tokenizer.peek() match {
            case Some(TokIntLit(value, typ)) =>
                tokenizer.next()
                IntLit(value, PrimitiveType(typ))

            case Some(TokLowerIdent(name)) =>
                tokenizer.next() 
                Var(name)

            case Some(LParen()) =>
                tokenizer.next()
                val expr = parseExpression()
                expect(RParen())
                expr

            case Some(other) =>
                throw ParserError(s"Unexpected token in expression context: $other")

            case None =>
                throw ParserError("Unexpected end of file while parsing expression")
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
