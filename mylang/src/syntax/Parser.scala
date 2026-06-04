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
        val items = new ListBuffer[TopLevelItem]()
        while (tokenizer.peek().isDefined) {
            val linkage = tokenizer.peek() match {
                case Some(KwPrivate()) =>
                    tokenizer.consume()
                    Linkage.Private
                case _ =>
                    Linkage.Public
            }
            tokenizer.peek() match {
                case Some(KwFun()) =>
                    items += parseFunctionDef(linkage)
                case Some(KwVar()) =>
                    items += parseGlobalVarDeclaration(linkage)
                case Some(TokIdent(_)) =>
                    if (linkage == Linkage.Private) {
                        throw ParserError("Declarations can not have a linkage modifier.")
                    }
                    items += parseDeclaration()
                case Some(other) =>
                    throw ParserError(s"Expected function or top-level declaration, got: $other")
            }
            expect(OpSemicolon())
        }
        Program(items.toList)
    }

    def parseDeclaration(): Declaration = {
        val name = expect[TokIdent].value
        expect(OpColon())
        val typ = parseType()
        Declaration(name, typ)
    }

    def parseFunctionDef(linkage: Linkage): FunctionDef = {
        expect(KwFun())
        val name = expect[TokIdent].value
        expect(LParen())

        val params = new ListBuffer[String]()
        if (tokenizer.peek() != Some(RParen())) {
            params += expect[TokIdent].value
            expect(OpColon())
            expect(KwI32())
            while (tokenizer.peek() == Some(OpComma())) {
                tokenizer.consume()
                params += expect[TokIdent].value
                expect(OpColon())
                expect(KwI32())
            }
        }

        expect(RParen())
        expect(KwI32())
        val expr = parseExpression(0)
        FunctionDef(name, params.toList, expr, linkage)
    }

    def parseGlobalVarDeclaration(linkage: Linkage): GlobalVarDeclaration = {
        expect(KwVar())
        val name = expect[TokIdent].value
        expect(OpColon())
        expect(KwI32())
        val init = tokenizer.peek() match {
            case Some(OpAssign()) => {
                tokenizer.consume()
                val init = parseExpression(0)
                Some(init)
            }
            case _ => {
                None
            }
        }
        GlobalVarDeclaration(name, init, linkage)
    }

    def parseBlock(): Block = {
        val stmts                         = new ListBuffer[Statement]()
        var finalExpr: Option[Expression] = None

        while (tokenizer.peek() != Some(RBrace())) {
            val stmt = parseStatement()
            stmt match {
                case ExpressionStmt(expr) =>
                    if (tokenizer.peek() == Some(RBrace())) {
                        finalExpr = Some(expr)
                    } else {
                        expect(OpSemicolon())
                        stmts += stmt
                    }
                case nonExprStmt => {
                    stmts += nonExprStmt
                    expect(OpSemicolon())
                }
            }
        }
        expect(RBrace())
        Block(stmts.toList, finalExpr)
    }

    def parseStatement(): Statement = {
        tokenizer.peek() match {
            case Some(KwVar()) =>
                parseVarDeclaration()

            case Some(_) =>
                val expr = parseExpression(0)
                ExpressionStmt(expr)

            case None =>
                throw ParserError("Expected a statement but reached end of tokenstream.")
        }
    }

    def parseVarDeclaration(): VarDeclaration = {
        expect(KwVar())
        val name = expect[TokIdent].value
        expect(OpColon())
        expect(KwI32())

        val decl = tokenizer.peek() match {
            case Some(OpAssign()) => {
                tokenizer.consume()
                val init = parseExpression(0)
                VarDeclaration(name, Some(init))
            }
            case _ => {
                VarDeclaration(name, None)
            }
        }
        decl
    }

    def parseType(): Type =
        tokenizer.peek() match {
            case Some(KwI32()) => {
                tokenizer.consume()
                I32()
            }
            case Some(LParen()) => {
                tokenizer.consume()
                var argCount = 0
                if (tokenizer.peek() != Some(RParen())) {
                    expect(KwI32())
                    argCount += 1
                    while (tokenizer.peek() == Some(OpComma())) {
                        tokenizer.consume()
                        expect(KwI32())
                        argCount += 1
                    }
                }
                expect(RParen())
                expect(OpArrow())
                expect(KwI32())
                FunType(argCount)
            }
            case _ => throw ParserError("Invalid type.")
        }

    def precedence(t: Token): Int = t match {
        case OpAssign() | OpAddAssign() | OpSubAssign() | OpMulAssign() | OpDivAssign() | OpRemAssign() | OpAndAssign() | OpOrAssign() | OpBitAndAssign() | OpBitOrAssign() | OpBitXorAssign() | OpLShiftAssign() | OpRShiftAssign() => 10
        case OpOr()                                                                                                                                                                                                                  => 30
        case OpAnd()                                                                                                                                                                                                                 => 40
        case OpBitOr()                                                                                                                                                                                                               => 42
        case OpBitXor()                                                                                                                                                                                                              => 44
        case OpBitAnd()                                                                                                                                                                                                              => 46
        case OpEqual()                                                                                                                                                                                                               => 50
        case OpNotEqual()                                                                                                                                                                                                            => 50
        case OpLessThan()                                                                                                                                                                                                            => 60
        case OpGreaterThan()                                                                                                                                                                                                         => 60
        case OpLessOrEqual()                                                                                                                                                                                                         => 60
        case OpGreaterOrEqual()                                                                                                                                                                                                      => 60
        case OpLShift()                                                                                                                                                                                                              => 65
        case OpRShift()                                                                                                                                                                                                              => 65
        case OpPlus()                                                                                                                                                                                                                => 70
        case OpMinus()                                                                                                                                                                                                               => 70
        case OpRem()                                                                                                                                                                                                                 => 80
        case OpMul()                                                                                                                                                                                                                 => 80
        case OpDiv()                                                                                                                                                                                                                 => 80
    }

    def parseExpression(minPrec: Int): Expression = {
        var left = parseFactor()

        var nextTokenOpt = tokenizer.peek()
        while (
          nextTokenOpt.isDefined &&
          isBinaryOperator(nextTokenOpt.get) &&
          precedence(nextTokenOpt.get) >= minPrec
        ) {

            val opToken = tokenizer.next().get
            val cPrec   = precedence(opToken)

            if (cPrec == 10) { // it is an assignment
                // right-assoicative meaning a = b = c is possible
                val right = parseExpression(cPrec)

                if (opToken == OpAssign()) {
                    left = Assignment(left, right)
                } else {
                    val binaryOp = opToken match {
                        case OpAddAssign()    => BinaryOp.Add
                        case OpSubAssign()    => BinaryOp.Subtract
                        case OpMulAssign()    => BinaryOp.Multiply
                        case OpDivAssign()    => BinaryOp.Divide
                        case OpRemAssign()    => BinaryOp.Remainder
                        case OpAndAssign()    => BinaryOp.And
                        case OpOrAssign()     => BinaryOp.Or
                        case OpBitAndAssign() => BinaryOp.BitAnd
                        case OpBitOrAssign()  => BinaryOp.BitOr
                        case OpBitXorAssign() => BinaryOp.BitXor
                        case OpLShiftAssign() => BinaryOp.LShift
                        case OpRShiftAssign() => BinaryOp.RShift
                        case _                => throw ParserError("Unknown compound assignment operator.")
                    }
                    // x += y  =>  x = x + y
                    left = Assignment(left, Binary(binaryOp, left, right))
                }
            } else {
                val op = opToken match {
                    case OpPlus()           => BinaryOp.Add
                    case OpMinus()          => BinaryOp.Subtract
                    case OpMul()            => BinaryOp.Multiply
                    case OpDiv()            => BinaryOp.Divide
                    case OpRem()            => BinaryOp.Remainder
                    case OpOr()             => BinaryOp.Or
                    case OpAnd()            => BinaryOp.And
                    case OpEqual()          => BinaryOp.Equal
                    case OpNotEqual()       => BinaryOp.NotEqual
                    case OpLessThan()       => BinaryOp.LessThan
                    case OpGreaterThan()    => BinaryOp.GreaterThan
                    case OpLessOrEqual()    => BinaryOp.LessOrEqual
                    case OpGreaterOrEqual() => BinaryOp.GreaterOrEqual
                    case OpBitAnd()         => BinaryOp.BitAnd
                    case OpBitOr()          => BinaryOp.BitOr
                    case OpBitXor()         => BinaryOp.BitXor
                    case OpLShift()         => BinaryOp.LShift
                    case OpRShift()         => BinaryOp.RShift
                }
                val right = parseExpression(cPrec + 1)
                left = Binary(op, left, right)
            }

            nextTokenOpt = tokenizer.peek()
        }
        left
    }

    def parseFactor(): Expression = {
        tokenizer.next() match {
            case Some(LBrace())         => parseBlock()
            case Some(TokIntLit(value)) => Constant(value)
            case Some(KwIf()) => {
                val cond = parseExpression(0)
                expect(KwThen())
                val thenStmt = parseExpression(0)
                val elseBranch = tokenizer.peek() match {
                    case Some(KwElse()) => {
                        tokenizer.consume()
                        Some(parseExpression(0))
                    }
                    case _ => {
                        None
                    }
                }
                If(cond, thenStmt, elseBranch)
            }
            case Some(KwReturn()) => {
                val rv = parseExpression(0)
                Return(rv)
            }
            case Some(KwWhile()) => {
                val cond = parseExpression(0)
                expect(KwDo())
                val body = parseExpression(0)
                While(cond, body, "")
            }
            case Some(KwBreak()) =>
                Break("")

            case Some(KwContinue()) =>
                Continue("")
            case Some(TokIdent(value)) => {
                tokenizer.peek() match {
                    case Some(LParen()) => {
                        tokenizer.consume()
                        val args = new ListBuffer[Expression]()
                        if (tokenizer.peek() != Some(RParen())) {
                            args += parseExpression(0)

                            while (tokenizer.peek() == Some(OpComma())) {
                                tokenizer.consume()
                                args += parseExpression(0)
                            }
                        }

                        expect(RParen())
                        FunctionCall(value, args.toList)
                    }
                    case _ => {
                        Var(value)
                    }
                }
            }
            case Some(OpTilde()) => Unary(UnaryOp.Complement, parseFactor())
            case Some(OpMinus()) => Unary(UnaryOp.Negate, parseFactor())
            case Some(OpNot())   => Unary(UnaryOp.Not, parseFactor())
            case Some(LParen()) => {
                val in = parseExpression(0)
                expect(RParen())
                in
            }
            case other => throw ParserError(s"Malformed Factor. Found unexpected token: $other")
        }
    }

    def isBinaryOperator(t: Token): Boolean = t match {
        case OpPlus() | OpMinus() | OpMul() | OpDiv() | OpRem() | OpAnd() | OpOr() | OpEqual() | OpNotEqual() | OpGreaterThan() | OpLessThan() | OpLessOrEqual() | OpGreaterOrEqual() | OpAssign() | OpBitAnd() | OpBitOr() | OpBitXor() | OpLShift() | OpRShift() | OpAddAssign() | OpSubAssign() | OpMulAssign() | OpDivAssign() | OpRemAssign() | OpAndAssign() | OpOrAssign() | OpBitAndAssign() | OpBitOrAssign() | OpBitXorAssign() | OpLShiftAssign() | OpRShiftAssign() => true
        case _                                                                                                                                                                                                                                                                                                                                                                                                                                                                  => false
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
