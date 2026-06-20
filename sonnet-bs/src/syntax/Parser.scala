package syntax

import app.CompilerError
import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer
import pprint.pprintln
import scala.util.boundary

class ParserError(detail: String) extends CompilerError("Parser", detail)

object Parser {
    def fromString(s: String): Parser = Parser(Tokenizer(s))
}

class Parser(tokenizer: Tokenizer) {

    def parse(): Program = {
        val items = new ListBuffer[TopLevelItem]()
        while (tokenizer.peek().isDefined) {
            if (tokenizer.peek() == Some(KwImport())) {
                items += parseImportStmt()
            } else {
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
            }
            expect(OpSemicolon())
        }
        Program(items.toList)
    }

    def parseImportStmt(): Import = {
        expect(KwImport())
        val pathBuilder = new StringBuilder()

        boundary {
            while (tokenizer.peek().isDefined) {
                tokenizer.peek() match {
                    case Some(OpSemicolon()) =>
                        boundary.break()
                    case Some(TokIdent(v)) =>
                        pathBuilder.append(v)
                        tokenizer.consume()
                    case Some(OpDot()) => {
                        tokenizer.consume()
                        tokenizer.peek() match {
                            case Some(OpDot()) => {
                                pathBuilder.append("..")
                                tokenizer.consume()
                            }
                            case _ => {

                                pathBuilder.append(".")
                            }
                        }
                    }
                    case Some(OpDiv()) =>
                        pathBuilder.append("/")
                        tokenizer.consume()
                    case Some(other) =>
                        throw ParserError(s"Unexpected token inside import path: $other")
                    case None =>
                        throw ParserError("Reached End-Of-File while parsing.")
                }
            }
        }

        val resolvedPath = pathBuilder.toString()
        if (resolvedPath.isEmpty) {
            throw ParserError("Import statement is missing a file path.")
        }

        Import(resolvedPath)
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

        val params     = new ListBuffer[String]()
        val paramTypes = new ListBuffer[Type]()
        if (tokenizer.peek() != Some(RParen())) {
            params += expect[TokIdent].value
            expect(OpColon())
            paramTypes += parseType()
            while (tokenizer.peek() == Some(OpComma())) {
                tokenizer.consume()
                params += expect[TokIdent].value
                expect(OpColon())
                paramTypes += parseType()
            }
        }

        expect(RParen())
        val returnType = parseType()
        val expr       = parseExpression(0)
        FunctionDef(name, params.toList, FunType(paramTypes.toList, returnType), expr, linkage)
    }

    def parseGlobalVarDeclaration(linkage: Linkage): GlobalVarDeclaration = {
        expect(KwVar())
        val name = expect[TokIdent].value
        expect(OpColon())
        val typ = parseType()
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
        GlobalVarDeclaration(name, typ, init, linkage)
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
        val typ = parseType()

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
        VarDeclaration(name, typ, init)
    }

    def parseType(): Type =
        tokenizer.next() match {
            case Some(KwI8())  => I8()
            case Some(KwI16()) => I16()
            case Some(KwI32()) => I32()
            case Some(KwI64()) => I64()

            case Some(KwU8())  => U8()
            case Some(KwU16()) => U16()
            case Some(KwU32()) => U32()
            case Some(KwU64()) => U64()

            case Some(KwF16()) => F16()
            case Some(KwF32()) => F32()
            case Some(KwF64()) => F64()

            case Some(KwBool()) => Bool()

            case Some(LParen()) => {
                val params = new ListBuffer[Type]()

                if (tokenizer.peek() != Some(RParen())) {
                    params += parseType()

                    while (tokenizer.peek() == Some(OpComma())) {
                        tokenizer.consume()
                        params += parseType()
                    }
                }

                expect(RParen())
                expect(OpArrow())

                val returnType = parseType()

                FunType(params.toList, returnType)
            }
            case t => throw ParserError(s"No type can be constructed from token $t.")
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
        case OpAs()                                                                                                                                                                                                                  => 85
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
            if (opToken == OpAs()) {
                val targetType = parseType()
                left = Cast(left, targetType)
            } else if (cPrec == 10) { // it is an assignment
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

    def parseFactor(directNegation: Boolean = false): Expression = {
        tokenizer.next() match {
            case Some(LBrace()) => parseBlock()

            case Some(TokTrue())  => TrueExpr()
            case Some(TokFalse()) => FalseExpr()

            case Some(TokU8Lit(value))  => Constant(Const.U8Lit(value))
            case Some(TokU16Lit(value)) => Constant(Const.U16Lit(value))
            case Some(TokU32Lit(value)) => Constant(Const.U32Lit(value))
            case Some(TokU64Lit(value)) => Constant(Const.U64Lit(value))

            case Some(TokF16Lit(value)) => Constant(Const.F16Lit(value))
            case Some(TokF32Lit(value)) => Constant(Const.F32Lit(value))
            case Some(TokF64Lit(value)) => Constant(Const.F64Lit(value))

            case Some(TokI8Lit(value)) =>
                if (!directNegation) {
                    val max = BigInt(2).pow(7) - 1
                    if (value > max) throw ParserError(s"8-bit integer literal '$value' exceeds its maximum allowed positive bound.")
                }
                Constant(Const.I8Lit(value))

            case Some(TokI16Lit(value)) =>
                if (!directNegation) {
                    val max = BigInt(2).pow(15) - 1
                    if (value > max) throw ParserError(s"16-bit integer literal '$value' exceeds its maximum allowed positive bound.")
                }
                Constant(Const.I16Lit(value))

            case Some(TokI32Lit(value)) =>
                if (!directNegation) {
                    val max = BigInt(2).pow(31) - 1
                    if (value > max) throw ParserError(s"32-bit integer literal '$value' exceeds its maximum allowed positive bound.")
                }
                Constant(Const.I32Lit(value))

            case Some(TokI64Lit(value)) =>
                if (!directNegation) {
                    val max = BigInt(2).pow(63) - 1
                    if (value > max) throw ParserError(s"64-bit integer literal '$value' exceeds its maximum allowed positive bound.")
                }
                Constant(Const.I64Lit(value))
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
            case Some(OpMinus()) => {
                val fac = parseFactor(true)
                fac match {
                    case Constant(Const.I8Lit(v))  => Constant(Const.I8Lit(-v))
                    case Constant(Const.I16Lit(v)) => Constant(Const.I16Lit(-v))
                    case Constant(Const.I32Lit(v)) => Constant(Const.I32Lit(-v))
                    case Constant(Const.I64Lit(v)) => Constant(Const.I64Lit(-v))
                    case _                         => Unary(UnaryOp.Negate, fac)
                }
            }
            case Some(OpNot()) => Unary(UnaryOp.Not, parseFactor())
            case Some(LParen()) => {
                val in = parseExpression(0)
                expect(RParen())
                in
            }
            case other => throw ParserError(s"Malformed Factor. Found unexpected token: $other")
        }
    }

    def isBinaryOperator(t: Token): Boolean = t match {
        case OpAs() | OpPlus() | OpMinus() | OpMul() | OpDiv() | OpRem() | OpAnd() | OpOr() | OpEqual() | OpNotEqual() | OpGreaterThan() | OpLessThan() | OpLessOrEqual() | OpGreaterOrEqual() | OpAssign() | OpBitAnd() | OpBitOr() | OpBitXor() | OpLShift() | OpRShift() | OpAddAssign() | OpSubAssign() | OpMulAssign() | OpDivAssign() | OpRemAssign() | OpAndAssign() | OpOrAssign() | OpBitAndAssign() | OpBitOrAssign() | OpBitXorAssign() | OpLShiftAssign() | OpRShiftAssign() => true
        case _                                                                                                                                                                                                                                                                                                                                                                                                                                                                           => false
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
