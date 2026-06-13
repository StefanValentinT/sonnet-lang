package syntax

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import app.CompilerError
import pprint.pprintln
import scala.util.boundary

class Token

case class KwPrivate()  extends Token
case class KwFor()      extends Token
case class KwVal()      extends Token
case class KwVar()      extends Token
case class KwReturn()   extends Token
case class KwIf()       extends Token
case class KwThen()     extends Token
case class KwElse()     extends Token
case class KwFun()      extends Token
case class KwWhile()    extends Token
case class KwBreak()    extends Token
case class KwContinue() extends Token
case class KwDo()       extends Token

case class KwI8()  extends Token
case class KwI16() extends Token
case class KwI32() extends Token
case class KwI64() extends Token

case class LParen()   extends Token
case class RParen()   extends Token
case class LBracket() extends Token
case class RBracket() extends Token
case class LBrace()   extends Token
case class RBrace()   extends Token

case class OpArrow()          extends Token // ->
case class OpAssign()         extends Token // =
case class OpNot()            extends Token // !
case class OpEqual()          extends Token // ==
case class OpLessThan()       extends Token // <
case class OpGreaterThan()    extends Token // >
case class OpNotEqual()       extends Token // !=
case class OpGreaterOrEqual() extends Token // >=
case class OpLessOrEqual()    extends Token // <=
case class OpColon()          extends Token // :
case class OpComma()          extends Token // ,
case class OpSemicolon()      extends Token // ;
case class OpOr()             extends Token // |
case class OpAnd()            extends Token // &
case class OpBitAnd()         extends Token // &&
case class OpBitOr()          extends Token // ||
case class OpBitXor()         extends Token // ^
case class OpLShift()         extends Token // <<
case class OpRShift()         extends Token // >>
case class OpTilde()          extends Token // ~
case class OpAs()             extends Token // as
case class OpDot()            extends Token // .
case class OpDoubleColon()    extends Token // ::
case class OpRef()            extends Token // .*
case class OpDeref()          extends Token // .!
case class OpPlus()           extends Token // +
case class OpMinus()          extends Token // -
case class OpMul()            extends Token // *
case class OpRem()            extends Token // %
case class OpDiv()            extends Token // /
case class OpLt()             extends Token // <

case class OpAddAssign()    extends Token // +=
case class OpSubAssign()    extends Token // -=
case class OpMulAssign()    extends Token // *=
case class OpDivAssign()    extends Token // /=
case class OpRemAssign()    extends Token // %=
case class OpAndAssign()    extends Token // &=
case class OpOrAssign()     extends Token // |=
case class OpBitAndAssign() extends Token // &&=
case class OpBitOrAssign()  extends Token // ||=
case class OpBitXorAssign() extends Token // ^=
case class OpLShiftAssign() extends Token // <<=
case class OpRShiftAssign() extends Token // >>=

case class TokIdent(value: String)     extends Token
case class TokI8Lit(value: BigInt)     extends Token
case class TokI16Lit(value: BigInt)    extends Token
case class TokI32Lit(value: BigInt)    extends Token
case class TokI64Lit(value: BigInt)    extends Token
case class TokStringLit(value: String) extends Token

class TokenizerError(detail: String) extends CompilerError("Tokenizer", detail)

sealed trait MatchPattern
case class Lit(str: String)     extends MatchPattern
case class Word(str: String)    extends MatchPattern
case class RegexPat(re: String) extends MatchPattern

class Tokenizer(input: String) {
    private val len = input.length

    private var idx                             = 0
    private var precomputedToken: Option[Token] = None

    private val rawPatterns: List[(MatchPattern, String => Token)] = List(
      (Word("private"), _ => KwPrivate()),
      (Word("for"), _ => KwFor()),
      (Word("val"), _ => KwVal()),
      (Word("var"), _ => KwVar()),
      (Word("return"), _ => KwReturn()),
      (Word("if"), _ => KwIf()),
      (Word("then"), _ => KwThen()),
      (Word("else"), _ => KwElse()),
      (Word("fun"), _ => KwFun()),
      (Word("while"), _ => KwWhile()),
      (Word("break"), _ => KwBreak()),
      (Word("continue"), _ => KwContinue()),
      (Word("do"), _ => KwDo()),
      (Word("i8"), _ => KwI8()),
      (Word("i16"), _ => KwI16()),
      (Word("i32"), _ => KwI32()),
      (Word("i64"), _ => KwI64()),
      (Word("int"), _ => KwI32()),
      (Word("long"), _ => KwI64()),
      (Word("as"), _ => OpAs()),
      (Lit("<<="), _ => OpLShiftAssign()),
      (Lit(">>="), _ => OpRShiftAssign()),
      (Lit("&&="), _ => OpBitAndAssign()),
      (Lit("||="), _ => OpBitOrAssign()),
      (Lit("+="), _ => OpAddAssign()),
      (Lit("-="), _ => OpSubAssign()),
      (Lit("*="), _ => OpMulAssign()),
      (Lit("/="), _ => OpDivAssign()),
      (Lit("%="), _ => OpRemAssign()),
      (Lit("&="), _ => OpAndAssign()),
      (Lit("|="), _ => OpOrAssign()),
      (Lit("^="), _ => OpBitXorAssign()),
      (Lit("->"), _ => OpArrow()),
      (Lit(".*"), _ => OpRef()),
      (Lit(".!"), _ => OpDeref()),
      (Lit("::"), _ => OpDoubleColon()),
      (Lit(">="), _ => OpGreaterOrEqual()),
      (Lit("<="), _ => OpLessOrEqual()),
      (Lit("!="), _ => OpNotEqual()),
      (Lit("=="), _ => OpEqual()),
      (Lit("<<"), _ => OpLShift()),
      (Lit(">>"), _ => OpRShift()),
      (Lit("&&"), _ => OpBitAnd()),
      (Lit("||"), _ => OpBitOr()),
      (Lit("^"), _ => OpBitXor()),
      (Lit("<"), _ => OpLessThan()),
      (Lit(">"), _ => OpGreaterThan()),
      (Lit("="), _ => OpAssign()),
      (Lit(":"), _ => OpColon()),
      (Lit(","), _ => OpComma()),
      (Lit(";"), _ => OpSemicolon()),
      (Lit("|"), _ => OpOr()),
      (Lit("&"), _ => OpAnd()),
      (Lit("~"), _ => OpTilde()),
      (Lit("."), _ => OpDot()),
      (Lit("+"), _ => OpPlus()),
      (Lit("-"), _ => OpMinus()),
      (Lit("*"), _ => OpMul()),
      (Lit("%"), _ => OpRem()),
      (Lit("/"), _ => OpDiv()),
      (Lit("("), _ => LParen()),
      (Lit(")"), _ => RParen()),
      (Lit("["), _ => LBracket()),
      (Lit("]"), _ => RBracket()),
      (Lit("{"), _ => LBrace()),
      (Lit("}"), _ => RBrace()),
      (
        RegexPat("\\d+i8"),
        s => {
            val numStr = s.stripSuffix("i8")
            val bigNum = BigInt(numStr)
            if (bigNum > BigInt(2).pow(7)) {
                throw TokenizerError(s"8-bit integer literal '$s' exceeds allowable i8 bounds.")
            }
            TokI8Lit(bigNum)
        }
      ),
      (
        RegexPat("\\d+i16"),
        s => {
            val numStr = s.stripSuffix("i16")
            val bigNum = BigInt(numStr)
            if (bigNum > BigInt(2).pow(15)) {
                throw TokenizerError(s"16-bit integer literal '$s' exceeds allowable i16 bounds.")
            }
            TokI16Lit(bigNum)
        }
      ),
      (
        RegexPat("\\d+i32"),
        s => {
            val numStr = s.stripSuffix("i32")
            val bigNum = BigInt(numStr)
            if (bigNum > BigInt(2).pow(31)) {
                throw TokenizerError(s"32-bit integer literal '$s' exceeds allowable i32 bounds.")
            }
            TokI32Lit(bigNum)
        }
      ),
      (
        RegexPat("\\d+i64"),
        s => {
            val numStr = s.stripSuffix("i64")
            val bigNum = BigInt(numStr)
            if (bigNum > BigInt(2).pow(63)) {
                throw TokenizerError(s"64-bit integer literal '$s' exceeds allowable i64 bounds.")
            }
            TokI64Lit(bigNum)
        }
      ),
      (
        RegexPat("\\d+"),
        s => {
            val bigNum = BigInt(s)
            if (bigNum > BigInt(2).pow(31)) {
                throw TokenizerError(s"Implicit 32-bit integer literal '$s' exceeds allowable i32 bounds.")
            }
            TokI32Lit(bigNum)
        }
      ),
      (RegexPat("\"[^\"]*\""), s => TokStringLit(s.substring(1, s.length - 1))),
      (RegexPat("[a-zA-Z_]\\w*"), s => TokIdent(s))
    )
    val tokenPatterns: List[(Regex, String => Token)] = rawPatterns.map {
        case (Lit(str), builder) =>
            (Regex.quote(str).r, builder)
        case (Word(str), builder) =>
            (s"\\b${Regex.quote(str)}\\b".r, builder)
        case (RegexPat(re), builder) =>
            (re.r, builder)
    }

    def peek(): Option[Token] = {
        if (precomputedToken.isDefined) {
            return precomputedToken
        }
        while (idx < len) {
            val currentSub = input.substring(idx)

            if (currentSub.head.isWhitespace) {
                idx += 1
            } else if (currentSub.startsWith("#")) {
                val eol = currentSub.indexOf('\n')
                idx += (if (eol == -1) currentSub.length else eol)
            } else {
                tokenPatterns.collectFirst {
                    case (regex, tokenBuilder) if regex.findPrefixOf(currentSub).isDefined =>
                        val lexeme = regex.findPrefixOf(currentSub).get
                        idx += lexeme.length
                        val tok = tokenBuilder(lexeme)
                        precomputedToken = Some(tok)
                        tok
                } match {
                    case Some(matchedToken) => return Some(matchedToken)
                    case None               => throw TokenizerError(s"Unknown character '${currentSub.head}' at index $idx")
                }
            }
        }
        None
    }

    def next(): Option[Token] = {
        val tok = peek()
        precomputedToken = None
        tok
    }

    def consume(): Unit = {
        next(); ()
    }
}
