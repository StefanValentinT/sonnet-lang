package syntax

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import app.CompilerError
import pprint.pprintln

class Token

case class KwFor()    extends Token
case class KwVal()    extends Token
case class KwVar()    extends Token
case class KwReturn() extends Token
case class KwIf()     extends Token
case class KwThen()   extends Token
case class KwElse()   extends Token
case class KwVoid()   extends Token
case class KwInt()    extends Token

case class LParen()   extends Token
case class RParen()   extends Token
case class LBracket() extends Token
case class RBracket() extends Token
case class LBrace()   extends Token
case class RBrace()   extends Token

case class OpArrow()       extends Token // ->
case class OpEq()          extends Token // =
case class OpColon()       extends Token // :
case class OpComma()       extends Token // ,
case class OpSemicolon()   extends Token // ,
case class OpPipe()        extends Token // |
case class OpAmp()         extends Token // &
case class OpTilde()       extends Token // ~
case class OpDot()         extends Token // .
case class OpDoubleColon() extends Token // ::
case class OpRef()         extends Token // .*
case class OpDeref()       extends Token // .!
case class OpPlus()        extends Token // +
case class OpMinus()       extends Token // -
case class OpMul()         extends Token // *
case class OpDiv()         extends Token // /
case class OpLt()          extends Token // <
case class OpDoubleEq()    extends Token // ==

case class TokIdent(value: String)     extends Token
case class TokIntLit(value: Int)       extends Token
case class TokStringLit(value: String) extends Token

class TokenizerError(detail: String) extends CompilerError("Tokenizer", detail)

class Tokenizer(input: String) {
    private val len = input.length

    private var idx                             = 0
    private var precomputedToken: Option[Token] = None

    val tokenPatterns: List[(Regex, String => Token)] = List(
      ("\\bfor\\b".r, _ => KwFor()),
      ("\\bval\\b".r, _ => KwVal()),
      ("\\bvar\\b".r, _ => KwVar()),
      ("\\breturn\\b".r, _ => KwReturn()),
      ("\\bif\\b".r, _ => KwIf()),
      ("\\bthen\\b".r, _ => KwThen()),
      ("\\belse\\b".r, _ => KwElse()),
      ("\\bvoid\\b".r, _ => KwVoid()),
      ("\\bint\\b".r, _ => KwInt()),
      ("->".r, _ => OpArrow()),
      ("\\.\\*".r, _ => OpRef()),
      ("\\.\\!".r, _ => OpDeref()),
      ("::".r, _ => OpDoubleColon()),
      ("==".r, _ => OpDoubleEq()),
      ("=".r, _ => OpEq()),
      (":".r, _ => OpColon()),
      (",".r, _ => OpComma()),
      (";".r, _ => OpSemicolon()),
      ("\\|".r, _ => OpPipe()),
      ("&".r, _ => OpAmp()),
      ("~".r, _ => OpTilde()),
      ("\\.".r, _ => OpDot()),
      ("\\+".r, _ => OpPlus()),
      ("\\-".r, _ => OpMinus()),
      ("\\*".r, _ => OpMul()),
      ("/".r, _ => OpDiv()),
      ("<".r, _ => OpLt()),
      ("\\(".r, _ => LParen()),
      ("\\)".r, _ => RParen()),
      ("\\[".r, _ => LBracket()),
      ("\\]".r, _ => RBracket()),
      ("\\{".r, _ => LBrace()),
      ("\\}".r, _ => RBrace()),
      (
        "\\d+".r,
        s => TokIntLit(s.toInt)
      ),
      ("\"[^\"]*\"".r, s => TokStringLit(s.substring(1, s.length - 1))),
      ("\\b[a-zA-Z_]\\w*\\b".r, s => TokIdent(s))
    )

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
                for ((regex, tokenBuilder) <- tokenPatterns) {
                    regex.findPrefixOf(currentSub) match {
                        case Some(lexeme) =>
                            idx += lexeme.length
                            val tok = Some(tokenBuilder(lexeme))
                            precomputedToken = tok
                            return tok
                        case None => ()
                    }
                }
                throw TokenizerError(s"Unknown character '${currentSub.head}' at index $idx")
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
