package syntax

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import app.CompilerError
import pprint.pprintln

class Token

case class KwType()     extends Token
case class KwClass()    extends Token
case class KwObject()   extends Token
case class KwInstance() extends Token
case class KwFor()      extends Token
case class KwDef()      extends Token
case class KwVal()      extends Token
case class KwVar()      extends Token
case class KwIf()       extends Token
case class KwThen()     extends Token
case class KwElse()     extends Token
case class KwCase()     extends Token
case class KwOf()       extends Token
case class KwInst()     extends Token

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
case class OpPipe()        extends Token // |
case class OpAmp()         extends Token // &
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

case class TokUpperIdent(value: String)           extends Token
case class TokLowerIdent(value: String)           extends Token
case class TokIntLit(value: Int, intType: String) extends Token
case class TokStringLit(value: String)            extends Token

class TokenizerError(detail: String) extends CompilerError("Tokenizer", detail)

class Tokenizer(input: String) {
    private val len = input.length

    private var idx                             = 0
    private var precomputedToken: Option[Token] = None

    val tokenPatterns: List[(Regex, String => Token)] = List(
      ("\\btype\\b".r, _ => KwType()),
      ("\\bclass\\b".r, _ => KwClass()),
      ("\\bobject\\b".r, _ => KwObject()),
      ("\\binstance\\b".r, _ => KwInstance()),
      ("\\bfor\\b".r, _ => KwFor()),
      ("\\bdef\\b".r, _ => KwDef()),
      ("\\bval\\b".r, _ => KwVal()),
      ("\\bvar\\b".r, _ => KwVar()),
      ("\\bif\\b".r, _ => KwIf()),
      ("\\bthen\\b".r, _ => KwThen()),
      ("\\belse\\b".r, _ => KwElse()),
      ("\\bcase\\b".r, _ => KwCase()),
      ("\\bof\\b".r, _ => KwOf()),
      ("\\binst\\b".r, _ => KwInst()),
      ("->".r, _ => OpArrow()),
      ("\\.\\*".r, _ => OpRef()),
      ("\\.\\!".r, _ => OpDeref()),
      ("::".r, _ => OpDoubleColon()),
      ("==".r, _ => OpDoubleEq()),
      ("=".r, _ => OpEq()),
      (":".r, _ => OpColon()),
      (",".r, _ => OpComma()),
      ("\\|".r, _ => OpPipe()),
      ("&".r, _ => OpAmp()),
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
        "\\d+[a-zA-Z]\\w*".r,
        s => {
            val num = s.takeWhile(_.isDigit).toInt
            val typ = s.dropWhile(_.isDigit)
            TokIntLit(num, typ)
        }
      ),
      (
        "\\d+".r,
        s => TokIntLit(s.toInt, "I32")
      ),
      ("\"[^\"]*\"".r, s => TokStringLit(s.substring(1, s.length - 1))),
      ("\\b[A-Z]\\w*\\b".r, s => TokUpperIdent(s)),
      ("\\b[a-z_]\\w*\\b".r, s => TokLowerIdent(s))
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
        pprintln(tok)
        tok
    }
}
