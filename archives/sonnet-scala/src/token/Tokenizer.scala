package token

import app.CompilerError
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

sealed trait Token

object Token {
    case class QuoteToken()                  extends Token
    case class IntToken(value: Int)          extends Token
    case class DoubleToken(value: Double)    extends Token
    case class StringToken(value: String)    extends Token
    case class IdentifierToken(name: String) extends Token
    case class OpeningParen()                extends Token
    case class ClosingParen()                extends Token
}

class TokenizerError extends CompilerError("Tokenizer", null)

val idPattern: Regex = """[a-zA-Z0-9!@$%^&/\-_+=:<>.?*']+""".r

def tokenize(input: String): List[Token] = {
    val tokens = ListBuffer[Token]()
    val length = input.length
    var pos    = 0

    while (pos < length) {
        pos = skipWhitespaceAndComments(input, pos, length)
        if (pos < length) {
            val ch = input.charAt(pos)

            ch match {
                case '(' => {
                    tokens += Token.OpeningParen()
                    pos += 1
                }

                case ')' => {
                    tokens += Token.ClosingParen()
                    pos += 1
                }

                case '\'' => {
                    tokens += Token.QuoteToken()
                    pos += 1
                }

                case '"' => {
                    pos += 1
                    val start = pos
                    while (pos < length && input.charAt(pos) != '"') {
                        pos += 1
                    }

                    if (pos >= length) {
                        throw TokenizerError()
                    }

                    tokens += Token.StringToken(input.substring(start, pos))
                    pos += 1
                }

                case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    val start     = pos
                    var hasDot    = false
                    var breakLoop = false

                    while (pos < length && !breakLoop) {
                        val curr = input.charAt(pos)
                        if (curr == '.') {
                            if (hasDot) {
                                throw TokenizerError()
                            }
                            hasDot = true
                            pos += 1
                        } else if (isSequencingOmen(curr)) {
                            breakLoop = true
                        } else if (curr.isDigit) {
                            pos += 1
                        } else {
                            throw TokenizerError()
                        }
                    }

                    val lexeme = input.substring(start, pos)
                    if (hasDot) {
                        tokens += Token.DoubleToken(lexeme.toDouble)
                    } else {
                        tokens += Token.IntToken(lexeme.toInt)
                    }
                }

                case _ => {
                    val start     = pos
                    var breakLoop = false

                    while (pos < length && !breakLoop) {
                        val currCharStr = input.charAt(pos).toString
                        if (idPattern.matches(currCharStr)) {
                            pos += 1
                        } else {
                            breakLoop = true
                        }
                    }

                    val lexeme = input.substring(start, pos)
                    if (lexeme.isEmpty) {
                        throw TokenizerError()
                    }
                    tokens += Token.IdentifierToken(lexeme)
                }
            }
        }
    }
    tokens.toList
}

def isSequencingOmen(c: Char): Boolean = {
    c.isWhitespace || c == '(' || c == ')'
}

def skipWhitespaceAndComments(input: String, startPos: Int, length: Int): Int = {
    var pos       = startPos
    var breakLoop = false

    while (pos < length && !breakLoop) {
        val ch = input.charAt(pos)
        if (ch.isWhitespace) {
            pos += 1
        } else if (ch == '#') {
            while (pos < length && input.charAt(pos) != '\n') {
                pos += 1
            }
        } else {
            breakLoop = true
        }
    }
    pos
}
