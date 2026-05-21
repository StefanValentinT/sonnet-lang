package parser

import syntax.Node
import token.Token
import scala.collection.mutable.ListBuffer

class ParserError extends RuntimeException("Parser error encountered")

class Parser {

    def parse(tokenStream: List[Token]): Node = {
        if (tokenStream.isEmpty) {
            Node.ListNode(List.empty)
        } else {
            val (resultNode, remainingTokens) = parseNext(tokenStream)
            resultNode
        }
    }

    private def parseNext(tokens: List[Token]): (Node, List[Token]) = {
        tokens match {
            case Nil =>
                (Node.ListNode(List.empty), Nil)

            case Token.IntToken(value) :: tail =>
                (Node.IntNode(value), tail)

            case Token.DoubleToken(value) :: tail =>
                (Node.DoubleNode(value), tail)

            case Token.StringToken(value) :: tail =>
                (Node.StringNode(value), tail)

            case Token.IdentifierToken(name) :: tail =>
                (Node.IdentNode(name), tail)

            case Token.QuoteToken() :: tail => {
                val (quotedNode, rest) = parseNext(tail)
                val listNode           = Node.ListNode(List(Node.IdentNode("quote"), quotedNode))
                (listNode, rest)
            }

            case Token.OpeningParen() :: tail => {
                val listChildren  = ListBuffer[Node]()
                var currentTokens = tail

                while (currentTokens.nonEmpty && !currentTokens.head.isInstanceOf[Token.ClosingParen]) {
                    val (childNode, nextTokens) = parseNext(currentTokens)
                    listChildren += childNode
                    currentTokens = nextTokens
                }

                currentTokens match {
                    case Token.ClosingParen() :: rest =>
                        (Node.ListNode(listChildren.toList), rest)
                    case _ =>
                        throw new ParserError()
                }
            }

            case Token.ClosingParen() :: _ =>
                throw new ParserError()
        }
    }
}
