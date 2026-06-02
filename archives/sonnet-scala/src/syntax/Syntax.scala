package syntax

sealed trait Node {
    override def toString: String = {
        this match {
            case Node.IdentNode(name)   => name
            case Node.StringNode(value) => s"\"$value\""
            case Node.IntNode(value)    => value.toString
            case Node.DoubleNode(value) => value.toString
            case Node.ListNode(elements) => {
                elements.map(_.toString).mkString("(", " ", ")")
            }
        }
    }
}

object Node {
    case class IdentNode(name: String)        extends Node
    case class StringNode(value: String)      extends Node
    case class IntNode(value: Int)            extends Node
    case class DoubleNode(value: Double)      extends Node
    case class ListNode(elements: List[Node]) extends Node
}
