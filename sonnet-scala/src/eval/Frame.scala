package eval

import syntax.Node
import scala.collection.mutable

class EvaluationError(message: String) extends RuntimeException(message)

class Frame(val parent: Option[Frame]) {

    def this(parentFrame: Frame) = {
        this(Some(parentFrame))
    }

    def this() = {
        this(None)
    }

    private val bindings: mutable.Map[String, Node] = mutable.Map.empty[String, Node]

    def define(name: String, value: Node): Unit = {
        bindings.put(name, value)
    }

    def assign(name: String, value: Node): Unit = {
        if (bindings.contains(name)) {
            bindings.put(name, value)
        } else {
            parent match {
                case Some(p) => p.assign(name, value)
                case None    => throw new EvaluationError(s"Unbound variable: $name")
            }
        }
    }

    def lookup(name: String): Option[Node] = {
        bindings.get(name) match {
            case v @ Some(value) => v
            case None => {
                parent match {
                    case Some(p) => p.lookup(name)
                    case None    => None
                }
            }
        }
    }

    def contains(name: String): Boolean = {
        bindings.contains(name)
    }

    def toStringBuilder(sb: StringBuilder): StringBuilder = {
        bindings.foreach { case (key, value) =>
            sb.append(s"$key : $value\n")
        }
        parent.foreach { p =>
            p.toStringBuilder(sb)
        }
        sb
    }

    override def toString: String = {
        val sb = new StringBuilder("--- Frame ---\n")
        toStringBuilder(sb).append("\n-------------").toString()
    }
}
