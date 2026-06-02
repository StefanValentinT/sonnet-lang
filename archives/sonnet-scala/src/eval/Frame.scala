package eval

import syntax.Node
import scala.collection.mutable

class EvaluationError(message: String) extends RuntimeException(message)

enum Modifiability {
    case Mutable
    case Immutable
}
class Variable(val name: String, val mod: Modifiability)

class Frame(val parent: Option[Frame]) {

    def this(parentFrame: Frame) = {
        this(Some(parentFrame))
    }

    def this() = {
        this(None)
    }

    private val bindings: mutable.Map[Variable, Node] = mutable.Map.empty[Variable, Node]

    def define(name: String, value: Node, mutable: Boolean): Unit = {
        if (bindings.keys.exists(_.name == name)) {
            throw new EvaluationError(s"Identifier '$name' is already bound in this scope.")
        }
        val mod = if mutable then Modifiability.Mutable else Modifiability.Immutable
        bindings.put(Variable(name, mod), value)
    }

    def assign(name: String, value: Node): Unit = {
        bindings.keys.find(_.name == name) match {
            case Some(v) =>
                if (v.mod == Modifiability.Immutable) {
                    throw new EvaluationError(s"Cannot reassign immutable variable: $name")
                }
                bindings.put(v, value)
            case None =>
                parent match {
                    case Some(p) => p.assign(name, value)
                    case None    => throw new EvaluationError(s"Unbound variable: $name")
                }
        }
    }

    def lookup(name: String): Option[Node] = {
        bindings.find { case (v, _) => v.name == name } match {
            case Some((_, value)) => Some(value)
            case None =>
                parent match {
                    case Some(p) => p.lookup(name)
                    case None    => None
                }
        }
    }

    def contains(name: String): Boolean = {
        bindings.keys.exists(_.name == name)
    }

    def toStringBuilder(sb: StringBuilder): StringBuilder = {
        bindings.foreach { case (v, value) =>
            sb.append(s"${v.name} (${v.mod}) : $value\n")
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
