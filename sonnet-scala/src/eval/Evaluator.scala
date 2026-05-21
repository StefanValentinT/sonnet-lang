package eval

import syntax.Node
import app.CompilerContext

class Evaluator(ctx: CompilerContext) {

    private val unit: Node.ListNode = Node.ListNode(
      List(Node.IdentNode("quote"), Node.ListNode(List.empty))
    )

    def evaluate(node: Node): Node = eval(node, Frame())

    def eval(node: Node, frame: Frame): Node = {
        val result = node match {
            case n: Node.IntNode    => n
            case n: Node.DoubleNode => n
            case n: Node.StringNode => n

            case Node.IdentNode(name) => {
                frame.lookup(name) match {
                    case Some(value) => value
                    case None        => throw new EvaluationError(s"Undefined variable: $name")
                }
            }

            case list @ Node.ListNode(elements) => {
                if (elements.isEmpty) {
                    // () -> () symbolic evaluation
                    list
                } else if (isFunction(list)) {
                    list
                } else if (isQuote(list)) {
                    elements(1)
                } else if (isConditional(list)) {
                    evalConditional(elements(1), elements(2), elements(3), frame)
                } else if (isSet(list)) {
                    evalSet(elements(1), elements(2), frame)
                } else {
                    // Application
                    val first     = elements.head
                    val arguments = elements.tail

                    if (isBuiltIn(first)) {
                        evaluateBuiltIn(first.asInstanceOf[Node.IdentNode].name, arguments, frame)
                    } else {
                        val callee = eval(first, frame)
                        if (isFunction(callee)) {
                            evalFunctionApp(callee.asInstanceOf[Node.ListNode], arguments, frame)
                        } else {
                            throw new EvaluationError(s"First element of the list is not a function or builtin operator: $callee")
                        }
                    }
                }
            }
        }
        if (ctx.printStackFrames()) println(frame)
        result
    }

    private def isFunction(node: Node): Boolean = {
        node match {
            case Node.ListNode(elements) if elements.size == 3 => {
                elements.head match {
                    case Node.IdentNode("fun") => isFormals(elements(1))
                    case _                     => false
                }
            }
            case _ => false
        }
    }

    private def isQuote(node: Node): Boolean = {
        node match {
            case Node.ListNode(elements) if elements.size == 2 => {
                elements.head match {
                    case Node.IdentNode("quote") => true
                    case _                       => false
                }
            }
            case _ => false
        }
    }

    private def isConditional(node: Node): Boolean = {
        node match {
            case Node.ListNode(elements) if elements.size == 4 => {
                elements.head match {
                    case Node.IdentNode("if") => true
                    case _                    => false
                }
            }
            case _ => false
        }
    }

    private def isSet(node: Node): Boolean = {
        node match {
            case Node.ListNode(elements) if elements.size == 3 => {
                elements.head match {
                    case Node.IdentNode("set") => true
                    case _                     => false
                }
            }
            case _ => false
        }
    }

    private def evalConditional(cond: Node, thenBranch: Node, elseBranch: Node, frame: Frame): Node = {
        eval(cond, frame) match {
            case Node.IdentNode("true")  => eval(thenBranch, frame)
            case Node.IdentNode("false") => eval(elseBranch, frame)
            case _                       => throw new EvaluationError("Conditional condition must evaluate to either 'true or 'false.")
        }
    }

    private def evalSet(target: Node, expr: Node, frame: Frame): Node = {
        target match {
            case Node.IdentNode(name) => {
                val calculatedValue = eval(expr, frame)
                frame.assign(name, calculatedValue)
                unit
            }
            case _ => throw new EvaluationError("set assignment target must be a valid identifier symbol.")
        }
    }

    private def evalFunctionApp(callee: Node.ListNode, rawArgs: List[Node], frame: Frame): Node = {
        val args      = rawArgs.map(arg => eval(arg, frame))
        val bodyFrame = new Frame(frame)

        val paramNode = callee.elements(1)
        val paramsRaw = paramNode match {
            case Node.ListNode(pList) => pList
            case other                => List(other)
        }

        val params = paramsRaw.map {
            case Node.IdentNode(name) => name
            case _                    => throw new EvaluationError("Formals must be a list of identifiers or a singular identifier.")
        }

        if (params.distinct.size != params.size) {
            throw new EvaluationError("It is an error for a variable to appear more than once in formals.")
        }

        val body = callee.elements(2)

        if (params.size != args.size) {
            throw new EvaluationError(s"Arity mismatch: function expects ${params.size} arguments, but received ${args.size}")
        }

        params.zip(args).foreach { case (param, arg) =>
            bodyFrame.define(param, arg)
        }

        eval(body, bodyFrame)
    }

    private def isFormals(node: Node): Boolean = {
        node match {
            case Node.IdentNode(_)       => true
            case Node.ListNode(elements) => elements.forall(_.isInstanceOf[Node.IdentNode])
            case _                       => false
        }
    }

    private def isBuiltIn(node: Node): Boolean = {
        node match {
            case Node.IdentNode(name) => {
                name == "+" || name == "-" || name == "*" || name == "/" || name == "print"
            }
            case _ => false
        }
    }

    private def evaluateBuiltIn(op: String, rawArgs: List[Node], frame: Frame): Node = {
        val args = rawArgs.map(arg => eval(arg, frame))

        op match {
            case "print" => {
                val sb = new StringBuilder()
                args.foreach {
                    case Node.StringNode(value) => sb.append(value)
                    case other                  => sb.append(other.toString)
                }
                println(sb.toString())
                unit
            }

            case "+" | "-" | "*" | "/" => {
                val isDouble = args.head.isInstanceOf[Node.DoubleNode]

                if (isDouble) {
                    val doubleArgs = args.map {
                        case Node.DoubleNode(v) => v
                        case _                  => throw new EvaluationError(s"Mixed number types in builtin math operation $op.")
                    }

                    val calculated = op match {
                        case "+" => doubleArgs.sum
                        case "*" => doubleArgs.product
                        case "-" => doubleArgs.reduceLeft(_ - _)
                        case "/" => doubleArgs.reduceLeft(_ / _)
                        case _   => throw new EvaluationError("Undefined math operation.")
                    }
                    Node.DoubleNode(calculated)
                } else {
                    val intArgs = args.map {
                        case Node.IntNode(v) => v
                        case _               => throw new EvaluationError(s"Mixed number types in builtin math operation $op.")
                    }

                    val calculated = op match {
                        case "+" => intArgs.sum
                        case "*" => intArgs.product
                        case "-" => intArgs.reduceLeft(_ - _)
                        case "/" => intArgs.reduceLeft(_ / _)
                        case _   => throw new EvaluationError("Undefined math operation.")
                    }
                    Node.IntNode(calculated)
                }
            }
            case _ => throw new EvaluationError(s"Unknown built-in operator: $op")
        }
    }
}
