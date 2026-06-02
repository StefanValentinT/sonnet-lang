package eval

import syntax.Node
import app.CompilerContext
import syntax.Node.IdentNode
import pprint.pprintln

class Evaluator(ctx: CompilerContext) {

    private var expansionPhase = false

    private val unit: Node.ListNode = Node.ListNode(
      List(Node.IdentNode("quote"), Node.ListNode(List.empty))
    )

    def evaluate(node: Node): Node = {
        // expansionPhase = true;
        eval(node, Frame())
    }

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
                val splicedElements = elements.flatMap {
                    case Node.ListNode(Node.IdentNode("splice") :: target :: Nil) =>
                        eval(target, frame) match {
                            case Node.ListNode(innerElements) => innerElements
                            case other                        => throw EvaluationError("Splice can only act upon lists.")
                        }
                    case ordinaryNode =>
                        List(ordinaryNode)
                }

                splicedElements match {
                    case Nil => list // () -> () symbolic evaluation

                    case Node.IdentNode("quote") :: value :: Nil => value

                    case Node.IdentNode("if") :: cond :: thenBr :: elseBr :: Nil =>
                        evalConditional(cond, thenBr, elseBr, frame)

                    case Node.IdentNode("set") :: target :: expr :: Nil =>
                        evalSet(target, expr, frame)
                    case Node.IdentNode("val") :: target :: expr :: Nil =>
                        evalDefine(target, expr, frame, false)
                    case Node.IdentNode("var") :: target :: expr :: Nil =>
                        evalDefine(target, expr, frame, true)

                    case Node.IdentNode("fun") :: formals :: body if isFormals(formals, frame) => list

                    case Node.IdentNode("do") :: body => {
                        val bodyScope = Frame(frame)
                        body.foldLeft[Node](unit)((_, e) => eval(e, bodyScope))
                    }

                    case (n @ Node.IdentNode(op)) :: arguments if isBuiltIn(n) =>
                        evaluateBuiltIn(op, arguments, frame)

                    case first :: arguments => {
                        first match {
                            case IdentNode(name) if isSpecialForm(name) => throw new EvaluationError("Wrong usage of special form.")
                            case _                                      => ()
                        }

                        eval(first, frame) match {
                            case callee: Node.ListNode if callee.elements.headOption.contains(Node.IdentNode("fun")) =>
                                evalFunctionApp(callee, arguments, frame)
                            // case callee: Node.ListNode if callee.elements.headOption.contains(Node.IdentNode("syn")) =>
                            // evalSyntaxApp(callee, arguments, frame)
                            case callee =>
                                throw new EvaluationError(s"Not a function or builtin operator: $callee")
                        }
                    }
                }
            }
        }
        if (ctx.printStackFrames()) println(frame)
        result
    }

    private def isSpecialForm(n: String): Boolean = {
        List("fun", "quote", "let-syn", "if", "set", "do").contains(n)
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

    private def evalDefine(target: Node, expr: Node, frame: Frame, mutable: Boolean): Node = {
        target match {
            case Node.IdentNode(name) => {
                val calculatedValue = eval(expr, frame)
                frame.define(name, calculatedValue, mutable)
                unit
            }
            case _ => throw new EvaluationError("Definition target must be a valid identifier symbol.")
        }
    }

    private def evalFunctionApp(callee: Node.ListNode, rawArgs: List[Node], frame: Frame): Node = {
        val args      = rawArgs.map(arg => eval(arg, frame))
        val bodyFrame = new Frame()

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

        val callee_elems = callee.elements

        val body = callee_elems.slice(2, callee_elems.size)

        if (params.size != args.size) {
            throw new EvaluationError(s"Arity mismatch: function expects ${params.size} arguments, but received ${args.size}")
        }

        params.zip(args).foreach { case (param, arg) =>
            bodyFrame.define(param, arg, false)
        }

        body.foldLeft[Node](unit)((_, e) => eval(e, bodyFrame))
    }

    private def evalSyntaxApp(callee: Node.ListNode, args: List[Node], frame: Frame): Node = {
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
            bodyFrame.define(param, arg, false)
        }

        val expandedAst = substitute(body, bodyFrame)

        eval(expandedAst, frame)
    }

    private def substitute(node: Node, macroFrame: Frame): Node = {
        node match {
            case Node.IdentNode(name) =>
                macroFrame.lookup(name).getOrElse(node)

            case Node.ListNode(elements) =>
                Node.ListNode(elements.map(e => substitute(e, macroFrame)))

            case other =>
                other
        }
    }

    private def isFormals(node: Node, frame: Frame): Boolean = {
        node match {
            case Node.IdentNode(name) if !frame.contains(name) => true
            case Node.ListNode(elements) =>
                elements.forall(element =>
                    element match {
                        case Node.IdentNode(name) => !frame.contains(name)
                        case _                    => false
                    }
                )
            case _ => false
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
