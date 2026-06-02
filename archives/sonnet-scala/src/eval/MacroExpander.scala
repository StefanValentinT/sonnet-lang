package eval

import syntax.Node
import syntax.Node.*
import app.CompilerContext

object MacroExpander {

    private case class MacroDef(leftParams: List[String], rightParams: List[String], body: Node)
    private type MacroEnv = Map[String, MacroDef]

    def expandMacros(node: Node): Node = {
        expandNode(node, Map.empty)
    }

    private def expandNode(node: Node, env: MacroEnv): Node = node match {
        case ListNode(elements) =>
            // STEP 1: Deeply expand all children first!
            // This ensures nested lists like (var x 10 (comment ...)) expand their inner macros first.
            val deeplyExpandedChildren = elements.map(e => expandNode(e, env))

            // STEP 2: Linearly scan for def-syntax definitions among the expanded children
            val (_, processedElements) = deeplyExpandedChildren.foldLeft((env, List.empty[Node])) { case ((currentEnv, acc), currentElement) =>
                currentElement match {
                    case ListNode(IdentNode("def-syntax") :: IdentNode(name) :: leftRaw :: rightRaw :: macroBody :: Nil) =>
                        val leftParams  = extractNames(leftRaw)
                        val rightParams = extractNames(rightRaw)
                        val newMacro    = MacroDef(leftParams, rightParams, macroBody)
                        (currentEnv + (name -> newMacro), acc)

                    case other =>
                        (currentEnv, acc :+ other)
                }
            }

            // STEP 3: Check if this flat list layer contains a macro invocation
            // It could be a structural node (like a comment stripping itself) or a generalized macro.
            val macroIndex = processedElements.indexWhere {
                case IdentNode(name) => env.contains(name)
                case _               => false
            }

            if (macroIndex != -1) {
                val lefts     = processedElements.take(macroIndex)
                val macroName = processedElements(macroIndex).asInstanceOf[IdentNode].name
                val rights    = processedElements.drop(macroIndex + 1)

                val mac = env(macroName)

                // Spin up compile-time evaluation environment
                val compileTimeFrame = Frame()
                mac.leftParams.zip(lefts).foreach { case (p, arg) => compileTimeFrame.define(p, arg, false) }
                mac.rightParams.zip(rights).foreach { case (p, arg) => compileTimeFrame.define(p, arg, false) }

                val evaluator    = new Evaluator(CompilerContext.default())
                val generatedAst = evaluator.eval(mac.body, compileTimeFrame)

                // Recursively expand the result in case the macro output contains more macros
                expandNode(generatedAst, env)
            } else {
                ListNode(processedElements)
            }

        case other => other
    }

    private def extractNames(node: Node): List[String] = node match {
        case ListNode(pList) =>
            pList.map {
                case IdentNode(name) => name
                case _               => throw new Exception("Macro parameters must be identifiers")
            }
        case IdentNode(name) => List(name)
        case _               => throw new Exception("Macro parameters must be an identifier")
    }
}
