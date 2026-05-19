package org.example.eval;

import java.util.ArrayList;
import java.util.List;
import org.example.syntax.AST;

public class Evaluator {
	public AST eval(AST node, Frame frame) {
		return switch (node) {
			case AST.IntNode n -> n;
			case AST.FloatNode n -> n;
			case AST.StringNode n -> n;
			case AST.IdentNode id -> {
				AST val = frame.lookup(id.name());
				if (val != null) yield val;
				throw new EvaluationError("Undefined variable: " + id.name());
			}
			case AST.ListNode list -> {
				List<AST> elements = list.elements();
				if (elements.isEmpty()) {
					throw new EvaluationError("Cannot evaluate an empty list.");
				}

				AST first = elements.get(0);

				if (first instanceof AST.IdentNode id && id.name().equals("fun")) {
					if (elements.size() != 3) {
						throw new EvaluationError(
								"Invalid 'fun' syntax. Expected (fun param body) or (fun (params) body)");
					}
					AST paramsCheck = elements.get(1);
					if (!(paramsCheck instanceof AST.IdentNode) && !(paramsCheck instanceof AST.ListNode)) {
						throw new EvaluationError(
								"Function parameters must be an identifier or a list of identifiers.");
					}
					yield list;
				}

				if (first instanceof AST.IdentNode id && isBuiltIn(id.name())) {
					yield evaluateBuiltIn(id.name(), elements.subList(1, elements.size()), frame);
				}

				AST callable = eval(first, frame);

				if (callable instanceof AST.ListNode funNode) {
					List<AST> funElements = funNode.elements();
					if (!funElements.isEmpty()
							&& funElements.get(0) instanceof AST.IdentNode id
							&& id.name().equals("fun")) {

						AST paramNode = funElements.get(1);
						AST body = funElements.get(2);

						List<AST> evaluatedArgs = elements.subList(1, elements.size()).stream()
								.map(arg -> eval(arg, frame))
								.toList();

						List<AST> params =
								(paramNode instanceof AST.ListNode pList) ? pList.elements() : List.of(paramNode);

						if (params.size() != evaluatedArgs.size()) {
							throw new EvaluationError("Arity mismatch: function expects " + params.size()
									+ " arguments, but received " + evaluatedArgs.size());
						}

						Frame bodyFrame = new Frame(frame);
						for (int i = 0; i < params.size(); i++) {
							if (!(params.get(i) instanceof AST.IdentNode paramId)) {
								throw new EvaluationError("Function parameter must be an identifier symbol.");
							}
							bodyFrame.define(paramId.name(), evaluatedArgs.get(i));
						}

						yield eval(body, bodyFrame);
					}
				}

				throw new EvaluationError(
						"First element of the list is not a function or builtin operator: " + callable);
			}
		};
	}

	private boolean isFunction(AST f) {
		if (f instanceof AST.ListNode funNode) {
			List<AST> funElements = funNode.elements();
			return !funElements.isEmpty()
					&& funElements.get(0) instanceof AST.IdentNode id
					&& id.name().equals("fun");
		} else {
			return false;
		}
	}

	private boolean isBuiltIn(String name) {
		return name.equals("+") || name.equals("-") || name.equals("*") || name.equals("/") || name.equals("print");
	}

	private AST evaluateBuiltIn(String op, List<AST> rawArgs, Frame frame) {
		List<AST> args = new ArrayList<>();
		for (AST arg : rawArgs) {
			args.add(eval(arg, frame));
		}

		return switch (op) {
			case "print" -> {
				StringBuilder sb = new StringBuilder();
				for (int i = 0; i < args.size(); i++) {
					AST arg = args.get(i);
					if (arg instanceof AST.StringNode str) {
						sb.append(str.value());
					} else {
						sb.append(arg.toString());
					}
				}
				System.out.println(sb.toString());
				yield new AST.StringNode(sb.toString());
			}

			case "+", "-", "*", "/" -> {
				boolean isFloat = false;
				for (AST arg : args) {
					if (arg instanceof AST.FloatNode) {
						isFloat = true;
						break;
					} else if (!(arg instanceof AST.IntNode)) {
						throw new EvaluationError("Built-in math operator '" + op + "' expects numeric arguments.");
					}
				}

				if (isFloat) {
					double result = getNumericValue(args.get(0));

					for (int i = 1; i < args.size(); i++) {
						double val = getNumericValue(args.get(i));
						switch (op) {
							case "+" -> result += val;
							case "-" -> result -= val;
							case "*" -> result *= val;
							case "/" -> {
								if (val == 0.0) throw new EvaluationError("Division by zero error.");
								result /= val;
							}
						}
					}
					yield new AST.FloatNode(result);
				} else {
					int result = ((AST.IntNode) args.get(0)).value();

					for (int i = 1; i < args.size(); i++) {
						int val = ((AST.IntNode) args.get(i)).value();
						switch (op) {
							case "+" -> result += val;
							case "-" -> result -= val;
							case "*" -> result *= val;
							case "/" -> {
								if (val == 0) throw new EvaluationError("Division by zero error.");
								result /= val;
							}
						}
					}
					yield new AST.IntNode(result);
				}
			}

			default -> throw new EvaluationError("Unknown built-in operator: " + op);
		};
	}

	private double getNumericValue(AST node) {
		if (node instanceof AST.IntNode intNode) return intNode.value();
		if (node instanceof AST.FloatNode floatNode) return floatNode.value();
		throw new EvaluationError("Expected a numeric node value.");
	}
}
