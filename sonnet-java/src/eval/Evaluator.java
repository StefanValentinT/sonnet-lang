package eval;

import java.util.*;
import java.util.stream.*;
import syntax.AST;
import util.Zipper;

public class Evaluator {
	private final AST.ListNode unit = new AST.ListNode(
			List.of(new AST.IdentNode("quote"), new AST.ListNode(List.of())));

	public AST eval(AST node, Frame frame) {
		AST ast = switch (node) {
			case AST.IntNode n -> n;
			case AST.FloatNode n -> n;
			case AST.StringNode n -> n;
			case AST.IdentNode id -> {
				AST val = frame.lookup(id.name());
				if (val != null)
					yield val;
				throw new EvaluationError("Undefined variable: " + id.name());
			}
			case AST.ListNode list -> {
				List<AST> elements = list.elements();
				if (elements.isEmpty()) {
					yield list; // () -> () symbolic evaluation
				}

				if (isFunction(list)) {
					yield list;
				} else if (isQuote(list)) {
					yield elements.get(1);
				} else if (isConditional(list)) {
					yield evalConditional(elements.get(1), elements.get(2), elements.get(3), frame);
				} else if (isSet(list)) {
					yield evalSet(elements.get(1), elements.get(2), frame);
				}

				// Application

				AST first = elements.get(0);
				List<AST> arguments = elements.subList(1, elements.size());

				if (isBuiltIn(first)) {
					yield evaluateBuiltIn(((AST.IdentNode) first).name(), arguments, frame);
				}

				AST callee = eval(first, frame);

				if (isFunction(callee)) {
					yield evalFunctionApp((AST.ListNode) callee, arguments, frame);
				}

				throw new EvaluationError("First element of the list is not a function or builtin operator: " + callee);
			}
		};
		System.out.println(frame);
		return ast;
	}

	private boolean isFunction(AST f) {
		if (f instanceof AST.ListNode funNode) {
			List<AST> funElements = funNode.elements();
			if (funElements.size() != 3)
				return false;
			if (!(funElements.get(0) instanceof AST.IdentNode id && id.name().equals("fun")))
				return false;
			return isFormals(funElements.get(1));
		}
		return false;
	}

	private boolean isQuote(AST q) {
		if (q instanceof AST.ListNode quoteNode) {
			List<AST> quoteElements = quoteNode.elements();
			if (quoteElements.size() != 2)
				return false;
			if (!(quoteElements.get(0) instanceof AST.IdentNode id && id.name().equals("quote")))
				return false;
			return true;
		}
		return false;
	}

	private boolean isConditional(AST c) {
		if (c instanceof AST.ListNode cNode) {
			List<AST> cElements = cNode.elements();
			if (cElements.size() != 4)
				return false;
			if (!(cElements.get(0) instanceof AST.IdentNode id && id.name().equals("if")))
				return false;
			return true;
		}
		return false;
	}

	private boolean isSet(AST c) {
		if (c instanceof AST.ListNode cNode) {
			List<AST> cElements = cNode.elements();
			if (cElements.size() != 3)
				return false;
			if (!(cElements.get(0) instanceof AST.IdentNode id && id.name().equals("set")))
				return false;
			return true;
		}
		return false;
	}

	private AST evalConditional(AST cond, AST then, AST els, Frame frame) {
		AST c = eval(cond, frame);
		if (c.equals(new AST.IdentNode("true")))
			return eval(then, frame);
		if (c.equals(new AST.IdentNode("false")))
			return eval(els, frame);
		throw new EvaluationError("Conditionals condition must be to either 'true or 'false.");
	}

	private AST evalSet(AST target, AST expr, Frame frame) {
		if (!(target instanceof AST.IdentNode id)) {
			throw new EvaluationError("set assignment target must be a valid identifier symbol.");
		}
		AST calculatedValue = eval(expr, frame);
		frame.assign(id.name(), calculatedValue);
		return unit;
	}

	private AST evalFunctionApp(AST.ListNode callee, List<AST> rawArgs, Frame frame) {
		List<AST> args = rawArgs.stream().map(arg -> eval(arg, frame)).toList();

		Frame bodyFrame = new Frame(frame);

		AST paramNode = callee.elements().get(1);
		List<AST> paramsRaw = (paramNode instanceof AST.ListNode pList) ? pList.elements() : List.of(paramNode);
		List<String> params = paramsRaw.stream().map(e -> {
			if (e instanceof AST.IdentNode id) {
				return id.name();
			}
			{
				throw new EvaluationError("Formals must be a list of identifiers or a singular identifer.");
			}
		}).toList();

		if (params.stream().collect(Collectors.toSet()).size() != params.size()) {
			throw new EvaluationError("It is an error for a variable to appear more than once in formals.");
		}

		AST body = callee.elements().get(2);

		if (params.size() != args.size()) {
			throw new EvaluationError(
					"Arity mismatch: function expects " + params.size() + " arguments, but received " + args.size());
		}

		Zipper.zip(params.stream(), args.stream(), (param, arg) -> {

			bodyFrame.define(param, arg);

		});

		return eval(body, bodyFrame);
	}

	private boolean isFormals(AST f) {
		if (f instanceof AST.IdentNode id)
			return true;
		if (f instanceof AST.ListNode list)
			return list.elements().stream().allMatch(e -> e instanceof AST.IdentNode id);
		return false;
	}

	private boolean isBuiltIn(AST first) {
		if (first instanceof AST.IdentNode id) {
			String name = id.name();
			return name.equals("+") || name.equals("-") || name.equals("*") || name.equals("/") || name.equals("print");
		}
		return false;
	}

	private AST evaluateBuiltIn(String op, List<AST> rawArgs, Frame frame) {
		List<AST> args = rawArgs.stream().map(arg -> eval(arg, frame)).toList();

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
				boolean isFloat = rawArgs.get(0) instanceof AST.FloatNode node;
				if (isFloat) {
					yield new AST.FloatNode(switch (op) {
						case "+" -> args.stream().reduce(0.0, (acc, arg) -> {
							if (arg instanceof AST.FloatNode f)
								return (acc + f.value());
							throw new EvaluationError("Mixed  number types in builtin math operation '" + op + "'.");
						}, (a, b) -> a + b);
						case "-" -> args.stream().reduce(null, (acc, arg) -> {
							if (arg instanceof AST.FloatNode f)
								return (acc == null) ? f.value() : acc - f.value();
							throw new EvaluationError("Mixed  number types in builtin math operation '" + op + "'.");
						}, (a, b) -> a - b);
						case "*" -> args.stream().reduce(0.0, (acc, arg) -> {
							if (arg instanceof AST.FloatNode f)
								return (acc * f.value());
							throw new EvaluationError("Mixed  number types in builtin math operation '" + op + "'.");
						}, (a, b) -> a * b);
						case "/" -> args.stream().reduce(null, (acc, arg) -> {
							if (arg instanceof AST.FloatNode f)
								return (acc == null) ? f.value() : acc / f.value();
							throw new EvaluationError("Mixed  number types in builtin math operation '" + op + "'.");
						}, (a, b) -> a / b);
						default ->
							throw new EvaluationError("Impossible state. This would mean an undefined math operation.");
					});
				} else {
					yield new AST.IntNode(switch (op) {
						case "+" -> args.stream().reduce(0, (acc, arg) -> {
							if (arg instanceof AST.IntNode i)
								return (acc + i.value());
							throw new EvaluationError("Mixed  number types in builtin math operation '" + op + "'.");
						}, (a, b) -> a + b);
						case "-" -> args.stream().reduce(null, (acc, arg) -> {
							if (arg instanceof AST.IntNode i)
								return (acc == null) ? i.value() : acc / i.value();
							throw new EvaluationError("Mixed  number types in builtin math operation '" + op + "'.");
						}, (a, b) -> a - b);
						case "*" -> args.stream().reduce(0, (acc, arg) -> {
							if (arg instanceof AST.IntNode i)
								return (acc * i.value());
							throw new EvaluationError("Mixed  number types in builtin math operation '" + op + "'.");
						}, (a, b) -> a * b);
						case "/" -> args.stream().reduce(null, (acc, arg) -> {
							if (arg instanceof AST.IntNode i)
								return (acc == null) ? i.value() : acc / i.value();
							throw new EvaluationError("Mixed  number types in builtin math operation '" + op + "'.");
						}, (a, b) -> a / b);
						default ->
							throw new EvaluationError("Impossible state. This would mean an undefined math operation.");
					});
				}
			}
			default -> throw new EvaluationError("Unknown built-in operator: " + op);
		};
	}

	private double getNumericValue(AST node) {
		if (node instanceof AST.IntNode intNode)
			return intNode.value();
		if (node instanceof AST.FloatNode floatNode)
			return floatNode.value();
		throw new EvaluationError("Expected a numeric node value.");
	}
}
