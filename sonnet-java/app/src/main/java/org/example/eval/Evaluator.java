package org.example.eval;

import java.util.List;
import org.example.syntax.AST;

public class Evaluator {
	private final Environment env = new Environment();

	public AST evaluate(AST program) {
		if (!(program instanceof AST.ListNode root)) {
			throw new EvaluationError();
		}

		for (AST node : root.elements()) {
			if (node instanceof AST.ListNode list
					&& !list.elements().isEmpty()
					&& list.elements().get(0) instanceof AST.IdentNode id
					&& id.name().equals("def")) {

				if (list.elements().size() < 3) throw new EvaluationError();
				if (!(list.elements().get(1) instanceof AST.IdentNode target)) throw new EvaluationError();

				env.defineGlobal(target.name(), eval(list.elements().get(2), null));
			}
		}

		return execute(env.getGlobal("main"), List.of(), null);
	}

	private AST eval(AST node, Environment.Frame frame) {
		return switch (node) {
			case AST.IntNode n -> n;
			case AST.FloatNode n -> n;
			case AST.StringNode n -> n;
			case AST.SymbolNode n -> n;

			case AST.TrueNode n -> n;
			case AST.FalseNode n -> n;
			case AST.NihilNode n -> n;
			case AST.BoolTypeNode n -> n;
			case AST.NihilTypeNode n -> n;
			case AST.F16TypeNode n -> n;
			case AST.F32TypeNode n -> n;
			case AST.F64TypeNode n -> n;
			case AST.I8TypeNode n -> n;
			case AST.I16TypeNode n -> n;
			case AST.I32TypeNode n -> n;
			case AST.I64TypeNode n -> n;
			case AST.U8TypeNode n -> n;
			case AST.U16TypeNode n -> n;
			case AST.U32TypeNode n -> n;
			case AST.U64TypeNode n -> n;

			case AST.IdentNode n -> resolveIdentifier(n, frame);
			case AST.ListNode list -> evaluateList(list, frame);
		};
	}

	private AST resolveIdentifier(AST.IdentNode id, Environment.Frame frame) {
		return (frame != null && frame.contains(id.name())) ? frame.get(id.name()) : env.getGlobal(id.name());
	}

	private AST evaluateList(AST.ListNode list, Environment.Frame frame) {
		if (list.elements().isEmpty()) {
			return list;
		}

		AST head = list.elements().get(0);

		if (head instanceof AST.IdentNode op) {
			return switch (op.name()) {
				case "fun" -> list;
				case "quote" -> {
					if (list.elements().size() < 2) throw new EvaluationError();
					yield list.elements().get(1);
				}
				case "if" -> handleIf(list.elements(), frame);
				case "do" -> handleDo(list.elements(), frame);
				case "print" -> handlePrint(list.elements(), frame);
				case "list/get" -> handleListGet(list.elements(), frame);
				case "+", "-" -> handleMath(op.name(), list.elements(), frame);
				case "eq", "<", ">" -> handleComparison(op.name(), list.elements(), frame);
				default -> {
					AST targetFunction = eval(head, frame);
					List<AST> args = list.elements().subList(1, list.elements().size());
					yield execute(targetFunction, args, frame);
				}
			};
		}

		AST targetFunction = eval(head, frame);
		List<AST> args = list.elements().subList(1, list.elements().size());
		return execute(targetFunction, args, frame);
	}

	private AST handleIf(List<AST> elements, Environment.Frame frame) {
		if (elements.size() < 4) throw new EvaluationError();

		AST cond = eval(elements.get(1), frame);

		boolean isTrue =
				switch (cond) {
					case AST.TrueNode ignored -> true;
					case AST.FalseNode ignored -> false;
					default -> throw new EvaluationError();
				};

		return isTrue ? eval(elements.get(2), frame) : eval(elements.get(3), frame);
	}

	private AST handleDo(List<AST> elements, Environment.Frame frame) {
		AST result = new AST.ListNode(List.of());
		for (int i = 1; i < elements.size(); i++) {
			result = eval(elements.get(i), frame);
		}
		return result;
	}

	private AST handlePrint(List<AST> elements, Environment.Frame frame) {
		AST res = new AST.ListNode(List.of());
		for (int i = 1; i < elements.size(); i++) {
			res = eval(elements.get(i), frame);
			switch (res) {
				case AST.StringNode s -> System.out.print(s.value());
				default -> System.out.print(":(" + res.toString() + ")");
			}
		}
		System.out.println();
		return res;
	}

	private AST handleListGet(List<AST> elements, Environment.Frame frame) {
		if (elements.size() < 3) throw new EvaluationError();

		AST targetList = eval(elements.get(1), frame);
		AST targetIndex = eval(elements.get(2), frame);

		if (!(targetList instanceof AST.ListNode list) || !(targetIndex instanceof AST.IntNode indexNode)) {
			throw new EvaluationError();
		}

		int index = indexNode.value();
		List<AST> innerElements = list.elements();

		if (index < 0 || index >= innerElements.size()) {
			throw new EvaluationError();
		}

		return innerElements.get(index);
	}

	private AST handleMath(String op, List<AST> elements, Environment.Frame frame) {
		if (elements.size() < 2) throw new EvaluationError();

		AST first = eval(elements.get(1), frame);

		double total;
		boolean isFloat;

		if (first instanceof AST.IntNode i) {
			total = i.value();
			isFloat = false;
		} else if (first instanceof AST.FloatNode f) {
			total = f.value();
			isFloat = true;
		} else {
			throw new EvaluationError();
		}

		if (elements.size() == 2) {
			if (op.equals("-")) total = -total;
			return isFloat ? new AST.FloatNode(total) : new AST.IntNode((int) total);
		}

		for (int i = 2; i < elements.size(); i++) {
			AST next = eval(elements.get(i), frame);

			double val =
					switch (next) {
						case AST.IntNode in -> in.value();
						case AST.FloatNode fn -> {
							isFloat = true;
							yield fn.value();
						}
						default -> throw new EvaluationError();
					};

			total = op.equals("+") ? total + val : total - val;
		}

		return isFloat ? new AST.FloatNode(total) : new AST.IntNode((int) total);
	}

	private AST handleComparison(String op, List<AST> elements, Environment.Frame frame) {
		if (elements.size() < 2) throw new EvaluationError();

		boolean result =
				switch (op) {
					case "eq" -> {
						AST left = eval(elements.get(1), frame);
						boolean allEqual = true;

						for (int i = 2; i < elements.size(); i++) {
							AST right = eval(elements.get(i), frame);
							boolean pairEqual = false;

							if (left instanceof AST.IntNode l && right instanceof AST.IntNode r) {
								pairEqual = l.value() == r.value();
							} else if (left instanceof AST.FloatNode l && right instanceof AST.FloatNode r) {
								pairEqual = l.value() == r.value();
							} else if (left instanceof AST.StringNode l && right instanceof AST.StringNode r) {
								pairEqual = l.value().equals(r.value());
							} else if (left instanceof AST.SymbolNode l && right instanceof AST.SymbolNode r) {
								pairEqual = l.symbol().equals(r.symbol());
							} else if (left instanceof AST.TrueNode && right instanceof AST.TrueNode) {
								pairEqual = true;
							} else if (left instanceof AST.FalseNode && right instanceof AST.FalseNode) {
								pairEqual = true;
							} else if (left instanceof AST.NihilNode && right instanceof AST.NihilNode) {
								pairEqual = true;
							}

							if (!pairEqual) {
								allEqual = false;
								break;
							}
							left = right;
						}
						yield allEqual;
					}
					case "<", ">" -> {
						if (elements.size() != 3) throw new EvaluationError();

						AST left = eval(elements.get(1), frame);
						AST right = eval(elements.get(2), frame);

						double lVal =
								switch (left) {
									case AST.IntNode l -> l.value();
									case AST.FloatNode l -> l.value();
									default -> throw new EvaluationError();
								};
						double rVal =
								switch (right) {
									case AST.IntNode r -> r.value();
									case AST.FloatNode r -> r.value();
									default -> throw new EvaluationError();
								};
						yield op.equals("<") ? lVal < rVal : lVal > rVal;
					}
					default -> throw new EvaluationError();
				};

		return result ? new AST.TrueNode() : new AST.FalseNode();
	}

	private AST execute(AST funcValue, List<AST> args, Environment.Frame callerFrame) {
		if (!(funcValue instanceof AST.ListNode funcList)
				|| funcList.elements().isEmpty()
				|| !(funcList.elements().get(0) instanceof AST.IdentNode id)
				|| !id.name().equals("fun")) {
			throw new EvaluationError();
		}

		if (funcList.elements().size() < 3) throw new EvaluationError();

		AST rawParams = funcList.elements().get(1);

		List<AST> params =
				switch (rawParams) {
					case AST.ListNode pl -> pl.elements();
					case AST.IdentNode in -> List.of(in);
					default -> throw new EvaluationError();
				};

		if (args.size() != params.size()) {
			throw new EvaluationError();
		}

		Environment.Frame nextFrame = new Environment.Frame();

		for (int i = 0; i < args.size(); i++) {
			if (!(params.get(i) instanceof AST.IdentNode paramId)) {
				throw new EvaluationError();
			}
			nextFrame.define(paramId.name(), eval(args.get(i), callerFrame));
		}

		AST result = new AST.ListNode(List.of());
		for (int i = 2; i < funcList.elements().size(); i++) {
			result = eval(funcList.elements().get(i), nextFrame);
		}

		return result;
	}
}
