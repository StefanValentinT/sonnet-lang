package org.example.syntax;

public class PrettyPrinter {
	public String prettifyAST(AST node) {
		return prettifyAST(node, 0);
	}

	private String prettifyAST(AST node, int tabLevel) {
		return switch (node) {
			case AST.TrueNode() -> node.toString();
			case AST.FalseNode() -> node.toString();
			case AST.NihilNode() -> node.toString();
			case AST.BoolTypeNode() -> node.toString();
			case AST.NihilTypeNode() -> node.toString();
			case AST.F16TypeNode() -> node.toString();
			case AST.F32TypeNode() -> node.toString();
			case AST.F64TypeNode() -> node.toString();
			case AST.I8TypeNode() -> node.toString();
			case AST.I16TypeNode() -> node.toString();
			case AST.I32TypeNode() -> node.toString();
			case AST.I64TypeNode() -> node.toString();
			case AST.U8TypeNode() -> node.toString();
			case AST.U16TypeNode() -> node.toString();
			case AST.U32TypeNode() -> node.toString();
			case AST.U64TypeNode() -> node.toString();

			case AST.IntNode(int value) -> node.toString();
			case AST.FloatNode(double value) -> node.toString();
			case AST.StringNode(String value) -> node.toString();
			case AST.SymbolNode(String symbol) -> node.toString();
			case AST.IdentNode(String name) -> node.toString();

			case AST.ListNode(var elements) -> {
				if (elements.isEmpty()) {
					yield "()";
				}
				StringBuilder sb = new StringBuilder("(\n");
				String currentTabs = "\t".repeat(tabLevel);
				String childTabs = "\t".repeat(tabLevel + 1);
				for (AST element : elements) {
					sb.append(childTabs)
							.append(prettifyAST(element, tabLevel + 1))
							.append("\n");
				}
				sb.append(currentTabs).append(")");
				yield sb.toString();
			}
		};
	}
}
