package org.example.syntax;

import java.util.List;

public sealed interface AST {

	record IntNode(int value) implements AST {
		@Override
		public String toString() {
			return String.valueOf(value);
		}
	}

	record FloatNode(double value) implements AST {
		@Override
		public String toString() {
			return String.valueOf(value);
		}
	}

	record StringNode(String value) implements AST {
		@Override
		public String toString() {
			return "\"" + value + "\"";
		}
	}

	record IdentNode(String name) implements AST {
		@Override
		public String toString() {
			return name;
		}
	}

	record ListNode(List<AST> elements) implements AST {
		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			sb.append("(");
			for (int i = 0; i < elements.size(); i++) {
				sb.append(elements.get(i).toString());
				if (i < elements.size() - 1) {
					sb.append(" ");
				}
			}
			sb.append(")");
			return sb.toString();
		}
	}
}
