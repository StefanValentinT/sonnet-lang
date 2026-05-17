package org.example.eval;

import java.util.HashMap;
import java.util.Map;
import org.example.syntax.AST;

public class Environment {
	private final Map<String, AST> globals = new HashMap<>();

	public void defineGlobal(String name, AST value) {
		globals.put(name, value);
	}

	public AST getGlobal(String name) {
		if (!globals.containsKey(name)) {
			throw new EvaluationError();
		}
		return globals.get(name);
	}

	public static class Frame {
		private final Map<String, AST> bindings = new HashMap<>();

		public void define(String name, AST value) {
			bindings.put(name, value);
		}

		public AST get(String name) {
			return bindings.get(name);
		}

		public boolean contains(String name) {
			return bindings.containsKey(name);
		}
	}
}
