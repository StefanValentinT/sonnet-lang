package org.example.eval;

import org.example.CompilerError;

public class EvaluationError extends CompilerError {
	public EvaluationError(String msg) {
		super("Tree-Walk Interpreter", msg);
	}
}
