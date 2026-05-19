package eval;

import app.CompilerError;

public class EvaluationError extends CompilerError {
	public EvaluationError(String msg) {
		super("Tree-Walk Interpreter", msg);
	}
}
