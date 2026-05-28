package token;

import app.CompilerError;

public class TokenizerError extends CompilerError {
	public TokenizerError() {
		super("Tokenizer", null);
	}
}
