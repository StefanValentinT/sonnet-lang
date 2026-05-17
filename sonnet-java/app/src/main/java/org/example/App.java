package org.example;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import org.example.eval.Evaluator;
import org.example.format.Formatter;
import org.example.parser.Parser;
import org.example.scanner.FileScanner;
import org.example.syntax.AST;
import org.example.token.Token;
import org.example.token.Tokenizer;

public class App {

	public static void main(String[] args) {
		if (args.length < 2) {
			printUsage();
			return;
		}

		String mode = args[0].toLowerCase();
		String filename = args[1];

		FileScanner scanner = new FileScanner();
		String fileContent = scanner.readFile(filename);

		switch (mode) {
			case "fmt":
				fmt(filename, fileContent);
				break;
			case "run":
				run(fileContent);
				break;
			case "help":
				printUsage();
				break;
			default:
				System.out.println("Mode not recognized. Use mode help to get an overview of availaible modes.");
				break;
		}
	}

	private static void fmt(String filename, String fileContent) {
		try {
			List<Token> tokenStream = new Tokenizer().tokenize(fileContent);

			Formatter formatter = new Formatter();
			String formattedCode = formatter.format(tokenStream);

			Files.writeString(Paths.get(filename), formattedCode);
		} catch (Exception e) {
			throw new CompilerError("Compiler", null);
		}
	}

	private static void run(String fileContent) {
		List<Token> tokenStream = new Tokenizer().tokenize(fileContent);
		AST ast = new Parser().parse(tokenStream);
		System.out.println(ast);

		Evaluator eval = new Evaluator();
		eval.evaluate(ast);
	}

	private static void printUsage() {
		System.out.println("Usage:");
		System.out.println("	To format:   gradle run --args=\"fmt filename.st\"");
		System.out.println("	To run:      gradle run --args=\"run filename.st\"");
		System.out.println("	To get this: gradle run --args=\"help\"");
	}
}
