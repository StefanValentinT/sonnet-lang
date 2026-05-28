package app;

import eval.Evaluator;
import eval.Frame;
import format.Formatter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Scanner;
import parser.Parser;
import scanner.FileScanner;
import syntax.AST;
import token.Token;
import token.Tokenizer;

public class App {

	public static void main(String[] args) {
		FileScanner scanner = new FileScanner();
		String mode = args[0].toLowerCase();
		String filename;
		String fileContent;

		switch (mode) {
			case "fmt" :
				filename = args[1];
				fileContent = scanner.readFile(filename);

				fmt(filename, fileContent);
				break;
			case "run" :
				filename = args[1];

				fileContent = scanner.readFile(filename);
				run(fileContent);
				break;
			case "repl" :
				startRepl();
				break;
			case "help" :
				printUsage();
				break;
			default :
				System.out.println(
						"Mode '" + mode + "' not recognized. Use mode help to get an overview of availaible modes.");
				break;
		}
	}

	private static void startRepl() {
		while (true) {
			Scanner scanner = new Scanner(System.in);
			System.out.print(">>> ");
			String line = scanner.nextLine();
			if (line == "exit") {
				break;
			}
			List<Token> tokenStream = new Tokenizer().tokenize(line);
			AST ast = new Parser().parse(tokenStream);
			Evaluator eval = new Evaluator();
			AST res = eval.eval(ast, new Frame());
			System.out.println(res);
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
		System.out.println("Output:");
		System.out.println(eval.eval(ast, new Frame()));
	}

	private static void printUsage() {
		System.out.println("Usage:");
		System.out.println("	To format:   gradle run --args=\"fmt filename.st\"");
		System.out.println("	To run:      gradle run --args=\"run filename.st\"");
		System.out.println("	To get this: gradle run --args=\"help\"");
	}
}
