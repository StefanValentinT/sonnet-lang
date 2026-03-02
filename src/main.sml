
fun read_file filename =
    let
        val ins = TextIO.openIn filename
        val content = TextIO.inputAll ins
    in
        TextIO.closeIn ins;
        content
    end

val _ = let
    val args = CommandLine.arguments ()
    val filename =
        if List.null args then raise Fail "Usage: program <filename>"
        else List.nth (args, 0)

    val content = read_file filename
    val _ = TextIO.output (TextIO.stdOut, content)

    val tokens = Lexer.tokenize (content, 0, [])
  
    val _ = Lexer.print_tokens tokens

	val program_ast = Parser.parse tokens

	val _ = Parser.print_ast program_ast

	in () end
