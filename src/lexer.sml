structure Lexer =
struct

  datatype lexem =
    Identifier of string
  | NumberI32 of Int32.int
  | Assign
  | KeyIf
  | KeyThen
  | KeyElse
  | KeyFun
  | Arrow
  | TypeBool
  | ValTrue
  | ValFalse
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Comma
  | Dot
  | Semicolon
  | SymMinus
  | SymPlus
  | SymStar
  | SymSlash
  | SymEqual
  | SymNotEqual
  | SymBigger
  | SymLess
  | SymLessThan
  | SymBiggerThan

  fun show_lexem lex =
    case lex of
      Identifier s => "Identifier " ^ s
    | NumberI32 i => "Number " ^ Int32.toString i
    | Assign => "Assign"
    | KeyIf => "If"
    | KeyThen => "Then"
    | KeyElse => "Else"
    | KeyFun => "Fun"
    | Arrow => "Arrow"
    | TypeBool => "Bool"
    | ValTrue => "True"
    | ValFalse => "False"
    | OpenParen => "OpenParen"
    | CloseParen => "CloseParen"
    | OpenBrace => "OpenBrace"
    | CloseBrace => "CloseBrace"
    | Comma => "Comma"
    | Dot => "Dot"
    | Semicolon => "Semicolon"
    | SymMinus => "Minus"
    | SymPlus => "Plus"
    | SymStar => "Star"
    | SymSlash => "Slash"
    | SymEqual => "Equal"
    | SymNotEqual => "NotEqual"
    | SymBigger => "Greater"
    | SymLess => "Less"
    | SymLessThan => "LessEqual"
    | SymBiggerThan => "GreaterEqual"

  fun print_tokens tokens =
    List.app (fn t => TextIO.print (show_lexem t ^ "\n")) tokens

  fun is_whitespace c =
    c = #" " orelse c = #"\t" orelse c = #"\n" orelse c = #"\r"

  fun is_digit c = #"0" <= c andalso c <= #"9"
  fun is_letter c =
    (#"a" <= c andalso c <= #"z") orelse (#"A" <= c andalso c <= #"Z")

  fun read_word (s, i) =
    let
      val len = String.size s
      fun loop j buf =
        if j >= len then
          (buf, j)
        else
          let
            val c = String.sub (s, j)
          in
            if is_letter c orelse is_digit c then
              loop (j + 1) (buf ^ String.str c)
            else
              (buf, j)
          end
    in
      loop i ""
    end

  fun read_number (s, i) =
    let
      val len = String.size s
      fun loop j buf =
        if j >= len then
          (case Int32.fromString buf of
             SOME n => (n, j)
           | NONE => raise Fail "Invalid i32 literal")
        else
          let
            val c = String.sub (s, j)
          in
            if is_digit c then
              loop (j + 1) (buf ^ String.str c)
            else
              (case Int32.fromString buf of
                 SOME n => (n, j)
               | NONE => raise Fail "Invalid i32 literal")
          end
    in
      loop i ""
    end

  fun tokenize (s, i, acc) =
    if i >= String.size s then
      List.rev acc
    else
      let
        val c = String.sub (s, i)
      in
        if is_whitespace c then
          tokenize (s, i + 1, acc)
        else
          case c of
            #"=" =>
              if i + 1 < String.size s andalso String.sub (s, i + 1) = #"=" then
                tokenize (s, i + 2, SymEqual :: acc)
              else
                tokenize (s, i + 1, Assign :: acc)
          | #"(" => tokenize (s, i + 1, OpenParen :: acc)
          | #")" => tokenize (s, i + 1, CloseParen :: acc)
          | #"{" => tokenize (s, i + 1, OpenBrace :: acc)
          | #"}" => tokenize (s, i + 1, CloseBrace :: acc)
          | #";" => tokenize (s, i + 1, Semicolon :: acc)
          | #"," => tokenize (s, i + 1, Comma :: acc)
          | #"." => tokenize (s, i + 1, Dot :: acc)
          | #"-" =>
              if i + 1 < String.size s andalso String.sub (s, i + 1) = #">" then
                tokenize (s, i + 2, Arrow :: acc)
              else
                tokenize (s, i + 1, SymMinus :: acc)
          | #"+" => tokenize (s, i + 1, SymPlus :: acc)
          | #"*" => tokenize (s, i + 1, SymStar :: acc)
          | #"/" => tokenize (s, i + 1, SymSlash :: acc)
          | #"<" =>
              if i + 1 < String.size s andalso String.sub (s, i + 1) = #"=" then
                tokenize (s, i + 2, SymLessThan :: acc)
              else
                tokenize (s, i + 1, SymLess :: acc)
          | #">" =>
              if i + 1 < String.size s andalso String.sub (s, i + 1) = #"=" then
                tokenize (s, i + 2, SymBiggerThan :: acc)
              else
                tokenize (s, i + 1, SymBigger :: acc)
          | #"!" =>
              if i + 1 < String.size s andalso String.sub (s, i + 1) = #"=" then
                tokenize (s, i + 2, SymNotEqual :: acc)
              else
                raise Fail ("Unknown char: " ^ String.str c)
          | _ =>
              if is_letter c then
                let
                  val (word, new_i) = read_word (s, i)
                  val token =
                    case word of
                      "if" => KeyIf
                    | "then" => KeyThen
                    | "else" => KeyElse
                    | "fun" => KeyFun
                    | "Bool" => TypeBool
                    | "true" => ValTrue
                    | "false" => ValFalse
                    | _ => Identifier word
                in
                  tokenize (s, new_i, token :: acc)
                end
              else if is_digit c then
                let val (num, new_i) = read_number (s, i)
                in tokenize (s, new_i, NumberI32 num :: acc)
                end
              else
                raise Fail ("Unknown char: " ^ String.str c)
      end

end
