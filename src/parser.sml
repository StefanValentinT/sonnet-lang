structure Parser =
struct

  datatype binop =
    Add
  | Sub
  | Mul
  | Div
  | Less
  | Greater
  | Equal
  | NotEqual
  | LessEqual
  | GreaterEqual

  datatype type_expr =
    Fun of type_expr * type_expr
  | Ident of string
  | Record of (string * term) list
  | TypeVar of string

  datatype term =
    Identifier of string
  | I32Lit of Int32.int
  | BoolLit of bool
  | Fun of term * term
  | Apply of term * term
  | If of term * term * term
  | BinOp of binop * term * term
  | Declaration of term * term (* name = e *)
  | Compound of term list (* (e_1, e_2, e_3)*)
  | Record of (string * term) list (* {a = 1, b = 1} *)
  | Project of term * string (* args.a *)
  | Typed of term * type_expr

  fun parse_atom tokens =
    case tokens of
      Lexer.NumberI32 n :: r => (I32Lit n, r)
    | Lexer.ValTrue :: r => (BoolLit true, r)
    | Lexer.ValFalse :: r => (BoolLit false, r)
    | Lexer.Identifier name :: r => (Identifier name, r)
    | Lexer.OpenParen :: r =>
        let
          val (t, r1) = parse_expr r
          val r2 =
            case r1 of
              Lexer.CloseParen :: r3 => r3
            | _ => raise Fail "Expected ')'"
        in
          (t, r2)
        end
    | Lexer.OpenBrace :: r =>
        let
          fun is_record (Lexer.Identifier _ :: Lexer.Assign :: _) = true
            | is_record _ = false

          fun parse_record_fields toks acc =
            case toks of
              Lexer.Identifier name :: Lexer.Assign :: rest =>
                let
                  val (e, r1) = parse_expr rest
                in
                  case r1 of
                    Lexer.Comma :: r2 =>
                      parse_record_fields r2 ((name, e) :: acc)
                  | Lexer.CloseBrace :: r2 =>
                      (Record (List.rev ((name, e) :: acc)), r2)
                  | _ => raise Fail "Expected ',' or '}' in record"
                end
            | Lexer.CloseBrace :: r1 => (Record (List.rev acc), r1)
            | _ => raise Fail "Expected field name in record"

          fun parse_compound_exprs toks acc =
            case toks of
              Lexer.CloseBrace :: r1 => (Compound (List.rev acc), r1)
            | _ =>
                let
                  val (e, r1) = parse_expr toks
                in
                  case r1 of
                    Lexer.Semicolon :: r2 => parse_compound_exprs r2 (e :: acc)
                  | Lexer.CloseBrace :: r2 =>
                      (Compound (List.rev (e :: acc)), r2)
                  | _ => raise Fail "Expected ';' or '}' in compound"
                end
        in
          if is_record r then parse_record_fields r []
          else parse_compound_exprs r []
        end
    | _ => raise Fail "Expected an identifier, number, or '('"

  and parse_project tokens =
    let
      val (t, r) = parse_atom tokens
      fun chain left toks =
        case toks of
          Lexer.Dot :: Lexer.Identifier field :: r2 =>
            chain (Project (left, field)) r2
        | _ => (left, toks)
    in
      chain t r
    end

  and parse_app tokens =
    let
      val (f, r) = parse_project tokens
      fun chain left toks =
        case toks of
          (Lexer.Identifier _ :: _) =>
            let val (arg, r2) = parse_project toks
            in chain (Apply (left, arg)) r2
            end
        | (Lexer.NumberI32 _ :: _) =>
            let val (arg, r2) = parse_project toks
            in chain (Apply (left, arg)) r2
            end
        | (Lexer.OpenParen :: _) =>
            let val (arg, r2) = parse_project toks
            in chain (Apply (left, arg)) r2
            end
        | (Lexer.OpenBrace :: _) =>
            let val (arg, r2) = parse_project toks
            in chain (Apply (left, arg)) r2
            end
        | _ => (left, toks)
    in
      chain f r
    end

  and parse_mul tokens =
    let
      val (left, r) = parse_app tokens
      fun chain l toks =
        case toks of
          Lexer.SymStar :: r2 =>
            let val (right, r3) = parse_app r2
            in chain (BinOp (Mul, l, right)) r3
            end
        | Lexer.SymSlash :: r2 =>
            let val (right, r3) = parse_app r2
            in chain (BinOp (Div, l, right)) r3
            end
        | _ => (l, toks)
    in
      chain left r
    end

  and parse_add tokens =
    let
      val (left, r) = parse_mul tokens
      fun chain l toks =
        case toks of
          Lexer.SymPlus :: r2 =>
            let val (right, r3) = parse_mul r2
            in chain (BinOp (Add, l, right)) r3
            end
        | Lexer.SymMinus :: r2 =>
            let val (right, r3) = parse_mul r2
            in chain (BinOp (Sub, l, right)) r3
            end
        | _ => (l, toks)
    in
      chain left r
    end

  and parse_cmp tokens =
    let
      val (left, r) = parse_add tokens
      fun chain l toks =
        case toks of
          Lexer.SymLess :: r2 =>
            let val (right, r3) = parse_add r2
            in (BinOp (Less, l, right), r3)
            end
        | Lexer.SymBigger :: r2 =>
            let val (right, r3) = parse_add r2
            in (BinOp (Greater, l, right), r3)
            end
        | Lexer.SymEqual :: r2 =>
            let val (right, r3) = parse_add r2
            in (BinOp (Equal, l, right), r3)
            end
        | _ => (l, toks)
    in
      chain left r
    end

  and parse_expr tokens =
    case tokens of
      Lexer.KeyFun :: Lexer.Identifier p :: Lexer.Arrow :: r =>
        let val (body, r2) = parse_expr r
        in (Fun (Identifier p, body), r2)
        end
    | Lexer.KeyIf :: r =>
        let
          val (c, r1) = parse_expr r
          val (t, r2) =
            case r1 of
              Lexer.KeyThen :: rt => parse_expr rt
            | _ => raise Fail "Expected then"
          val (e, r3) =
            case r2 of
              Lexer.KeyElse :: re => parse_expr re
            | _ => raise Fail "Expected else"
        in
          (If (c, t, e), r3)
        end
    | _ => parse_cmp tokens

  fun parse_term tokens =
    case tokens of
      Lexer.Identifier name :: Lexer.Assign :: rest =>
        let val (expr, r) = parse_expr rest
        in (Declaration (Identifier name, expr), r)
        end
    | _ => parse_expr tokens

  fun parse tokens =
    let
      fun loop toks acc =
        case toks of
          [] => List.rev acc
        | Lexer.Semicolon :: r => loop r acc
        | _ =>
            let
              val (t, remaining) = parse_term toks
            in
              case t of
                Declaration _ => loop remaining (t :: acc)
              | _ => raise Fail "Top-level must be declarations"
            end
    in
      loop tokens []
    end


  fun term_to_string t =
    let
      fun indent d =
        String.implode (List.tabulate (d * 2, fn _ => #" "))

      fun to_str d t =
        let
          val next_d = d + 1
          fun fmt (name, args) =
            name ^ "(\n"
            ^
            String.concatWith ",\n" (List.map (fn a => indent next_d ^ a) args)
            ^ "\n" ^ indent d ^ ")"
        in
          case t of
            Identifier s => "Identifier(" ^ s ^ ")"
          | I32Lit n => "I32Lit(" ^ Int32.toString n ^ ")"
          | BoolLit b => "BoolLit(" ^ Bool.toString b ^ ")"
          | Fun (p, b) => fmt ("Fun", [to_str next_d p, to_str next_d b])
          | Apply (f, a) => fmt ("Apply", [to_str next_d f, to_str next_d a])
          | BinOp (opn, l, r) =>
              fmt
                ( "BinOp"
                , [binop_to_string opn, to_str next_d l, to_str next_d r]
                )
          | If (c, t1, t2) =>
              fmt ("If", [to_str next_d c, to_str next_d t1, to_str next_d t2])
          | Declaration (i, v) =>
              fmt ("Declaration", [to_str next_d i, to_str next_d v])
          | Project (e, name) =>
              "Project(\n" ^ indent next_d ^ to_str next_d e ^ ",\n"
              ^ indent next_d ^ name ^ "\n" ^ indent d ^ ")"
          | Record fields =>
              let
                val field_strs =
                  List.map
                    (fn (k, v) => "(" ^ k ^ " = " ^ to_str (next_d + 1) v ^ ")")
                    fields
              in
                "Record(\n"
                ^
                String.concatWith ",\n"
                  (List.map (fn s => indent next_d ^ s) field_strs) ^ "\n"
                ^ indent d ^ ")"
              end
          | Compound ts =>
              "Compound(\n"
              ^
              String.concatWith ";\n"
                (List.map (fn t => indent next_d ^ to_str next_d t) ts) ^ "\n"
              ^ indent d ^ ")"
        end
    in
      to_str 0 t
    end

  and binop_to_string b =
    case b of
      Add => "Add"
    | Sub => "Sub"
    | Mul => "Mul"
    | Div => "Div"
    | Less => "Less"
    | Greater => "Greater"
    | Equal => "Equal"
    | NotEqual => "NotEqual"
    | LessEqual => "LessEqual"
    | GreaterEqual => "GreaterEqual"

  fun print_ast ast =
    let
      val _ = print "AST: \n"
      val _ = List.app (fn t => print (term_to_string t ^ "\n")) ast

    in
      ()
    end
end
