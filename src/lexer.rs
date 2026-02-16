use crate::queue::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    IntLiteral32(i32),
    IntLiteral64(i64),
    FloatLiteral64(f64),
    CharLiteral(char),
    StringLiteral(String),
    Keyword(String),

    OpenParen,
    CloseParen,

    OpenBrace,
    CloseBrace,

    OpenBracket,
    CloseBracket,

    Semicolon,

    Tilde,
    Minus,
    Decrement,

    Plus,
    Multiply,
    Divide,
    Remainder,

    Not,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,

    Assign,

    QuestionMark,
    Colon,

    Comma,
    Dot,

    EOF,
}

const KEYWORDS: [&str; 19] = [
    "fun", "cofun", "I32", "I64", "F64", "Char", "Ref", "Unit", "if", "then", "else", "while",
    "break", "continue", "match", "let", "as", "ref", "deref",
];

pub fn lex_string(input: String) -> Queue<Token> {
    let mut input = str_to_queue(input);
    let mut tokens: Queue<Token> = Queue::new();

    while !input.is_empty() {
        consume_while(&mut input, |c| matches!(c, ' ' | '\t' | '\n'));
        if input.is_empty() {
            break;
        }

        let c = input.peek().unwrap();

        match c {
            '/' => {
                st(Token::Divide, &mut input, &mut tokens);
            }

            'a'..='z' | 'A'..='Z' | '_' => tokens.add(lex_identifier(&mut input)),

            '0'..='9' => tokens.add(lex_number(&mut input)),

            '(' => {
                st(Token::OpenParen, &mut input, &mut tokens);
            }
            ')' => {
                st(Token::CloseParen, &mut input, &mut tokens);
            }
            '{' => {
                st(Token::OpenBrace, &mut input, &mut tokens);
            }
            '[' => {
                st(Token::OpenBracket, &mut input, &mut tokens);
            }
            ']' => {
                st(Token::CloseBracket, &mut input, &mut tokens);
            }
            '}' => {
                st(Token::CloseBrace, &mut input, &mut tokens);
            }
            ';' => {
                st(Token::Semicolon, &mut input, &mut tokens);
            }
            ',' => {
                st(Token::Comma, &mut input, &mut tokens);
            }
            '.' => {
                st(Token::Dot, &mut input, &mut tokens);
            }
            '~' => {
                st(Token::Tilde, &mut input, &mut tokens);
            }
            '?' => {
                st(Token::QuestionMark, &mut input, &mut tokens);
            }
            ':' => {
                st(Token::Colon, &mut input, &mut tokens);
            }

            '-' => tok_alt(
                &mut input,
                &mut tokens,
                &[("--", Token::Decrement), ("-", Token::Minus)],
            ),

            '+' => tok_alt(&mut input, &mut tokens, &[("+", Token::Plus)]),
            '*' => tok_alt(&mut input, &mut tokens, &[("*", Token::Multiply)]),
            '%' => tok_alt(&mut input, &mut tokens, &[("%", Token::Remainder)]),

            '!' => tok_alt(
                &mut input,
                &mut tokens,
                &[("!=", Token::NotEqual), ("!", Token::Not)],
            ),

            '<' => tok_alt(
                &mut input,
                &mut tokens,
                &[("<=", Token::LessOrEqual), ("<", Token::LessThan)],
            ),

            '>' => tok_alt(
                &mut input,
                &mut tokens,
                &[(">=", Token::GreaterOrEqual), (">", Token::GreaterThan)],
            ),

            '=' => tok_alt(
                &mut input,
                &mut tokens,
                &[("==", Token::Equal), ("=", Token::Assign)],
            ),

            '|' => tok_alt(&mut input, &mut tokens, &[("||", Token::Or)]),

            '&' => tok_alt(&mut input, &mut tokens, &[("&&", Token::And)]),

            '"' => tokens.add(lex_string_literal(&mut input)),
            '\'' => tokens.add(lex_char_literal(&mut input)),

            _ => panic!("Lexer error: unexpected character '{}'", c),
        }
    }

    tokens.add(Token::EOF);
    tokens
}

fn st(t: Token, input: &mut Queue<char>, tokens: &mut Queue<Token>) {
    input.consume();
    tokens.add(t);
}

fn tok_alt(input: &mut Queue<char>, tokens: &mut Queue<Token>, alts: &[(&str, Token)]) {
    for (s, t) in alts {
        if s.chars().enumerate().all(|(i, c)| input.is_there(i, c)) {
            for _ in 0..s.len() {
                input.consume();
            }
            tokens.add(t.clone());
            return;
        }
    }
    panic!(
        "Lexer error: unexpected character '{}'",
        input.peek().unwrap()
    );
}

fn consume_while<F>(input: &mut Queue<char>, mut pred: F)
where
    F: FnMut(char) -> bool,
{
    while let Ok(c) = input.peek() {
        if pred(c) {
            input.consume();
        } else {
            break;
        }
    }
}

fn is_keyword(s: &str) -> bool {
    KEYWORDS.contains(&s)
}

fn lex_identifier(input: &mut Queue<char>) -> Token {
    let mut ident = String::new();

    while let Ok(c) = input.peek() {
        if c.is_alphanumeric() || c == '_' {
            ident.push(input.remove().unwrap());
        } else {
            break;
        }
    }

    if is_keyword(&ident) {
        Token::Keyword(ident)
    } else {
        Token::Identifier(ident)
    }
}

fn lex_number(input: &mut Queue<char>) -> Token {
    let mut buf = String::new();

    while let Ok(c) = input.peek() {
        if c.is_ascii_digit() {
            buf.push(input.remove().unwrap());
        } else {
            break;
        }
    }

    if input.is_there(0, '.') {
        input.consume();
        let mut frac = String::new();

        while let Ok(c) = input.peek() {
            if c.is_ascii_digit() {
                frac.push(input.remove().unwrap());
            } else {
                break;
            }
        }

        if frac.is_empty() {
            panic!("Invalid float literal");
        }

        buf.push('.');
        buf.push_str(&frac);

        let value: f64 = buf.parse().unwrap();
        return Token::FloatLiteral64(value);
    }

    let mut is_i64 = false;

    if input.is_there(0, '_') {
        input.consume();
        let mut suffix = String::new();
        while let Ok(c) = input.peek() {
            if c.is_ascii_alphanumeric() {
                suffix.push(input.remove().unwrap());
            } else {
                break;
            }
        }

        match suffix.to_uppercase().as_str() {
            "I64" => is_i64 = true,
            "I32" => {}
            _ => panic!("Unknown integer suffix: {}", suffix),
        }
    }

    if is_i64 {
        Token::IntLiteral64(buf.parse().unwrap())
    } else {
        Token::IntLiteral32(buf.parse().unwrap())
    }
}

fn lex_char_literal(input: &mut Queue<char>) -> Token {
    input.consume();
    let ch = match input.remove() {
        Ok('\\') => lex_escape(input),
        Ok(c) => c,
        Err(_) => panic!("Unterminated char literal"),
    };

    match input.remove() {
        Ok('\'') => Token::CharLiteral(ch),
        Ok(_) => panic!("Char literal must contain exactly one character"),
        Err(_) => panic!("Unterminated char literal"),
    }
}

fn lex_string_literal(input: &mut Queue<char>) -> Token {
    input.consume();
    let mut result = String::new();

    while let Ok(c) = input.peek() {
        match c {
            '"' => {
                input.consume();
                return Token::StringLiteral(result);
            }
            '\\' => {
                input.consume();
                result.push(lex_escape(input));
            }
            '\n' => panic!("Unterminated string literal"),
            _ => {
                result.push(input.remove().unwrap());
            }
        }
    }

    panic!("Unterminated string literal");
}

fn lex_escape(input: &mut Queue<char>) -> char {
    let c = input.remove().unwrap();
    match c {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        '0' => '\0',
        '\\' => '\\',
        '\'' => '\'',
        '"' => '"',
        _ => panic!("Invalid escape sequence: \\{}", c),
    }
}
