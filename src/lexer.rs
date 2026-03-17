#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Lexem {
    Identifier(String),
    I32(i32),
    Assign,
    KeyFun,
    KeyIota,
    KeyCase,
    KeyOf,
    KeyNeg,
    KeyDo,
    KeyEnd,
    Underscore,
    Arrow,
    DoubleArrow,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenSquare,
    CloseSquare,
    Comma,
    Dot,
    Semicolon,
    SymBar,
    SymAmp,
    SymMinus,
    SymPlus,
    SymStar,
    SymSlash,
    SymPercent,
    SymEqual,
    SymNotEqual,
    SymGreater,
    SymLess,
    SymLessEqual,
    SymGreaterEqual,
}

pub fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

pub fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

pub fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}

pub fn read_identifier(chars: &[char], start: usize) -> (String, usize) {
    let mut i = start;
    let mut buf = String::new();

    while i < chars.len() && (is_letter(chars[i]) || is_digit(chars[i])) {
        buf.push(chars[i]);
        i += 1;
    }

    (buf, i)
}

pub fn read_number(chars: &[char], start: usize) -> (i32, usize) {
    let mut i = start;
    let mut buf = String::new();

    while i < chars.len() && is_digit(chars[i]) {
        buf.push(chars[i]);
        i += 1;
    }

    let value = buf.parse::<i32>().expect("Invalid integer");
    (value, i)
}

pub fn tokenize(input: &str) -> Vec<Lexem> {
    let mut i = 0;
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();

    while i < chars.len() {
        let c = chars[i];

        if is_whitespace(c) {
            i += 1;
            continue;
        }

        if c == '#' {
            i += 1;
            if i < chars.len() && is_whitespace(chars[i]) {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
                continue;
            } else {
                panic!(
                    "'#' must be followed by whitespace to start a comment at index {}",
                    i - 1
                );
            }
        }

        let next_char = if i + 1 < chars.len() {
            Some(chars[i + 1])
        } else {
            None
        };

        let token = match c {
            '=' => {
                if next_char == Some('=') {
                    i += 1;
                    Lexem::SymEqual
                } else if next_char == Some('>') {
                    i += 1;
                    Lexem::DoubleArrow
                } else {
                    Lexem::Assign
                }
            }

            '!' => {
                if next_char == Some('=') {
                    i += 1;
                    Lexem::SymNotEqual
                } else {
                    panic!("Unknown char: !");
                }
            }

            '-' => {
                if next_char == Some('>') {
                    i += 1;
                    Lexem::Arrow
                } else {
                    Lexem::SymMinus
                }
            }

            '(' => Lexem::OpenParen,
            '_' => Lexem::Underscore,
            ')' => Lexem::CloseParen,
            '{' => Lexem::OpenBrace,
            '}' => Lexem::CloseBrace,
            '[' => Lexem::OpenSquare,
            ']' => Lexem::CloseSquare,
            ',' => Lexem::Comma,
            '.' => Lexem::Dot,
            ';' => Lexem::Semicolon,

            '|' => Lexem::SymBar,
            '&' => Lexem::SymAmp,

            '+' => Lexem::SymPlus,
            '*' => Lexem::SymStar,
            '/' => Lexem::SymSlash,
            '%' => Lexem::SymPercent,

            '<' => {
                if next_char == Some('=') {
                    i += 1;
                    Lexem::SymLessEqual
                } else {
                    Lexem::SymLess
                }
            }

            '>' => {
                if next_char == Some('=') {
                    i += 1;
                    Lexem::SymGreaterEqual
                } else {
                    Lexem::SymGreater
                }
            }

            c if is_letter(c) => {
                let (id, pos) = read_identifier(&chars, i);
                i = pos - 1;

                match id.as_str() {
                    "fun" => Lexem::KeyFun,
                    "iota" => Lexem::KeyIota,
                    "case" => Lexem::KeyCase,
                    "of" => Lexem::KeyOf,
                    "neg" => Lexem::KeyNeg,
                    "do" => Lexem::KeyDo,
                    "end" => Lexem::KeyEnd,
                    _ => Lexem::Identifier(id),
                }
            }

            c if is_digit(c) => {
                let (num, pos) = read_number(&chars, i);
                i = pos - 1;
                Lexem::I32(num)
            }

            _ => panic!("Unknown char: {}", c),
        };

        tokens.push(token);
        i += 1;
    }

    tokens
}
