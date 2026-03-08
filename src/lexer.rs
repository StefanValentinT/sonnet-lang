#[derive(Debug, Clone)]
pub enum Lexem {
    LowerIdentifier(String),
    UpperIdentifier(String),
    I32(i32),
    Assign,
    KeyFun,
    KeyOf,
    Arrow,
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
    SymEqual,
    SymNotEqual,
    SymBigger,
    SymLess,
    SymLessEqual,
    SymBiggerEqual,
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
                    Lexem::SymBiggerEqual
                } else {
                    Lexem::SymBigger
                }
            }

            c if is_letter(c) => {
                let (id, pos) = read_identifier(&chars, i);
                i = pos - 1;

                match id.as_str() {
                    "fun" => Lexem::KeyFun,
                    "of" => Lexem::KeyOf,
                    _ => {
                        let first = id.chars().next().unwrap();
                        if first.is_ascii_uppercase() {
                            Lexem::UpperIdentifier(id)
                        } else {
                            Lexem::LowerIdentifier(id)
                        }
                    }
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
