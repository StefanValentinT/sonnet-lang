use crate::ast::*;
use crate::lexer::Lexem;
use std::sync::atomic::{AtomicU64, Ordering};

static IOTA_COUNTER: AtomicU64 = AtomicU64::new(0);

fn next_iota_id() -> u64 {
    IOTA_COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub fn parse(tokens: Vec<Lexem>) -> Program {
    Parser::new(tokens).parse_program()
}

// =============================================================================
// Precedence table, higher = tighter binding
//
// Term-level operators:
//  100  field access         e.f          left
//   95  type annotation      e[T]         left (postfix-like infix)
//   90  function application e a          left (juxtaposition)
//   70  * / %                             left
//   60  + -                               left
//   50  == != < <= > >=                   left
//   10  definition           lhs = rhs    right
//
// Type-level operators:
//   80  &   intersection     left
//   70  |   union            left
//   60  ->  function arrow   right
// =============================================================================

pub struct Parser {
    tokens: Vec<Lexem>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Lexem>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Lexem> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<Lexem> {
        let t = self.tokens.get(self.pos).cloned();
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    fn expect(&mut self, expected: Lexem) {
        let got = self.next();
        if got.as_ref() != Some(&expected) {
            panic!("Expected {:?}, got {:?}", expected, got);
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            terms: Vec::new(),
            types: Vec::new(),
        };

        while let Some(tok) = self.peek() {
            match tok {
                Lexem::KeyDef => {
                    self.next(); // consume 'def'
                    let (pat, term) = self.parse_top_level_def();
                    program.terms.push((pat, term));
                }
                Lexem::KeyType => {
                    self.next(); // consume 'type'
                    let (name, ty) = self.parse_top_level_type();
                    program.types.push((name, ty));
                }
                _ => {
                    panic!("Top level item not a definition!")
                }
            }
            if self.peek() == Some(&Lexem::Semicolon) {
                self.next();
            }
        }
        program
    }
    fn parse_top_level_def(&mut self) -> (Pattern, Term) {
        let mut pat = self.parse_pattern();

        self.expect(Lexem::Assign);
        let rhs = self.parse_term(0);
        (pat, rhs)
    }

    fn parse_top_level_type(&mut self) -> (String, Type) {
        let name = match self.next() {
            Some(Lexem::Identifier(n)) => n,
            t => panic!("Expected type name after 'type', got {:?}", t),
        };
        self.expect(Lexem::Assign);
        let ty = self.parse_type(0);
        (name, ty)
    }

    fn parse_term(&mut self, min_bp: u8) -> Term {
        let mut lhs = self.parse_term_prefix();

        loop {
            let op = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };

            if op == Lexem::Dot {
                if 100 < min_bp {
                    break;
                }
                self.next();
                let name = match self.next() {
                    Some(Lexem::Identifier(n)) => n,
                    t => panic!("Expected field name after '.', got {:?}", t),
                };
                lhs = Term::FieldAccess(Box::new(lhs), name);
                continue;
            }

            if op == Lexem::OpenSquare {
                if 95 < min_bp {
                    break;
                }
                self.next();
                let ty = self.parse_type(0);
                self.expect(Lexem::CloseSquare);
                lhs = Term::Typed(Box::new(lhs), ty);
                continue;
            }

            if self.is_term_start(&op) {
                if 90 < min_bp {
                    break;
                }
                let arg = self.parse_term(91);
                lhs = Term::App(Box::new(lhs), Box::new(arg));
                continue;
            }

            let (l_bp, r_bp) = match term_infix_bp(&op) {
                Some(bps) => bps,
                None => break,
            };

            if l_bp < min_bp {
                break;
            }

            self.next();

            lhs = match op {
                Lexem::SymStar => Term::Bin(
                    BinOp::Multiply,
                    Box::new(lhs),
                    Box::new(self.parse_term(r_bp)),
                ),
                Lexem::SymSlash => Term::Bin(
                    BinOp::Divide,
                    Box::new(lhs),
                    Box::new(self.parse_term(r_bp)),
                ),
                Lexem::SymPercent => Term::Bin(
                    BinOp::Remainder,
                    Box::new(lhs),
                    Box::new(self.parse_term(r_bp)),
                ),
                Lexem::SymPlus => {
                    Term::Bin(BinOp::Add, Box::new(lhs), Box::new(self.parse_term(r_bp)))
                }
                Lexem::SymMinus => Term::Bin(
                    BinOp::Subtract,
                    Box::new(lhs),
                    Box::new(self.parse_term(r_bp)),
                ),
                Lexem::SymEqual => {
                    Term::Bin(BinOp::Equal, Box::new(lhs), Box::new(self.parse_term(r_bp)))
                }
                Lexem::SymNotEqual => Term::Bin(
                    BinOp::NotEqual,
                    Box::new(lhs),
                    Box::new(self.parse_term(r_bp)),
                ),
                Lexem::SymLess => {
                    Term::Bin(BinOp::Less, Box::new(lhs), Box::new(self.parse_term(r_bp)))
                }
                Lexem::SymLessEqual => Term::Bin(
                    BinOp::LessEqual,
                    Box::new(lhs),
                    Box::new(self.parse_term(r_bp)),
                ),
                Lexem::SymGreater => Term::Bin(
                    BinOp::Greater,
                    Box::new(lhs),
                    Box::new(self.parse_term(r_bp)),
                ),
                Lexem::SymGreaterEqual => Term::Bin(
                    BinOp::GreaterEqual,
                    Box::new(lhs),
                    Box::new(self.parse_term(r_bp)),
                ),
                Lexem::Assign => {
                    let pat = self.term_to_pattern(lhs);
                    let rhs = self.parse_term(r_bp);
                    Term::VarDef(pat, Box::new(rhs))
                }
                _ => unreachable!(),
            };
        }

        lhs
    }

    fn parse_term_prefix(&mut self) -> Term {
        match self.next() {
            Some(Lexem::I32(n)) => Term::Lit(Literal::I32(n)),
            Some(Lexem::Identifier(id)) => Term::Ident(id),

            Some(Lexem::KeyFun) => {
                let pat = self.parse_pattern();
                self.expect(Lexem::Arrow);
                let body = self.parse_term(0);
                Term::Fun(pat, Box::new(body))
            }

            Some(Lexem::KeyCase) => {
                let scrutinee = self.parse_term(0);
                self.expect(Lexem::KeyOf);
                let mut branches = Vec::new();
                while self.peek().is_some()
                    && !matches!(self.peek(), Some(Lexem::KeyEnd) | Some(Lexem::Semicolon))
                {
                    let pat = self.parse_pattern();
                    self.expect(Lexem::DoubleArrow);
                    let rhs = self.parse_term(0);
                    branches.push((pat, rhs));
                    if self.peek() == Some(&Lexem::Comma) {
                        self.next();
                    }
                }
                Term::CaseOf(Box::new(scrutinee), branches)
            }

            // do e; e; ... end
            Some(Lexem::KeyDo) => {
                let mut stmts = Vec::new();
                while self.peek() != Some(&Lexem::KeyEnd) {
                    if self.peek().is_none() {
                        panic!("Unexpected EOF inside do block");
                    }
                    stmts.push(self.parse_term(0));
                    if self.peek() == Some(&Lexem::Semicolon) {
                        self.next();
                    }
                }
                self.expect(Lexem::KeyEnd);
                Term::Compound(stmts)
            }

            Some(Lexem::OpenParen) => {
                let e = self.parse_term(0);
                self.expect(Lexem::CloseParen);
                e
            }

            // record val: { field = val, ... }
            Some(Lexem::OpenBrace) => self.parse_record_value(),

            t => panic!("Unexpected token in term position: {:?}", t),
        }
    }

    fn parse_record_value(&mut self) -> Term {
        let mut fields = Vec::new();
        while self.peek() != Some(&Lexem::CloseBrace) {
            if self.peek().is_none() {
                panic!("Unexpected EOF in record value");
            }
            let name = match self.next() {
                Some(Lexem::Identifier(n)) => n,
                t => panic!("Expected field name in record, got {:?}", t),
            };
            self.expect(Lexem::Assign);
            let val = self.parse_term(0);
            fields.push((name, val));
            if self.peek() == Some(&Lexem::Comma) {
                self.next();
            }
        }
        self.expect(Lexem::CloseBrace);
        Term::RecordVal(fields)
    }

    fn parse_pattern(&mut self) -> Pattern {
        self.parse_pattern_bp(0)
    }

    fn parse_pattern_bp(&mut self, min_bp: u8) -> Pattern {
        let mut lhs = self.parse_pattern_prefix();
        while let Some(Lexem::OpenSquare) = self.peek() {
            if 95 < min_bp {
                break;
            }
            self.next();
            let ty = self.parse_type(0);
            self.expect(Lexem::CloseSquare);
            lhs = Pattern::PatternTyped(Box::new(lhs), ty);
        }
        lhs
    }

    fn parse_pattern_prefix(&mut self) -> Pattern {
        match self.peek() {
            Some(Lexem::Underscore) => {
                self.next();
                Pattern::Wildcard
            }
            Some(Lexem::I32(n)) => {
                let val = *n;
                self.next();
                Pattern::TypePattern(Type::TypeLit(Literal::I32(val)))
            }

            Some(Lexem::OpenBrace) => {
                self.next();
                let mut fields = Vec::new();
                while self.peek() != Some(&Lexem::CloseBrace) {
                    fields.push(self.parse_pattern());
                    if self.peek() == Some(&Lexem::Comma) {
                        self.next();
                    }
                }
                self.expect(Lexem::CloseBrace);
                Pattern::RecordPattern(fields)
            }

            Some(Lexem::Identifier(id)) => {
                let name = id.clone();
                self.next();
                if !Self::is_lowercase(&name) {
                    let ty = Type::TypeIdent(name);
                    if self.is_pattern_start() {
                        Pattern::PatternApp(ty, Box::new(self.parse_pattern_bp(91)))
                    } else {
                        Pattern::TypePattern(ty)
                    }
                } else {
                    Pattern::PatternIdent(name)
                }
            }

            Some(Lexem::KeyIota) => {
                self.next();
                let ty = self.parse_type(0);
                if self.is_pattern_start() {
                    Pattern::PatternApp(ty, Box::new(self.parse_pattern_bp(91)))
                } else {
                    Pattern::TypePattern(Type::Iota(next_iota_id(), Box::new(ty)))
                }
            }

            Some(Lexem::OpenParen) => {
                self.next();
                let p = self.parse_pattern();
                self.expect(Lexem::CloseParen);
                p
            }

            t => panic!("Unexpected pattern: {:?}", t),
        }
    }

    fn is_pattern_start(&self) -> bool {
        matches!(
            self.peek(),
            Some(Lexem::Underscore)
                | Some(Lexem::Identifier(_))
                | Some(Lexem::I32(_))
                | Some(Lexem::OpenBrace)
                | Some(Lexem::OpenParen)
                | Some(Lexem::KeyIota)
        )
    }

    fn parse_type(&mut self, min_bp: u8) -> Type {
        let mut lhs = self.parse_type_prefix();

        loop {
            let op = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };

            let (l_bp, r_bp) = match type_infix_bp(&op) {
                Some(bps) => bps,
                None => break,
            };

            if l_bp < min_bp {
                break;
            }

            self.next();

            lhs = match op {
                Lexem::SymAmp => Type::Inter(Box::new(lhs), Box::new(self.parse_type(r_bp))),
                Lexem::SymBar => Type::Union(Box::new(lhs), Box::new(self.parse_type(r_bp))),
                Lexem::Arrow => Type::TypeFun(Box::new(lhs), Box::new(self.parse_type(r_bp))),
                _ => unreachable!(),
            };
        }

        lhs
    }

    fn is_lowercase(s: &str) -> bool {
        s.chars().next().map(|c| c.is_lowercase()).unwrap_or(false)
    }

    fn parse_type_prefix(&mut self) -> Type {
        match self.next() {
            Some(Lexem::Identifier(id)) => {
                if Self::is_lowercase(&id) {
                    Type::TypeVar(id)
                } else {
                    if id == "Top".to_string() {
                        Type::Top
                    } else if id == "Bot".to_string() {
                        Type::Bot
                    } else {
                        Type::TypeIdent(id)
                    }
                }
            }
            Some(Lexem::I32(n)) => Type::TypeLit(Literal::I32(n)),
            Some(Lexem::KeyNeg) => Type::Neg(Box::new(self.parse_type(90))),

            Some(Lexem::KeyIota) => {
                let inner = self.parse_type(0);
                Type::Iota(next_iota_id(), Box::new(inner))
            }

            Some(Lexem::OpenParen) => {
                let t = self.parse_type(0);
                self.expect(Lexem::CloseParen);
                t
            }

            // record type: { name[T], name[T], ... } or empty: {}
            Some(Lexem::OpenBrace) => self.parse_record_type_body(),

            t => panic!("Unexpected token in type position: {:?}", t),
        }
    }

    fn parse_record_type_body(&mut self) -> Type {
        let mut fields = Vec::new();
        while self.peek() != Some(&Lexem::CloseBrace) {
            if self.peek().is_none() {
                panic!("Unexpected EOF in record type");
            }
            let name = match self.next() {
                Some(Lexem::Identifier(n)) => n,
                t => panic!("Expected field name in record type, got {:?}", t),
            };
            self.expect(Lexem::OpenSquare);
            let ty = self.parse_type(0);
            self.expect(Lexem::CloseSquare);
            fields.push((name, ty));
            if self.peek() == Some(&Lexem::Comma) {
                self.next();
            }
        }
        self.expect(Lexem::CloseBrace);
        Type::Record(fields)
    }

    fn term_to_pattern(&self, t: Term) -> Pattern {
        match t {
            Term::Ident(id) => {
                if !Self::is_lowercase(&id) {
                    Pattern::TypePattern(Type::TypeIdent(id))
                } else {
                    Pattern::PatternIdent(id)
                }
            }
            Term::Lit(l) => Pattern::TypePattern(Type::TypeLit(l)),
            Term::App(f, arg) => {
                // S Z -> PatternApp(TypeIdent(S), PatternApp(TypeIdent(Z), ...))
                let ctor_ty = match *f {
                    Term::Ident(id) => Type::TypeIdent(id),
                    _ => panic!("Expected constructor name in pattern"),
                };
                Pattern::PatternApp(ctor_ty, Box::new(self.term_to_pattern(*arg)))
            }
            Term::RecordVal(fields) => Pattern::RecordPattern(
                fields
                    .into_iter()
                    .map(|(_, v)| self.term_to_pattern(v))
                    .collect(),
            ),
            Term::Typed(inner, ty) => {
                Pattern::PatternTyped(Box::new(self.term_to_pattern(*inner)), ty)
            }
            _ => panic!("Cannot convert term to pattern: {:?}", t),
        }
    }

    fn is_term_start(&self, t: &Lexem) -> bool {
        matches!(
            t,
            Lexem::I32(_)
                | Lexem::Identifier(_)
                | Lexem::KeyFun
                | Lexem::KeyCase
                | Lexem::KeyDo
                | Lexem::OpenParen
                | Lexem::OpenBrace
                | Lexem::KeyIota
        )
    }
}

/// Left bindingpower and right bindingpower for infix operators.
/// Definition is right-associative if (10, 9).
fn term_infix_bp(tok: &Lexem) -> Option<(u8, u8)> {
    match tok {
        Lexem::SymStar | Lexem::SymSlash | Lexem::SymPercent => Some((70, 71)),
        Lexem::SymPlus | Lexem::SymMinus => Some((60, 61)),
        Lexem::SymEqual
        | Lexem::SymNotEqual
        | Lexem::SymLess
        | Lexem::SymLessEqual
        | Lexem::SymGreater
        | Lexem::SymGreaterEqual => Some((50, 51)),
        Lexem::Assign => Some((10, 9)), // right-associative
        Lexem::SymBar => Some((70, 71)),
        Lexem::SymAmp => Some((80, 81)),
        _ => None,
    }
}

/// Left bindingpower and right bindingpower for type-level infix operators.
fn type_infix_bp(tok: &Lexem) -> Option<(u8, u8)> {
    match tok {
        Lexem::SymAmp => Some((80, 81)), // & intersection, left
        Lexem::SymBar => Some((70, 71)), // | union, left
        Lexem::Arrow => Some((60, 59)),  // -> function, right
        _ => None,
    }
}
