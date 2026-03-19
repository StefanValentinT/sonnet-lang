use crate::ast::*;
use crate::lexer::Lexem;
use std::sync::atomic::{AtomicU64, Ordering};

static IOTA_COUNTER: AtomicU64 = AtomicU64::new(0);

fn next_iota_id() -> u64 {
    IOTA_COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub fn parse(tokens: Vec<Lexem>) -> Vec<Term> {
    Parser::new(tokens).parse_all()
}

// =============================================================================
// Precedence table, higher = tighter binding
//
//  100  field access         e.f          left
//   95  type annotation      e[T]         left (postfix-like infix)
//   90  function application e a          left (juxtaposition)
//   70  * / %                             left
//   60  + -                               left
//   50  == != < <= > >=                   left
//   10  definition           lhs = rhs    right
//
// Type-level operators (parsed separately but same philosophy):
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

    pub fn parse_all(&mut self) -> Vec<Term> {
        let mut res = Vec::new();
        while self.peek().is_some() {
            res.push(self.parse_term(0));
            if self.peek() == Some(&Lexem::Semicolon) {
                self.next();
            }
        }
        res
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
                    Term::Def(pat, Box::new(rhs))
                }
                Lexem::SymBar => {
                    let rhs = self.parse_term(r_bp);
                    Term::TypeExpr(Type::Union(
                        Box::new(self.term_to_type(lhs)),
                        Box::new(self.term_to_type(rhs)),
                    ))
                }
                Lexem::SymAmp => {
                    let rhs = self.parse_term(r_bp);
                    Term::TypeExpr(Type::Inter(
                        Box::new(self.term_to_type(lhs)),
                        Box::new(self.term_to_type(rhs)),
                    ))
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

            // TypeExpr ( iota <type> )
            Some(Lexem::KeyIota) => {
                let ty = self.parse_type(0);
                Term::TypeExpr(Type::Iota(next_iota_id(), Box::new(ty)))
            }

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

        loop {
            let op = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };

            if op == Lexem::OpenSquare {
                if 95 < min_bp {
                    break;
                }
                self.next();
                let ty = self.parse_type(0);
                self.expect(Lexem::CloseSquare);
                lhs = Pattern::PatternTyped(Box::new(lhs), ty);
                continue;
            }

            break;
        }

        lhs
    }

    fn parse_pattern_prefix(&mut self) -> Pattern {
        match self.peek() {
            Some(Lexem::Underscore) => {
                self.next();
                Pattern::Wildcard
            }

            Some(Lexem::I32(_)) => {
                if let Some(Lexem::I32(n)) = self.next() {
                    Pattern::TypePattern(Type::TypeLit(Literal::I32(n)))
                } else {
                    unreachable!()
                }
            }

            // record pattern: { a, b, ... }
            Some(Lexem::OpenBrace) => {
                self.next();
                self.parse_record_pattern_body()
            }

            // parenthesised — could be (iota T) which becomes a constructor term
            Some(Lexem::OpenParen) => {
                self.next();
                let inner_term = self.parse_term(0);
                self.expect(Lexem::CloseParen);
                // pattern atom -> constructor application
                if self.is_pattern_start() {
                    let arg = self.parse_pattern_bp(91);
                    Pattern::PatternApp(Box::new(inner_term), Box::new(arg))
                } else {
                    // bare parenthesised term used as a pattern (can be a literal or var)
                    self.term_to_pattern(inner_term)
                }
            }

            // identifier: could be a plain variable or a constructor followed by a pat
            Some(Lexem::Identifier(_)) => {
                let id = match self.next() {
                    Some(Lexem::Identifier(s)) => s,
                    _ => unreachable!(),
                };

                if !Self::is_lowercase(&id) {
                    let ty = Type::TypeIdent(id);

                    if self.is_pattern_start() {
                        let arg = self.parse_pattern_bp(91);
                        Pattern::PatternApp(Box::new(Term::TypeExpr(ty)), Box::new(arg))
                    } else {
                        Pattern::TypePattern(ty)
                    }
                } else {
                    let var_term = Term::Ident(id.clone());
                    if self.is_pattern_start() {
                        let arg = self.parse_pattern_bp(91);
                        Pattern::PatternApp(Box::new(var_term), Box::new(arg))
                    } else {
                        Pattern::PatternIdent(id)
                    }
                }
            }
            // iota constructor in pattern: iota T  followed by pat
            Some(Lexem::KeyIota) => {
                self.next();
                let ty = self.parse_type(0);
                let ctor = Term::TypeExpr(Type::Iota(next_iota_id(), Box::new(ty)));
                if self.is_pattern_start() {
                    let arg = self.parse_pattern_bp(91);
                    Pattern::PatternApp(Box::new(ctor), Box::new(arg))
                } else {
                    panic!(
                        "Iota has not been provided a pattern, which is necessary for a case expression."
                    )
                }
            }

            t => panic!("Unexpected token in pattern position: {:?}", t),
        }
    }

    // { a, b, ... }  — opening brace already consumed
    fn parse_record_pattern_body(&mut self) -> Pattern {
        let mut fields = Vec::new();
        while self.peek() != Some(&Lexem::CloseBrace) {
            if self.peek().is_none() {
                panic!("Unexpected EOF in record pattern");
            }
            let name = match self.next() {
                Some(Lexem::Identifier(n)) => n,
                t => panic!("Expected field name in record pattern, got {:?}", t),
            };
            fields.push(Pattern::PatternIdent(name));
            if self.peek() == Some(&Lexem::Comma) {
                self.next();
            }
        }
        self.expect(Lexem::CloseBrace);
        Pattern::RecordPattern(fields)
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
                    Type::TypeIdent(id)
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
            Term::Ident(id) => Pattern::PatternIdent(id),
            Term::Lit(l) => Pattern::TypePattern(Type::TypeLit(l)),
            Term::App(f, arg) => Pattern::PatternApp(f, Box::new(self.term_to_pattern(*arg))),
            Term::Typed(inner, ty) => {
                Pattern::PatternTyped(Box::new(self.term_to_pattern(*inner)), ty)
            }
            Term::RecordVal(fields) => Pattern::RecordPattern(
                fields
                    .into_iter()
                    .map(|(_, v)| self.term_to_pattern(v))
                    .collect(),
            ),
            _ => panic!("Invalid pattern on LHS of definition: {:?}", t),
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
    fn term_to_type(&self, t: Term) -> Type {
        match t {
            Term::Ident(id) => Type::TypeIdent(id),
            Term::TypeExpr(ty) => ty,
            Term::Lit(l) => Type::TypeLit(l),
            _ => panic!("Expected type expression, got {:?}", t),
        }
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
