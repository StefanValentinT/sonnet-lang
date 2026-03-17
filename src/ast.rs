#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    Ident(String),
    Lit(Literal),
    Def(Pattern, Box<Term>),
    Fun(Pattern, Box<Term>),
    Bin(BinOp, Box<Term>, Box<Term>),
    FieldAccess(Box<Term>, String),
    CaseOf(Box<Term>, Vec<(Pattern, Term)>),
    RecordVal(Vec<(String, Term)>),
    App(Box<Term>, Box<Term>),
    Typed(Box<Term>, Type),
    TypeExpr(Type),
    Compound(Vec<Term>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    TypeIdent(String),
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
    Neg(Box<Type>),
    TypeLit(Literal),
    TypeFun(Box<Type>, Box<Type>),
    Record(Vec<(String, Type)>),
    Iota(u64, Box<Type>),
    TypeVar(String),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Pattern {
    PatternIdent(String),
    Wildcard,
    PatternLit(Literal),
    RecordPattern(Vec<Pattern>),
    PatternApp(Box<Term>, Box<Pattern>),
    PatternTyped(Box<Pattern>, Type),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Literal {
    I32(i32),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}
