pub enum Term {
    Var(String),
    Lit(Literal),
    Def(Pattern, Box<Term>),
    Fun(Pattern, Box<Term>),
    Con(ConId),
    Bin(BinOp, Box<Term>, Box<Term>),
    FieldAccess(Box<Term>, String),
    CaseOf(Box<Term>, Vec<(Pattern, Term)>),
    RecordVal(Vec<(String, Term)>),
    App(Box<Term>, Box<Term>),
    Labelled(String, Box<Term>),
}

pub enum Type {
    Ident(String),
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
    Fun(Box<Type>, Box<Type>),
    Record(Vec<(String, Type)>),
    Labelled(String, Box<Type>),
    Top,
    Bottom,
    TypeVar(String),
}

pub struct ConId(u64);

pub enum Pattern {
    Var(String),
    Wildcard,
    Record(Vec<(String, Pattern)>),
    DeLabel(String, Box<Pattern>),
    Typed(Box<Pattern>, Type),
}

pub enum Literal {
    I32(i32),
}

pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}
