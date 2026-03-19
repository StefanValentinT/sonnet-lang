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
pub enum TypedTerm {
    Ident(String, Type),
    Lit(Literal, Type),
    Def(TypedPattern, Box<TypedTerm>, Type),
    Fun(TypedPattern, Box<TypedTerm>, Type),
    Bin(BinOp, Box<TypedTerm>, Box<TypedTerm>, Type),
    FieldAccess(Box<TypedTerm>, String, Type),
    CaseOf(Box<TypedTerm>, Vec<(TypedPattern, Term, Type)>),
    RecordVal(Vec<(String, TypedTerm)>, Type),
    App(Box<TypedTerm>, Box<TypedTerm>, Type),
    TypeExpr(Type),
    Compound(Vec<TypedTerm>, Type),
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
    RecordPattern(Vec<Pattern>),
    PatternApp(Box<Term>, Box<Pattern>),
    TypePattern(Type),
    PatternTyped(Box<Pattern>, Type),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypedPattern {
    PatternIdent(String, Type),
    Wildcard(Type),
    PatternLit(Literal, Type),
    RecordPattern(Vec<TypedPattern>, Type),
    PatternApp(Box<Term>, Box<TypedPattern>, Type),
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
