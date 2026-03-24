
#[derive(Debug)]
pub struct Program{
    pub terms: Vec<(Pattern, Term)>,
    pub types: Vec<(String, Type)>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Term {
    Ident(String),
    Lit(Literal),
    VarDef(Pattern, Box<Term>),
    Fun(Pattern, Box<Term>),
    Bin(BinOp, Box<Term>, Box<Term>),
    FieldAccess(Box<Term>, String),
    CaseOf(Box<Term>, Vec<(Pattern, Term)>),
    RecordVal(Vec<(String, Term)>),
    App(Box<Term>, Box<Term>),
    Typed(Box<Term>, Type),
    Compound(Vec<Term>),
}

pub struct TypedProgram{
    terms: Vec<(TypedPattern, TypedTerm)>,
    types: Vec<(String, Type)>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum TypedTerm {
    Ident(String, Type),
    Lit(Literal, Type),
    Def(TypedPattern, Box<TypedTerm>, Type),
    Fun(TypedPattern, Box<TypedTerm>, Type),
    Bin(BinOp, Box<TypedTerm>, Box<TypedTerm>, Type),
    FieldAccess(Box<TypedTerm>, String, Type),
    CaseOf(Box<TypedTerm>, Vec<(TypedPattern, TypedTerm, Type)>, Type),
    RecordVal(Vec<(String, TypedTerm)>, Type),
    App(Box<TypedTerm>, Box<TypedTerm>, Type),
    Compound(Vec<TypedTerm>, Type),
}

impl TypedTerm {
    pub fn ty(&self) -> &Type {
        match self {
            TypedTerm::Ident(_, ty)
            | TypedTerm::Lit(_, ty)
            | TypedTerm::Def(_, _, ty)
            | TypedTerm::Fun(_, _, ty)
            | TypedTerm::Bin(_, _, _, ty)
            | TypedTerm::FieldAccess(_, _, ty)
            | TypedTerm::RecordVal(_, ty)
            | TypedTerm::App(_, _, ty)
            | TypedTerm::Compound(_, ty) => ty,
            TypedTerm::CaseOf(_, _, ty) => ty,
        }
    }
}

pub const N_I32: &str = "I32";

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    TypeIdent(String),
    Top,
    Bot,
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
    Neg(Box<Type>),
    TypeLit(Literal),
    TypeFun(Box<Type>, Box<Type>),
    Record(Vec<(String, Type)>),
    Iota(u64, Box<Type>),
    TypeVar(String),
}

impl Type {
    pub fn bot() -> Self {
        Type::Bot
    }
    pub fn top() -> Self {
        Type::Top
    }

    pub fn unit() -> Self {
        Type::Record(vec![])
    }
    pub fn i32() -> Self {
        Type::TypeIdent(N_I32.to_string())
    }
    pub fn fun(d: Type, c: Type) -> Self {
        Type::TypeFun(Box::new(d), Box::new(c))
    }
    pub fn union(a: Type, b: Type) -> Self {
        Type::Union(Box::new(a), Box::new(b))
    }
    pub fn inter(a: Type, b: Type) -> Self {
        Type::Inter(Box::new(a), Box::new(b))
    }
    pub fn neg(t: Type) -> Self {
        Type::Neg(Box::new(t))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Pattern {
    PatternIdent(String),
    Wildcard,
    RecordPattern(Vec<Pattern>),
    PatternApp(Type, Box<Pattern>),
    TypePattern(Type),
    PatternTyped(Box<Pattern>, Type),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum TypedPattern {
    Wildcard(Type),
    PatternLit(Literal, Type),
    RecordPattern(Vec<TypedPattern>, Type),
    TypePattern(Type),
    PatternApp(Type, Box<TypedPattern>, Type),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Literal {
    I32(i32),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
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

impl BinOp {
    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide | BinOp::Remainder
        )
    }
}