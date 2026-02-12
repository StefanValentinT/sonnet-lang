use crate::ast::ast_type::{BinaryOp, Const, ExecTime, Type, UnaryOp};

#[derive(Debug)]
pub enum Program {
    Program(Vec<FunDecl>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Variable(VarDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: String,
    pub initializer: Initializer,
    pub var_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Initializer {
    InitExpr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunDecl {
    pub name: String,
    pub params: Vec<(String, Option<Type>)>,
    pub body: Option<Block>,
    pub ret_type: Option<Type>,
    pub exec_time: ExecTime,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Block {
    Block(Vec<BlockItem>, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    UnitExpr(Expr),
    D(Decl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub ty: Option<Type>,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Constant(Const),
    Var(String),

    Compound(Box<Block>),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),

    Assign(Box<Expr>, Box<Expr>),

    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),

    FunctionCall(String, Vec<Expr>),

    Dereference(Box<Expr>),
    AddrOf(Box<Expr>),

    ArrayLiteral(Vec<Expr>),
    ArrayIndex(Box<Expr>, Box<Expr>),

    Cast { expr: Box<Expr>, target: Type },
}
