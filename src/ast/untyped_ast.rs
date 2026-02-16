use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum Type {
    I32,
    I64,
    F64,
    Char,

    Unit,

    FunType {
        params: Vec<Option<Type>>,
        ret: Box<Option<Type>>,
    },
    Pointer {
        referenced: Box<Type>,
    },
    Array {
        element_type: Box<Type>,
        size: i32,
    },
    TypeVar(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "I32"),
            Type::I64 => write!(f, "I64"),
            Type::F64 => write!(f, "F64"),
            Type::Char => write!(f, "Char"),
            Type::Unit => write!(f, "Unit"),
            Type::TypeVar(name) => write!(f, "{}", name),
            Type::Pointer { referenced } => write!(f, "Ref {}", referenced),
            Type::Array { element_type, size } => write!(f, "[{}; {}]", element_type, size),
            Type::FunType { params, ret } => {
                write!(f, "(")?;
                for (i, p) in params.iter().enumerate() {
                    if let Some(t) = p {
                        write!(f, "{}", t)?;
                    } else {
                        write!(f, "Unknown")?;
                    }
                    if i != params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {}", ret.clone().unwrap_or(Type::Unit))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExecTime {
    Runtime,
    CompileTime,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Const {
    I32(i32),
    I64(i64),
    F64(f64),
    Char(char),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,

    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

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
