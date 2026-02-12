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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(usize);

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
