use crate::ast::untyped_ast::Type;

#[derive(Debug)]
pub enum TacProgram {
    Program(Vec<TacFuncDef>),
}

#[derive(Debug)]
pub enum TacFuncDef {
    Function {
        name: String,
        params: Vec<(String, Type)>,
        ret_type: Type,
        body: Vec<TacInstruction>,
    },
}

#[derive(Debug)]
pub enum TacInstruction {
    Return(Option<TacVal>),
    Truncate {
        src: TacVal,
        dest: TacVal,
    },
    SignExtend {
        src: TacVal,
        dest: TacVal,
    },
    F64ToI32 {
        src: TacVal,
        dest: TacVal,
    },
    F64ToI64 {
        src: TacVal,
        dest: TacVal,
    },
    I32ToF64 {
        src: TacVal,
        dest: TacVal,
    },
    I64ToF64 {
        src: TacVal,
        dest: TacVal,
    },
    Unary {
        op: TacUnaryOp,
        src: TacVal,
        dest: TacVal,
    },
    Binary {
        op: TacBinaryOp,
        src1: TacVal,
        src2: TacVal,
        dest: TacVal,
    },
    Copy {
        src: TacVal,
        dest: TacVal,
    },
    Jump {
        target: String,
    },
    JumpIfZero {
        condition: TacVal,
        target: String,
    },
    JumpIfNotZero {
        condition: TacVal,
        target: String,
    },
    Label(String),
    FunCall {
        fun_name: String,
        args: Vec<TacVal>,
        dest: TacVal,
    },
    GetAddress {
        src: TacVal,
        dest: TacVal,
    },
    Load {
        src_ptr: TacVal,
        dest: TacVal,
    },
    Store {
        src: TacVal,
        dest_ptr: TacVal,
    },
    AddPtr {
        ptr: TacVal,
        index: TacVal,
        scale: i32,
        dest: TacVal,
    },
    CopyToOffset {
        src: TacVal,
        dest: TacVal,
        offset: i32,
    },
}

#[derive(Debug, Clone)]
pub enum TacVal {
    Constant(TacConst),
    Var(String, Type),
}

pub enum ExpResult {
    PlainOperand(TacVal),
    DereferencedPointer(TacVal),
}

#[derive(Debug, Clone)]
pub enum TacConst {
    I32(i32),
    I64(i64),
    F64(f64),
    Char(char),
}

#[derive(Debug, PartialEq)]
pub enum TacUnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum TacBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}
