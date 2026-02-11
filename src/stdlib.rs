use crate::ast::ast_type::{ExecTime, Type};
use crate::ast::untyped_ast::*;
use once_cell::sync::Lazy;
use std::collections::HashMap;

pub static BUILTIN_FUNCTIONS: Lazy<HashMap<String, FunDecl>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert(
        "print".to_string(),
        FunDecl {
            name: "print".to_string(),
            params: vec![(
                "s".to_string(),
                Some(Type::Slice {
                    element_type: Box::new(Type::Char),
                }),
            )],
            ret_type: Some(Type::I32),
            body: None,
            exec_time: ExecTime::Runtime,
        },
    );

    map.insert(
        "putchar".to_string(),
        FunDecl {
            name: "putchar".to_string(),
            params: vec![("c".to_string(), Some(Type::I32))],
            ret_type: Some(Type::Unit),
            body: None,
            exec_time: ExecTime::Runtime,
        },
    );

    map.insert(
        "puts".to_string(),
        FunDecl {
            name: "puts".to_string(),
            params: vec![(
                "s".to_string(),
                Some(Type::Slice {
                    element_type: Box::new(Type::Char),
                }),
            )],
            ret_type: Some(Type::I32),
            body: None,
            exec_time: ExecTime::Runtime,
        },
    );

    map
});

pub fn is_stdlib_fun(fun: &FunDecl) -> bool {
    BUILTIN_FUNCTIONS.get(&fun.name).map_or(false, |f| f == fun)
}

pub fn builtin_function_names() -> Vec<String> {
    BUILTIN_FUNCTIONS.keys().cloned().collect()
}

pub fn builtin_function_decls() -> Vec<FunDecl> {
    BUILTIN_FUNCTIONS.values().cloned().collect()
}
