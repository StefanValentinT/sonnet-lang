use crate::ast::untyped_ast::*;
use once_cell::sync::Lazy;
use std::collections::HashMap;

pub static BUILTIN_FUNCTIONS: Lazy<HashMap<String, FunDecl>> = Lazy::new(|| {
    let mut map = HashMap::new();

    // putchar : I32 -> Unit
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

    // ref : a -> Ref a
    map.insert(
        "ref".to_string(),
        FunDecl {
            name: "ref".to_string(),
            params: vec![("x".to_string(), Some(Type::TypeVar("a".to_string())))],
            ret_type: Some(Type::Pointer {
                referenced: Box::new(Type::TypeVar("a".to_string())),
            }),
            body: None,
            exec_time: ExecTime::Runtime,
        },
    );

    // deref : Ref a -> a
    map.insert(
        "deref".to_string(),
        FunDecl {
            name: "deref".to_string(),
            params: vec![(
                "p".to_string(),
                Some(Type::Pointer {
                    referenced: Box::new(Type::TypeVar("a".to_string())),
                }),
            )],
            ret_type: Some(Type::TypeVar("a".to_string())),
            body: None,
            exec_time: ExecTime::Runtime,
        },
    );

    // set : Ref a -> a -> Unit
    map.insert(
        "set".to_string(),
        FunDecl {
            name: "set".to_string(),
            params: vec![
                (
                    "p".to_string(),
                    Some(Type::Pointer {
                        referenced: Box::new(Type::TypeVar("a".to_string())),
                    }),
                ),
                ("value".to_string(), Some(Type::TypeVar("a".to_string()))),
            ],
            ret_type: Some(Type::Unit),
            body: None,
            exec_time: ExecTime::Runtime,
        },
    );

    map
});

pub fn is_stdlib_fun(name: &str) -> bool {
    BUILTIN_FUNCTIONS.contains_key(name)
}

pub fn builtin_function_names() -> Vec<String> {
    BUILTIN_FUNCTIONS.keys().cloned().collect()
}

pub fn builtin_function_decls() -> Vec<FunDecl> {
    BUILTIN_FUNCTIONS.values().cloned().collect()
}
