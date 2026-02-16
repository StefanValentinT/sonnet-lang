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

    map.insert(
        "mem_alloc".to_string(),
        FunDecl {
            name: "mem_alloc".to_string(),
            params: vec![("size".to_string(), Some(Type::TypeVar("a".to_string())))],
            ret_type: Some(Type::Pointer {
                referenced: Box::new(Type::TypeVar("a".to_string())),
            }),
            body: None,
            exec_time: ExecTime::Runtime,
        },
    );

    map
});

pub fn strip_non_std_away(name: &str) -> Option<String> {
    let parts: Vec<&str> = name.split('#').collect();
    if parts.len() >= 3 {
        Some(parts[1].to_string())
    } else {
        None
    }
}

pub fn is_stdlib_fun(name: &str) -> bool {
    if let Some(stripped_name) = strip_non_std_away(name) {
        BUILTIN_FUNCTIONS.contains_key(&stripped_name)
    } else {
        BUILTIN_FUNCTIONS.contains_key(name)
    }
}

pub fn builtin_function_names() -> Vec<String> {
    BUILTIN_FUNCTIONS.keys().cloned().collect()
}

pub fn builtin_function_decls() -> Vec<FunDecl> {
    BUILTIN_FUNCTIONS.values().cloned().collect()
}
