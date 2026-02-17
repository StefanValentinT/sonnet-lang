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
    // TODO: Do this wiht type constrinats, once type cosntriants on type variables are implemented, also change it in typechekcer then.
    map.insert(
        "mem_alloc".to_string(),
        FunDecl {
            name: "mem_alloc".to_string(),
            params: vec![("t".to_string(), Some(Type::Type))],
            ret_type: Some(Type::Pointer {
                referenced: Box::new(Type::Type),
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

pub fn comp_stdlib_fun(name1: &str, name2: &str) -> bool {
    let stripped1 = strip_non_std_away(name1).unwrap_or_else(|| name1.to_string());
    let stripped2 = strip_non_std_away(name2).unwrap_or_else(|| name2.to_string());

    if stripped1 != stripped2 {
        return false;
    }

    is_stdlib_fun(&stripped2)
}

pub fn builtin_function_names() -> Vec<String> {
    BUILTIN_FUNCTIONS.keys().cloned().collect()
}

pub fn builtin_function_decls() -> Vec<FunDecl> {
    BUILTIN_FUNCTIONS.values().cloned().collect()
}
