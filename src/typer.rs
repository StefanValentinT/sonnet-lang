use std::collections::{HashMap, HashSet};

use crate::ast::*;
use std::sync::atomic::{AtomicU64, Ordering};

static GLOBAL_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

fn next_id() -> u64 {
    GLOBAL_ID_COUNTER.fetch_add(1, Ordering::SeqCst)
}

fn typecheck_program(program: Vec<Term>) -> Vec<TypedTerm> {
    reset_global_state();
    let mut scope = base_scope();
    let result = vec![];

    for term in program {
        typed_term = typecheck_top(term, scope);
        scope = updated_scope_after(term, scope, typed_term);
        result.push(typed_term)
    }

    result
}

struct VarState {
    level: i32,
    lower_bounds: Vec<Type>,
    upper_bounds: Vec<Type>,
}

type VarTable = HashMap<String, VarState>;
type SubtypeCache = HashSet<(Type, Type)>;

fn fresh_var(level: i32, vars: &mut VarTable) -> Type {
    let id = next_id();
    let name = format!("a{}", id);

    vars.insert(
        name.clone(),
        VarState {
            level,
            lower_bounds: vec![],
            upper_bounds: vec![],
        },
    );

    Type::TypeVar(name)
}

fn infer
