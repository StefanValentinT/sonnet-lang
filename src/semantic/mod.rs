mod identifier_resolution;
mod loop_labeling;
mod typecheck;
use crate::ast::untyped_ast::Program;

use crate::semantic::{
    identifier_resolution::identifier_resolution_pass, loop_labeling::loop_labeling_pass,
    typecheck::typecheck,
};

pub fn semantic_analysis(program: Program) -> Program {
    let program1 = identifier_resolution_pass(program);
    let program2 = loop_labeling_pass(program1);
    let program3 = typecheck(program2);
    program3
}
