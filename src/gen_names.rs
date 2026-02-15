use std::sync::atomic::AtomicI32;
use std::sync::atomic::Ordering;

static TEMP_VAR_COUNTER: AtomicI32 = AtomicI32::new(0);

fn next_number() -> i32 {
    TEMP_VAR_COUNTER.fetch_add(1, Ordering::SeqCst)
}
pub fn make_temporary() -> String {
    let c = next_number();
    format!("tmp#{c}")
}

pub fn make_cond_else() -> String {
    format!("cond_else#{}", next_number())
}

pub fn make_cond_end() -> String {
    format!("cond_end#{}", next_number())
}

pub fn make_and_false() -> String {
    format!("and_false#{}", next_number())
}

pub fn make_and_end() -> String {
    format!("and_end#{}", next_number())
}

pub fn make_or_true() -> String {
    format!("or_true#{}", next_number())
}

pub fn make_or_end() -> String {
    format!("or_end#{}", next_number())
}
