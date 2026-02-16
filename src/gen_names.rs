use once_cell::sync::Lazy;
use std::sync::Mutex;

pub struct NameSequencer {
    current: Vec<u8>,
}

impl NameSequencer {
    pub fn new() -> Self {
        Self {
            current: vec![b'a' - 1],
        }
    }

    pub fn next(&mut self) -> String {
        let mut i = self.current.len();
        while i > 0 {
            i -= 1;
            if self.current[i] < b'z' {
                self.current[i] += 1;
                return String::from_utf8(self.current.clone()).unwrap();
            } else {
                self.current[i] = b'a';
            }
        }
        self.current.insert(0, b'a');
        String::from_utf8(self.current.clone()).unwrap()
    }

    pub fn reset(&mut self) {
        self.current = vec![b'a' - 1];
    }
}

static GLOBAL_SEQUENCER: Lazy<Mutex<NameSequencer>> =
    Lazy::new(|| Mutex::new(NameSequencer::new()));

pub fn reset_name_gen() {
    GLOBAL_SEQUENCER.lock().unwrap().reset();
}

pub fn new_name() -> String {
    GLOBAL_SEQUENCER.lock().unwrap().next()
}

pub fn make_temporary() -> String {
    format!("t#{}", new_name())
}
pub fn make_cond_else() -> String {
    format!("else#{}", new_name())
}
pub fn make_cond_end() -> String {
    format!("end#{}", new_name())
}
pub fn make_and_false() -> String {
    format!("af#{}", new_name())
}
pub fn make_and_end() -> String {
    format!("ae#{}", new_name())
}
pub fn make_or_true() -> String {
    format!("ot#{}", new_name())
}
pub fn make_or_end() -> String {
    format!("oe#{}", new_name())
}

pub fn make_register() -> String {
    format!("%{}", new_name())
}

pub fn make_ok_label() -> String {
    format!("ok{}", new_name())
}

pub fn make_trap_label() -> String {
    format!("t{}", new_name())
}

pub fn make_cont_label() -> String {
    format!("c{}", new_name())
}
