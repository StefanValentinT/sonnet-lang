install:
    cargo fmt
    cargo build
    cargo install --path . --force

test:
    cargo fmt
    cargo test -- --nocapture
