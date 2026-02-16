install:
    cargo fmt
    cargo build
    cargo install --path . --force --offline

test:
    cargo fmt
    cargo test -- --nocapture
