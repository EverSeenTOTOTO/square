OUT = target/debug/rustdemo

.PHONY: lint
lint:
	cargo clippy --fix --allow-staged

.PHONY: clean
clean:
	cargo clean

.PHONY: start
start:
	cargo run

.PHONY: build
build:
	cargo build

.PHONY: test 
test:
	cargo test

.PHONY: debug
debug: build
	gdb --quiet --args ${OUT}

.PHONY: wasm
wasm:
	rustc src/main.rs --target wasm32-unknown-unknown
