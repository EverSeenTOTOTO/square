OUT = target/debug/sq

.PHONY: lint
lint:
	cargo clippy --fix --allow-staged

.PHONY: clean
clean:
	cargo clean

.PHONY: build
build:
	cargo build --release
	mv target/wasm32-unknown-unknown/release/sq.wasm .

.PHONY: start
start: build

.PHONY: test 
test:
	cargo test
