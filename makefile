OUT = target/debug/sq

.PHONY: lint
lint:
	cargo clippy --fix --allow-staged

.PHONY: clean
clean:
	cargo clean

.PHONY: build
build:
	RUSTFLAGS="-C link-arg=-zstack-size=65536" cargo build --release
	mv target/wasm32-unknown-unknown/release/sq.wasm .

.PHONY: start
start: build

.PHONY: test 
test:
	cargo test --target=x86_64-unknown-linux-gnu
