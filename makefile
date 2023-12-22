OUT = target/debug/sq

.PHONY: lint
lint:
	cargo clippy --fix --allow-staged --bin "sq" --tests --broken-code

.PHONY: clean
clean:
	cargo clean

.PHONY: build
build:
	RUSTFLAGS="-C link-arg=-zstack-size=65536" cargo build --release --target=wasm32-unknown-unknown -Zbuild-std=core,compiler_builtins,alloc -Zunstable-options
	mv target/wasm32-unknown-unknown/release/square.wasm .

.PHONY: start
start: build

.PHONY: test 
ifeq ($(shell uname -s), Darwin)
test:
	cargo test # -- --nocapture
else
test:
	cargo test --target=x86_64-unknown-linux-gnu # -- --nocapture
endif
