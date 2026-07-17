OUT = target/debug/sq

.PHONY: lint
lint:
	cargo +nightly clippy --fix --allow-staged --bin "sq" --tests --broken-code

.PHONY: clean
clean:
	cargo clean

.PHONY: build
build:
	RUSTFLAGS="-C link-arg=-zstack-size=65536" cargo +nightly build --release --target=wasm32-unknown-unknown -Zbuild-std=core,compiler_builtins,alloc -Zunstable-options
	rm -f square.wasm
	cp target/wasm32-unknown-unknown/release/square.wasm .

.PHONY: start
start: build

.PHONY: test
ifeq ($(shell uname -s), Darwin)
test:
	cargo +nightly test # -- --nocapture
else
test:
	cargo +nightly test --target=x86_64-unknown-linux-gnu # -- --nocapture
endif

# 异步运行时的 Node 单测（需先 make build 产出 square.wasm）。
.PHONY: test-js
test-js:
	node --test
