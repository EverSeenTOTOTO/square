# Square Language Project Guidelines

## Build & Test

- This project targets both native and WASM. On Linux, tests must run with:
  ```
  cargo test --target=x86_64-unknown-linux-gnu
  ```
  Check the `makefile` for platform-specific test commands before running tests.

## Conventions

- All tests are inline in source files using `#[test]` attributes.
- The project uses nightly Rust features and `no_std` for WASM targets.
- Avoid useless comments, keep code clean and self-documenting.
