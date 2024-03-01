#[global_allocator]
pub static TALC: talc::TalckWasm = unsafe { talc::TalckWasm::new_global() };
