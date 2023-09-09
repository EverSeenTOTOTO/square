use core::fmt;

use lazy_static::lazy_static;
use spin::Mutex;

pub mod wasm {
    mod inner {
        #[link(wasm_import_module = "wasm")]
        extern "C" {
            pub fn get_stack_base() -> usize;
            pub fn get_heap_base() -> usize;
            pub fn get_data_end() -> usize;
        }
    }

    pub fn get_stack_base() -> usize {
        unsafe { inner::get_stack_base() }
    }

    pub fn get_heap_base() -> usize {
        unsafe { inner::get_heap_base() }
    }

    pub fn get_data_end() -> usize {
        unsafe { inner::get_data_end() }
    }
}

pub mod console {
    use core::fmt;

    mod inner {
        #[link(wasm_import_module = "console")]
        extern "C" {
            pub fn write(str: *const u8, len: usize);
        }
    }

    pub struct Writer {}

    impl fmt::Write for Writer {
        fn write_str(&mut self, message: &str) -> fmt::Result {
            unsafe { inner::write(message.as_ptr(), message.len()) };
            Ok(())
        }
    }
}

lazy_static! {
    static ref WRITER: Mutex<console::Writer> = Mutex::new(console::Writer {});
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::externs::extern_write(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}

#[doc(hidden)]
pub fn extern_write(args: fmt::Arguments) {
    use core::fmt::Write;
    WRITER.lock().write_fmt(args).unwrap();
}
