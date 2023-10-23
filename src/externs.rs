use lazy_static::lazy_static;
use spin::Mutex;

pub mod wasm {
    #[link(wasm_import_module = "wasm")]
    extern "C" {
        pub fn get_stack_base() -> usize;
        pub fn get_heap_base() -> usize;
    }
}

pub mod memory {
    use core::fmt;

    mod inner {
        #[link(wasm_import_module = "memory")]
        extern "C" {
            pub fn write(str: *const u8, len: usize);
        }
    }

    // write to memory and read by host
    pub fn write(message: &str) {
        unsafe { inner::write(message.as_ptr(), message.len()) };
    }

    // host write to memory and read by us
    pub fn read<'a>(addr: usize, len: usize) -> &'a str {
        unsafe {
            let slice = core::slice::from_raw_parts(addr as *const u8, len);
            let string = core::str::from_utf8_unchecked(slice);

            return string;
        }
    }

    pub struct Writer {}

    impl fmt::Write for Writer {
        fn write_str(&mut self, message: &str) -> fmt::Result {
            write(message);
            Ok(())
        }
    }
}

pub mod app {
    #[link(wasm_import_module = "app")]
    extern "C" {
        pub fn execute();
    }
}

lazy_static! {
    static ref WRITER: Mutex<memory::Writer> = Mutex::new(memory::Writer {});
}

#[doc(hidden)]
pub fn extern_write(args: core::fmt::Arguments) {
    use core::fmt::Write;
    WRITER.lock().write_fmt(args).unwrap();
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
