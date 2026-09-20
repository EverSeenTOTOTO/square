//! JS FFI：`[js 'path.to.fn' [vec args...]]`。
//!
//! 客机把参数序列化成最小 JSON 写入线性内存，经 `host.js_call` 导入交给宿主；
//! 宿主按点路径在 `globalThis` 上解析函数、展开调用，结果 JSON 写回线性内存并
//! 返回 packed 句柄 `(ptr << 32) | len`。仅支持同步调用；Promise 结果返回
//! `undefined` → nil（异步桥接后续版本）。JSON 为自写的最小子集实现：
//! null/true/false/number/string（`\uXXXX` 仅 BMP）/array/object。

#![cfg(target_family = "wasm")]

use alloc::{format, rc::Rc, string::String, vec::Vec};
use core::cell::RefCell;

use crate::errors::SquareError;
use crate::vm_insts::Inst;
use crate::vm::{ExecResult, RtCx, VM};
use crate::vm_value::{Object, Value};

mod host {
    #[link(wasm_import_module = "host")]
    extern "C" {
        pub fn js_call(
            name_ptr: *const u8,
            name_len: usize,
            args_ptr: *const u8,
            args_len: usize,
        ) -> u64;
        /// await 形态：宿主调用后按结果回调 cb_id——Promise 则 .then/.catch，
        /// 同步值立即回调；客机侧 park 等待
        pub fn js_await_call(
            name_ptr: *const u8,
            name_len: usize,
            args_ptr: *const u8,
            args_len: usize,
            cb_id: u32,
        );
    }
}

/// `js` syscall 入口：参数 (Str 路径, Vec 实参)，结果压栈。
/// 实参中的闭包跨界为回调句柄（宿主换成 JS 函数，调用即 call_cb 唤醒该任务）。
pub fn call_js(
    vm: &mut VM,
    inst: &Inst,
    cx: &RtCx,
    name: &str,
    args: &Rc<RefCell<Vec<Value>>>,
) -> ExecResult {
    let mut json = String::from("[");
    for (i, arg) in args.borrow().iter().enumerate() {
        if i > 0 {
            json.push(',');
        }
        value_to_json(arg, &mut json, cx)
            .map_err(|e| SquareError::InstructionError(e, inst.clone(), vm.pc))?;
    }
    json.push(']');

    let handle =
        unsafe { host::js_call(name.as_ptr(), name.len(), json.as_ptr(), json.len()) };
    if handle == 0 {
        vm.current_frame().borrow_mut().push(Value::Nil);
        return Ok(());
    }

    let ptr = (handle >> 32) as usize;
    let len = (handle & 0xffff_ffff) as usize;
    let bytes = unsafe { core::slice::from_raw_parts(ptr as *const u8, len) };
    let val = json_to_value(bytes).unwrap_or(Value::Nil);
    // 宿主侧 alloc 的结果缓冲，读完归还
    crate::dealloc(ptr as *mut u8, len);

    // 宿主异常以 {__sq_err} 回传 → 语言级错误（try 可捕获）
    if let Value::Obj(obj) = &val {
        if obj.borrow().contains_key("__sq_err") {
            let msg = match obj.borrow().get("__sq_err") {
                Some(Value::Str(s)) => String::from(&**s),
                _ => String::from("host call failed"),
            };
            return Err(SquareError::InstructionError(
                msg,
                inst.clone(),
                vm.pc,
            ));
        }
    }

    vm.current_frame().borrow_mut().push(val);
    Ok(())
}

/// `await` syscall 入口：参数 (Str 路径, Vec 实参)。注册当前任务为回调句柄、
/// 交给宿主（Promise then / 同步值立即回调），park 等待；恢复时 call_cb 投递的
/// 值压到本调用点（Promise 拒绝走 try 的 handle_error 路径）。
pub fn await_js(
    vm: &mut VM,
    inst: &Inst,
    cx: &RtCx,
    name: &str,
    args: &Rc<RefCell<Vec<Value>>>,
) -> ExecResult {
    let mut json = String::from("[");
    for (i, arg) in args.borrow().iter().enumerate() {
        if i > 0 {
            json.push(',');
        }
        value_to_json(arg, &mut json, cx)
            .map_err(|e| SquareError::InstructionError(e, inst.clone(), vm.pc))?;
    }
    json.push(']');

    let cb_id = cx.register(cx.task.clone());
    unsafe {
        host::js_await_call(
            name.as_ptr(),
            name.len(),
            json.as_ptr(),
            json.len(),
            cb_id,
        )
    };
    cx.park_self(vm);
    Ok(())
}

// ── 序列化 ───────────────────────────────────────────────────────────────────

fn value_to_json(v: &Value, out: &mut String, cx: &RtCx) -> Result<(), String> {
    match v {
        Value::Nil => out.push_str("null"),
        Value::Bool(true) => out.push_str("true"),
        Value::Bool(false) => out.push_str("false"),
        Value::Num(n) if n.is_finite() => out.push_str(&format!("{}", n)),
        // NaN/Inf 无 JSON 表示，回落 null
        Value::Num(_) => out.push_str("null"),
        Value::Str(s) => json_escape(s, out),
        // 闭包跨界：注册任务换句柄，宿主替换成 JS 函数（调用即 call_cb）
        Value::Function(_) => {
            let id = crate::runtime::register_closure(v).ok_or_else(|| {
                String::from("js cannot cross non-closure function")
            })?;
            out.push_str(&format!("{{\"__sq_cb\":{}}}", id));
        }
        Value::Vec(vec) => {
            out.push('[');
            for (i, item) in vec.borrow().iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                value_to_json(item, out, cx)?;
            }
            out.push(']');
        }
        Value::Obj(obj) => {
            out.push('{');
            for (i, (k, val)) in obj.borrow().iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                json_escape(k, out);
                out.push(':');
                value_to_json(val, out, cx)?;
            }
            out.push('}');
        }
        Value::UpValue(cell) => value_to_json(&cell.borrow(), out, cx)?,
        _ => {
            return Err(format!(
                "js cannot serialize {} (function/proxy)",
                v.typename()
            ))
        }
    }
    Ok(())
}

fn json_escape(s: &str, out: &mut String) {
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
}

// ── 解析（递归下降，输入为宿主写回的 UTF-8 字节）─────────────────────────────

pub fn json_to_value(s: &[u8]) -> Option<Value> {
    let (v, next) = parse_at(s, 0)?;
    let mut i = skip_ws(s, next);
    if i != s.len() {
        return None;
    }
    Some(v)
}

fn skip_ws(s: &[u8], mut i: usize) -> usize {
    while i < s.len() && matches!(s[i], b' ' | b'\t' | b'\n' | b'\r') {
        i += 1;
    }
    i
}

fn parse_at(s: &[u8], pos: usize) -> Option<(Value, usize)> {
    let i = skip_ws(s, pos);
    let b = *s.get(i)?;
    match b {
        b'n' => lit(s, i, b"null", Value::Nil),
        b't' => lit(s, i, b"true", Value::Bool(true)),
        b'f' => lit(s, i, b"false", Value::Bool(false)),
        b'"' => {
            let (str, next) = parse_string(s, i + 1)?;
            Some((Value::Str(Rc::from(str.as_str())), next))
        }
        b'[' => {
            let mut items = Vec::new();
            let mut j = skip_ws(s, i + 1);
            if s.get(j) == Some(&b']') {
                return Some((Value::Vec(Rc::new(RefCell::new(items))), j + 1));
            }
            loop {
                let (v, next) = parse_at(s, j)?;
                items.push(v);
                j = skip_ws(s, next);
                match s.get(j) {
                    Some(b',') => j += 1,
                    Some(b']') => {
                        return Some((Value::Vec(Rc::new(RefCell::new(items))), j + 1))
                    }
                    _ => return None,
                }
            }
        }
        b'{' => {
            let mut obj = Object::default();
            let mut j = skip_ws(s, i + 1);
            if s.get(j) == Some(&b'}') {
                return Some((Value::Obj(Rc::new(RefCell::new(obj))), j + 1));
            }
            loop {
                j = skip_ws(s, j);
                if s.get(j) != Some(&b'"') {
                    return None;
                }
                let (key, next) = parse_string(s, j + 1)?;
                j = skip_ws(s, next);
                if s.get(j) != Some(&b':') {
                    return None;
                }
                let (val, next) = parse_at(s, j + 1)?;
                obj.insert(Rc::from(key.as_str()), val);
                j = skip_ws(s, next);
                match s.get(j) {
                    Some(b',') => j += 1,
                    Some(b'}') => return Some((Value::Obj(Rc::new(RefCell::new(obj))), j + 1)),
                    _ => return None,
                }
            }
        }
        b'-' | b'0'..=b'9' => {
            let start = i;
            let mut j = i;
            while j < s.len() && matches!(s[j], b'-' | b'+' | b'.' | b'e' | b'E' | b'0'..=b'9') {
                j += 1;
            }
            let text = core::str::from_utf8(&s[start..j]).ok()?;
            let n: f64 = text.parse().ok()?;
            Some((Value::Num(n), j))
        }
        _ => None,
    }
}

fn lit(s: &[u8], i: usize, word: &[u8], v: Value) -> Option<(Value, usize)> {
    if s.get(i..i + word.len())? == word {
        Some((v, i + word.len()))
    } else {
        None
    }
}

fn parse_string(s: &[u8], mut i: usize) -> Option<(String, usize)> {
    let mut out = String::new();
    loop {
        let b = *s.get(i)?;
        match b {
            b'"' => return Some((out, i + 1)),
            b'\\' => {
                i += 1;
                match *s.get(i)? {
                    b'"' => out.push('"'),
                    b'\\' => out.push('\\'),
                    b'/' => out.push('/'),
                    b'n' => out.push('\n'),
                    b't' => out.push('\t'),
                    b'r' => out.push('\r'),
                    b'b' => out.push('\u{8}'),
                    b'f' => out.push('\u{c}'),
                    b'u' => {
                        let hex = core::str::from_utf8(s.get(i + 1..i + 5)?).ok()?;
                        let cp = u32::from_str_radix(hex, 16).ok()?;
                        out.push(char::from_u32(cp)?); // 仅 BMP；代理对留待后续
                        i += 4;
                    }
                    _ => return None,
                }
                i += 1;
            }
            _ => {
                // UTF-8 原样字节：按完整字符推进
                let len = utf8_len(b);
                let chunk = core::str::from_utf8(s.get(i..i + len)?).ok()?;
                out.push_str(chunk);
                i += len;
            }
        }
    }
}

fn utf8_len(b: u8) -> usize {
    if b < 0x80 {
        1
    } else if b >> 5 == 0b110 {
        2
    } else if b >> 4 == 0b1110 {
        3
    } else {
        4
    }
}
