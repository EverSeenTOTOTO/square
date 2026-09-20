//! 预置库：[`PRELUDE`]（src/prelude.sq）在编译期拼接到用户源码前。
//!
//! 为何不是运行时装载：闭包值只携带 ip 下标，全部指令必须位于同一个代码
//! 空间（单一 `Vec<Inst>`，槽位化架构的基本假设）。prelude 单独编译会在
//! 装载函数返回后释放指令数组，全局闭包的 ip 随即悬垂。拼接则天然同域，
//! 且 DELIMITER 段号 / NAMES / 超指令融合都由 emit() 统一处理。

use core::cell::RefCell;

use alloc::format;

use crate::code_frame::Position;
use crate::emit::{emit, EmitContext};
use crate::errors::SquareError;
use crate::parse::parse;
use crate::vm::{RtCx, VM};
use crate::vm_value::Value;

pub const PRELUDE: &str = include_str!("prelude.sq");

/// prelude + 用户源码 合成整程序文本
pub fn prepend(source: &str) -> alloc::string::String {
    format!("{}\n{}", PRELUDE, source)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(code: &str) -> (VM, Value) {
        let full = prepend(code);
        let ast = parse(&full, &mut Position::new()).unwrap();
        let (insts, _) = emit(&full, &ast, &RefCell::new(EmitContext::new())).unwrap();
        let mut vm = VM::new();
        let mut cx = RtCx::test();
        vm.run(&insts, &mut cx).unwrap();
        let v = vm.current_frame().borrow_mut().pop();
        (vm, v)
    }

    #[test]
    fn test_prelude_map_sum_range() {
        // sum(map(x*x, range(5))) = 0+1+4+9+16
        let (_, v) = run("[sum [map /[x] [* x x] [range 5]]]");
        assert_eq!(v, Value::Num(30.0));
    }

    #[test]
    fn test_prelude_filter_fold() {
        // fold(*, 1, filter(odd, range(10))) = 1*3*5*7*9
        let (_, v) = run(
            "[fold /[a b] [* a b] 1 [filter /[x] [== [% x 2] 1] [range 10]]]",
        );
        assert_eq!(v, Value::Num(945.0));
        // 偶数滤含 0：乘积为 0（回归确认 vec 相等是指针同一性，勿用值比较）
        let (_, z) = run("[fold /[a b] [* a b] 1 [filter /[x] [== [% x 2] 0] [range 10]]]");
        assert_eq!(z, Value::Num(0.0));
    }

    #[test]
    fn test_prelude_reverse_append_contains() {
        // Vec 相等为指针同一性，经 join 转字符串做值比较
        let (_, v) = run("[join '' [map str [reverse [append [vec 1 2] [vec 3 4]]]]]");
        assert_eq!(v, Value::Str(alloc::rc::Rc::from("4321")));
        let (_, hit) = run("[contains [vec 'a' 'b'] 'b']");
        assert_eq!(hit, Value::Bool(true));
    }

    #[test]
    fn test_prelude_join_not() {
        let (_, v) = run("[join ', ' [map str [vec 1 2 3]]]");
        assert_eq!(v, Value::Str(alloc::rc::Rc::from("1, 2, 3")));
        let (_, f) = run("[not [contains [vec 1] 2]]");
        assert_eq!(f, Value::Bool(true));
    }

    /// 用户代码可遮蔽 prelude 名字
    #[test]
    fn test_prelude_shadow() {
        let (_, v) = run("[begin [let map /[] 7] [map]]");
        assert_eq!(v, Value::Num(7.0));
    }
}
