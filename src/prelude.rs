//! 预置库：[`PRELUDE`]（src/prelude.sq）在编译期拼接到用户源码前。
//!
//! 为何不是运行时装载：闭包值只携带 ip 下标，全部指令必须位于同一个代码
//! 空间（单一 `Vec<Inst>`，槽位化架构的基本假设）。prelude 单独编译会在
//! 装载函数返回后释放指令数组，全局闭包的 ip 随即悬垂。拼接则天然同域，
//! 且 DELIMITER 段号 / NAMES / 超指令融合都由 emit() 统一处理。

use alloc::format;

#[cfg(test)]
use core::cell::RefCell;
#[cfg(test)]
use crate::code_frame::Position;
#[cfg(test)]
use crate::emit::{emit, EmitContext};
#[cfg(test)]
use crate::parse::parse;
#[cfg(test)]
use crate::vm::{RtCx, VM};
#[cfg(test)]
use crate::vm_value::Value;

pub const PRELUDE: &str = include_str!("prelude.sq");

/// prelude + 用户源码 合成整程序文本
pub fn prepend(source: &str) -> alloc::string::String {
    format!("{}\n{}", PRELUDE, source)
}

/// 用户代码首条指令的 pc（第一顶层语句的 DELIMITER 位）。source map 条目按
/// cursor（字符下标）定位，prelude 前缀长度即分界；无用户语句时回落指令总数。
/// 供宿主在单步模式下快进 prelude（导出 `user_start()`）。
pub fn user_start_pc(insts: &[crate::vm_insts::Inst], sm: &crate::code_frame::SourceMap) -> usize {
    sm.pc_after_cursor(PRELUDE.chars().count())
        .unwrap_or(insts.len())
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

    /// vec = 参数包恒等（/[...] 变参 + __args 整体引用）；slice = at+splice 派生
    /// （负起点/越界收敛）；str = fold + Display 拼接
    #[test]
    fn test_prelude_vec_slice_str() {
        // Vec 相等为指针同一性，经 join/len/str 做值断言
        let (_, v) = run("[join '' [vec 1 2 3]]");
        assert_eq!(v, Value::Str(alloc::rc::Rc::from("123")));
        let (_, v) = run("[len [vec]]");
        assert_eq!(v, Value::Num(0.0));

        let (_, v) = run("[join '' [slice [vec 1 2 3 4] 1 3]]");
        assert_eq!(v, Value::Str(alloc::rc::Rc::from("23")));
        // end 越界截断；start 越界/负 → 空（旧内建版会 panic）
        let (_, v) = run("[join '' [slice [vec 1 2 3] 2 9]]");
        assert_eq!(v, Value::Str(alloc::rc::Rc::from("3")));
        let (_, v) = run("[len [slice [vec 1 2 3] 5 9]]");
        assert_eq!(v, Value::Num(0.0));
        let (_, v) = run("[len [slice [vec 1 2 3] -2 2]]");
        assert_eq!(v, Value::Num(2.0));

        let (_, v) = run("[str 42 'x' true]");
        assert_eq!(v, Value::Str(alloc::rc::Rc::from("42xtrue")));
        let (_, v) = run("[str]");
        assert_eq!(v, Value::Str(alloc::rc::Rc::from("")));
        let (_, v) = run("[str [vec 1 'a']]");
        assert_eq!(v, Value::Str(alloc::rc::Rc::from("[1, a]")));
    }

    /// user_start 落在用户首语句的 DELIMITER 上；纯空白程序回落指令总数
    #[test]
    fn test_user_start_pc() {
        let full = prepend("[let x 1]\nx");
        let ast = parse(&full, &mut Position::new()).unwrap();
        let (insts, sm) = emit(&full, &ast, &RefCell::new(EmitContext::new())).unwrap();

        let start = user_start_pc(&insts, &sm);
        assert!(start < insts.len());
        assert!(matches!(insts[start], crate::vm_insts::Inst::DELIMITER(_)));

        // 融合收缩后 source_map 条目仍逐条精确落在 DELIMITER 位（回迁正确性）
        for (pc, _) in sm.entries() {
            assert!(matches!(insts[*pc], crate::vm_insts::Inst::DELIMITER(_)));
        }

        // 跑到 user_start 时 prelude 全部执行完毕（`=` 动态定义的全局 map 已就位）
        let mut vm = crate::vm::VM::new();
        let mut cx = crate::vm::RtCx::test();
        while vm.pc < start {
            vm.step(&insts, &mut cx).unwrap();
        }
        assert!(matches!(
            vm.globals.get("map"),
            Some(Value::Function(_))
        ));

        let blank = prepend("  \n");
        let ast = parse(&blank, &mut Position::new()).unwrap();
        let (insts, sm) = emit(&blank, &ast, &RefCell::new(EmitContext::new())).unwrap();
        assert_eq!(user_start_pc(&insts, &sm), insts.len());
    }
}
