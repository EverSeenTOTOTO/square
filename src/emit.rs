use core::cell::RefCell;

use crate::{
    builtin::Builtin,
    code_frame::{Position, SourceMap},
    errors::SquareError,
    parse::Node,
    scan::Token,
    vm_insts::Inst,
    vm_value::{CaptureSrc, ClosureInfo, Function, ParamLayout, Value},
};

#[cfg(test)]
use crate::parse::parse;
use hashbrown::HashSet;

use alloc::{boxed::Box, format, rc::Rc, string::String, string::ToString, vec, vec::Vec};
use hashbrown::HashMap;

pub type EmitResult = Result<Vec<Inst>, SquareError>;

/// 名字使用处的静态解析结果
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Binding {
    Local(u16),
    Upvalue(u16),
    /// builtin 或编译期无法归属（`=` 动态定义）：执行期才揭晓
    Global,
}

/// 每个函数（含顶层）的编译上下文：作用域栈（名→槽位）+ upvalue 表。
/// 槽位静态布局，作用域进出只影响编译期可见性，无运行时开销。
struct FnCtx {
    scopes: Vec<HashMap<String, u16>>,
    next_slot: u16,
    /// 本函数闭包的捕获表：(名字, 来源)，下标即 LOAD_UP/STORE_UP 的操作数
    upvalues: Vec<(String, CaptureSrc)>,
    /// 槽位名表（快照/调试）
    names: Vec<String>,
}

impl FnCtx {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            next_slot: 0,
            upvalues: Vec::new(),
            names: Vec::new(),
        }
    }

    fn lookup_local(&self, name: &str) -> Option<u16> {
        self.scopes.iter().rev().find_map(|s| s.get(name).copied())
    }
}

pub struct EmitContext {
    fns: Vec<FnCtx>,
    builtin: Builtin,
    /// 嵌套函数内引用、编译期无处归属的名字（前向引用/方法引用宿主对象）：
    /// 走 Global（全局表），顶层 let 这些名字时双写（STORE_GLOBAL + STORE_LOCAL），
    /// 保住旧运行时"Nil 单元 + 后续 let 写穿"的惰性捕获语义
    deferred: HashSet<String>,
    /// DELIMITER 起始段落号。REPL 把每条语句拼进同一份 session insts 时，用递增的 base
    /// 保证各语句的 DELIMITER 编号全局唯一（与 VM 的 mpc 同步）。默认 0（整程序编译）。
    pub base_mindex: usize,
}

impl EmitContext {
    pub fn new() -> Self {
        Self {
            fns: vec![FnCtx::new()],
            builtin: Builtin::new(),
            deferred: HashSet::new(),
            base_mindex: 0,
        }
    }

    /// 声明局部：当前函数最内层作用域分配槽位（重名/内建名报错，保持旧语义）
    pub fn add_local(&mut self, name: String) -> Result<u16, SquareError> {
        if self.builtin.is_builtin(&name) {
            return Err(SquareError::RuntimeError(format!(
                "redefine of builtin variable {}",
                name
            )));
        }

        let f = self.fns.last_mut().unwrap();
        let scope = f.scopes.last_mut().unwrap();
        if scope.contains_key(&name) {
            return Err(SquareError::RuntimeError(format!(
                "redefine of variable {}",
                name
            )));
        }

        let slot = f.next_slot;
        f.next_slot += 1;
        scope.insert(name.clone(), slot);
        if f.names.len() <= slot as usize {
            f.names.resize(slot as usize + 1, String::new());
        }
        f.names[slot as usize] = name;
        Ok(slot)
    }

    /// 使用处解析：当前函数作用域 → 本函数已有 upvalue → 外层函数链（逐层注册
    /// 传递捕获）→ Global。
    fn resolve(&mut self, name: &str) -> Binding {
        if self.builtin.is_builtin(name) {
            return Binding::Global;
        }

        let last = self.fns.len() - 1;
        if let Some(slot) = self.fns[last].lookup_local(name) {
            return Binding::Local(slot);
        }
        if let Some(idx) = self.fns[last]
            .upvalues
            .iter()
            .position(|(n, _)| n == name)
        {
            return Binding::Upvalue(idx as u16);
        }

        // 找定义者：最内层的外层函数，名字或是其局部槽位、或是其 upvalue
        let mut owner = None;
        for i in (0..last).rev() {
            if let Some(slot) = self.fns[i].lookup_local(name) {
                owner = Some((i, CaptureSrc::Local(slot)));
                break;
            }
            if let Some(idx) = self.fns[i]
                .upvalues
                .iter()
                .position(|(n, _)| n == name)
            {
                owner = Some((i, CaptureSrc::Upvalue(idx as u16)));
                break;
            }
        }

        let Some((owner, mut src)) = owner else {
            // 嵌套函数内的前向引用（定义在使用之后）：走全局表，顶层 let 双写接住
            if self.fns.len() > 1 {
                self.deferred.insert(name.to_string());
            }
            // `this`：方法体引用宿主对象，编译期注册专用捕获源，
            // 闭包存入 obj 时由 set/obj 内建回填（try_capture_this）
            if name == "this" {
                let f = self.fns.last_mut().unwrap();
                if let Some(idx) = f.upvalues.iter().position(|(n, _)| n == name) {
                    return Binding::Upvalue(idx as u16);
                }
                f.upvalues.push((name.to_string(), CaptureSrc::This));
                return Binding::Upvalue((f.upvalues.len() - 1) as u16);
            }
            return Binding::Global;
        };

        // 从定义者向内逐层注册捕获（含当前函数），链到自己的 upvalue 下标
        for j in owner + 1..=last {
            if let Some(idx) = self.fns[j]
                .upvalues
                .iter()
                .position(|(n, _)| n == name)
            {
                src = CaptureSrc::Upvalue(idx as u16);
            } else {
                self.fns[j].upvalues.push((name.to_string(), src));
                src = CaptureSrc::Upvalue((self.fns[j].upvalues.len() - 1) as u16);
            }
        }
        match src {
            CaptureSrc::Upvalue(idx) => Binding::Upvalue(idx),
            _ => unreachable!(),
        }
    }

    pub fn push_scope(&mut self) {
        self.fns.last_mut().unwrap().scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.fns.last_mut().unwrap().scopes.pop();
    }

    fn push_fn(&mut self) {
        self.fns.push(FnCtx::new());
    }

    fn pop_fn(&mut self) -> FnCtx {
        self.fns.pop().unwrap()
    }

    /// 顶层（根帧）槽位名表
    fn root_names(&self) -> Vec<String> {
        self.fns[0].names.clone()
    }

    /// 是否处于顶层裸作用域（非嵌套函数、非块作用域）——延迟名字双写的生效条件
    fn in_root_plain_scope(&self) -> bool {
        self.fns.len() == 1 && self.fns[0].scopes.len() == 1
    }
}

/// 名字读取 → 指令
fn load_inst(ctx: &RefCell<EmitContext>, name: &str) -> Inst {
    match ctx.borrow_mut().resolve(name) {
        Binding::Local(i) => Inst::LOAD_LOCAL(i),
        Binding::Upvalue(i) => Inst::LOAD_UP(i),
        Binding::Global => Inst::LOAD_GLOBAL(name.to_string()),
    }
}

/// 名字写入（`= x v`）→ 指令；未声明名字落到全局表（保持旧 `=` 即定义的语义）
fn store_inst(ctx: &RefCell<EmitContext>, name: &str) -> Inst {
    match ctx.borrow_mut().resolve(name) {
        Binding::Local(i) => Inst::STORE_LOCAL(i),
        Binding::Upvalue(i) => Inst::STORE_UP(i),
        Binding::Global => Inst::STORE_GLOBAL(name.to_string()),
    }
}

fn unescape(s: &str) -> String {
    let mut unescaped = String::new();

    if s.is_empty() {
        return unescaped;
    }

    let mut chars = s.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('\'') => unescaped.push('\''),
                Some('\\') => unescaped.push('\\'),
                Some('n') => unescaped.push('\n'),
                Some('r') => unescaped.push('\r'),
                Some('t') => unescaped.push('\t'),
                Some('0') => unescaped.push('\0'),
                Some(c) => {
                    unescaped.push('\\');
                    unescaped.push(c);
                }
                None => unescaped.push('\\'),
            }
        } else {
            unescaped.push(ch);
        }
    }
    unescaped
}

fn emit_token(input: &str, token: &Token, ctx: &RefCell<EmitContext>) -> EmitResult {
    match token {
        Token::Num(_, num) => Ok(vec![Inst::PUSH(Value::Num(num.parse::<f64>().unwrap()))]),
        Token::Str(_, s) => {
            let val = Value::Str(Rc::from(unescape(s).as_str()));
            Ok(vec![Inst::PUSH(val)])
        }
        Token::Id(_, id) => {
            Ok(vec![load_inst(ctx, id)])
        }
        _ => {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                format!(
                    "failed to emit_token, expect number, string or identifier name, got {}",
                    token
                ),
                token.pos().clone(),
                None,
            ));
        }
    }
}

/// 测试断言用的 ClosureMeta 构造
#[cfg(test)]
fn closure_meta(
    offset: i32,
    n_slots: u16,
    captures: Vec<CaptureSrc>,
    params: ParamLayout,
    names: &[&str],
) -> Function {
    Function::ClosureMeta(Rc::new(ClosureInfo {
        offset,
        n_slots,
        captures,
        params,
        names: Rc::new(names.iter().map(|s| s.to_string()).collect()),
    }))
}

#[test]
fn test_emit_token_num() {
    let code = "42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(insts, vec![Inst::PUSH(Value::Num(42.0))]);
}

#[test]
fn test_emit_token_str() {
    let code = "'42'";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(insts, vec![Inst::PUSH(Value::Str(Rc::from("42")))]);
}

#[test]
fn test_emit_token_lit() {
    let code = "nil";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![Inst::LOAD_GLOBAL("nil".to_string())]
    );
}

#[test]
fn test_emit_token_id() {
    let code = "a";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![Inst::LOAD_GLOBAL("a".to_string())]
    );
}

fn emit_assign(
    input: &str,
    eq: &Token,
    target: &Box<Node>,
    properties: &Vec<Box<Node>>,
    expression: &Box<Node>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let mut result = vec![];
    let is_define = eq.source() == "let";
    let declare = |ctx: &RefCell<EmitContext>, name: &str, pos: &Position| -> Result<u16, SquareError> {
        ctx.borrow_mut()
            .add_local(name.to_string())
            .map_err(|e| match e {
                SquareError::RuntimeError(msg) => {
                    SquareError::SyntaxError(input.to_string(), msg, pos.clone(), None)
                }
                _ => e,
            })
    };

    match target.as_ref() {
        Node::Token(id) => {
            if let Token::Id(_, source) = id {
                if !properties.is_empty() {
                    // o.p1…pN = value  →  <o.p1…p(N-1)> value SET pN
                    let keys: Vec<String> = properties
                        .iter()
                        .map(|n| match n.as_ref() {
                            Node::Prop(_, Token::Id(_, id)) => id.clone(),
                            _ => unreachable!(),
                        })
                        .collect();
                    let (last_key, rest_keys) = keys.split_last().unwrap();
                    result.extend(emit_get_chain(input, target, rest_keys, ctx)?);
                    result.extend(emit_node(input, expression, ctx)?);
                    result.push(Inst::SET(last_key.clone()));
                } else if is_define {
                    // let x v：函数字面量先声明再求值（letrec 自引用，`[let fib /[n] ...]`）；
                    // 其余值先行（`[let x [+ x 1]]` 的 RHS 读外层 x）
                    let is_fn_value = matches!(expression.as_ref(), Node::Fn(..));
                    let early = if is_fn_value {
                        Some(declare(ctx, source, id.pos())?)
                    } else {
                        None
                    };
                    result.extend(emit_node(input, expression, ctx)?);
                    let slot = match early {
                        Some(s) => s,
                        None => declare(ctx, source, id.pos())?,
                    };
                    // 前向引用过的名字：顶层 let 同步发布到全局表，接住闭包内的
                    // LOAD_GLOBAL（块作用域内的 let 不发布，保持块级封闭）
                    if ctx.borrow().deferred.contains(source.as_str())
                        && ctx.borrow().in_root_plain_scope()
                    {
                        result.push(Inst::STORE_GLOBAL(source.clone()));
                    }
                    result.push(Inst::STORE_LOCAL(slot));
                } else {
                    // = x v
                    result.extend(emit_node(input, expression, ctx)?);
                    result.push(store_inst(ctx, source));
                }
            } else {
                return Err(SquareError::SyntaxError(
                    input.to_string(),
                    format!(
                        "failed to emit_assign, cannot assign to {}, expect identifier",
                        id
                    ),
                    id.pos().clone(),
                    None,
                ));
            }
        }
        Node::Expand(.., placeholders) => {
            result.extend(emit_node(input, expression, ctx)?);
            result.extend(emit_expand(input, is_define, placeholders, ctx)?);
        }
        _ => unreachable!(),
    }

    Ok(result)
}

#[test]
fn test_emit_assign() {
    let code = "[= a 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::PUSH(Value::Num(42.0)),
            Inst::STORE_GLOBAL("a".to_string()),
        ]
    );
}

#[test]
fn test_emit_assign_dot() {
    let code = "[= o.x.y 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    // o.x 42 SET y
    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("o".to_string()),
            Inst::GET("x".to_string()),
            Inst::PUSH(Value::Num(42.0)),
            Inst::SET("y".to_string()),
        ]
    );
}

#[test]
fn test_emit_define() {
    let code = "[let a 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::PUSH(Value::Num(42.0)),
            Inst::STORE_LOCAL(0),
        ]
    );
}

#[test]
fn test_emit_redefine() {
    let code = "[let a 42] [let a 24]";
    let ast = parse(code, &mut Position::new()).unwrap();

    assert_eq!(
        emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())),
        Err(SquareError::SyntaxError(
            code.to_string(),
            "redefine of variable a".to_string(),
            Position {
                line: 1,
                column: 17,
                cursor: 16
            },
            None
        ))
    );
}

fn emit_op(
    input: &str,
    op: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.len() < 2 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_op, operands size not match".to_string(),
            op.pos().clone(),
            None,
        ));
    }
    let op_action = |action: Inst| {
        let mut result = emit_multi_node(input, &expressions[0..2].to_vec(), ctx)?;
        result.push(action);
        Ok(result)
    };
    let op_assign_action = |action: Inst| {
        let mut result = vec![];
        let mut properties = vec![];
        let mut rhs = vec![];

        for node in expressions[1..].iter() {
            if let Node::Prop(_, Token::Id(_, id)) = node.as_ref() {
                properties.push(id.clone());
            } else {
                let insts = emit_node(input, node, ctx)?;
                rhs.extend(insts);
                break;
            }
        }

        if rhs.is_empty() {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                "failed to emit_op, expect value after property".to_string(),
                op.pos().clone(),
                None,
            ));
        }

        if let Node::Token(Token::Id(_, source)) = expressions[0].as_ref() {
            if !properties.is_empty() {
                // o.p1…pN op= rhs  →  <o.p1…p(N-1)> (<o.p1…pN> op rhs) SET pN
                let (last_key, rest_keys) = properties.split_last().unwrap();
                result.extend(emit_get_chain(input, &expressions[0], rest_keys, ctx)?);
                result.extend(emit_get_chain(input, &expressions[0], &properties, ctx)?);
                result.extend(rhs);
                result.push(action);
                result.push(Inst::SET(last_key.clone()));
            } else {
                let load = load_inst(ctx, source);
                result.push(load);
                result.extend(rhs);
                result.push(action);
                result.push(store_inst(ctx, source));
            }

            Ok(result)
        } else {
            unreachable!()
        }
    };

    if let Token::Op(_, source) = op {
        return match source.as_str() {
            "+" => op_action(Inst::ADD),
            "-" => op_action(Inst::SUB),
            "*" => op_action(Inst::MUL),
            "/" => op_action(Inst::DIV),
            "%" => op_action(Inst::REM),
            "&" => op_action(Inst::BITAND),
            "|" => op_action(Inst::BITOR),
            "^" => op_action(Inst::BITXOR),
            "~" => op_action(Inst::BITNOT),
            "==" => op_action(Inst::EQ),
            "!=" => op_action(Inst::NE),
            "<" => op_action(Inst::LT),
            "<=" => op_action(Inst::LE),
            ">" => op_action(Inst::GT),
            ">=" => op_action(Inst::GE),
            ">>" => op_action(Inst::SHR),
            "<<" => op_action(Inst::SHL),
            ">>=" => op_assign_action(Inst::SHR),
            "<<=" => op_assign_action(Inst::SHL),
            "+=" => op_assign_action(Inst::ADD),
            "-=" => op_assign_action(Inst::SUB),
            "*=" => op_assign_action(Inst::MUL),
            "/=" => op_assign_action(Inst::DIV),
            "%=" => op_assign_action(Inst::REM),
            "&=" => op_assign_action(Inst::BITAND),
            "|=" => op_assign_action(Inst::BITOR),
            "^=" => op_assign_action(Inst::BITXOR),
            _ => todo!(),
        };
    }

    return Err(SquareError::SyntaxError(
        input.to_string(),
        format!("failed to emit_op, expect operator, got {}", op),
        op.pos().clone(),
        None,
    ));
}

#[test]
fn test_emit_op() {
    let code = "[+ a b]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("a".to_string()),
            Inst::LOAD_GLOBAL("b".to_string()),
            Inst::ADD,
        ]
    );
}

#[test]
fn test_emit_op_assign() {
    let code = "[+= a b]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("a".to_string()),
            Inst::LOAD_GLOBAL("b".to_string()),
            Inst::ADD,
            Inst::STORE_GLOBAL("a".to_string()),
        ]
    );
}

#[test]
fn test_emit_op_assign_dot() {
    let code = "[+= a.b.c d]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    // a.b (a.b.c + d) SET c
    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("a".to_string()),
            Inst::GET("b".to_string()),
            Inst::LOAD_GLOBAL("a".to_string()),
            Inst::GET("b".to_string()),
            Inst::GET("c".to_string()),
            Inst::LOAD_GLOBAL("d".to_string()),
            Inst::ADD,
            Inst::SET("c".to_string()),
        ]
    );
}

fn emit_if(
    input: &str,
    if_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.len() < 3 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_if, expect at least condition and true_branch".to_string(),
            if_token.pos().clone(),
            None,
        ));
    }

    let mut result = vec![];
    let condition = expressions.get(1).unwrap();
    ctx.borrow_mut().push_scope();
    let condition_result = emit_node(input, condition, ctx)?;
    result.extend(condition_result);

    let true_branch = expressions.get(2).unwrap();
    let true_branch_result = emit_node(input, true_branch, ctx)?;
    let true_branch_len = true_branch_result.len() as i32;
    // 为假跳过真分支，+1 抵随后的 JMP
    result.push(Inst::JNE(true_branch_len + 1));
    result.extend(true_branch_result);

    let false_branch_result = if let Some(false_branch) = expressions.get(3) {
        emit_node(input, false_branch, ctx)?
    } else {
        vec![Inst::PUSH(Value::Nil)] // FIXME: else { nil }
    };
    ctx.borrow_mut().pop_scope();

    let false_branch_len = false_branch_result.len() as i32;
    // 为真跳过假分支
    result.push(Inst::JMP(false_branch_len));
    result.extend(false_branch_result);

    Ok(result)
}

#[test]
fn test_emit_if_true() {
    let code = "[if true 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("true".to_string()),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::JMP(1),
            Inst::PUSH(Value::Nil),
        ]
    );
}

#[test]
fn test_emit_if_true_false() {
    let code = "[if true 42 24]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("true".to_string()),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::JMP(1),
            Inst::PUSH(Value::Num(24.0)),
        ]
    );
}

#[test]
fn test_emit_if_condition_scope() {
    let code = "[if [let x 42] x]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::PUSH(Value::Num(42.0)),
            Inst::STORE_LOCAL(0),
            Inst::JNE(2),
            Inst::LOAD_LOCAL(0),
            Inst::JMP(1),
            Inst::PUSH(Value::Nil),
        ]
    );
}

fn emit_while(
    input: &str,
    while_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.len() < 3 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_while, expect condition and body".to_string(),
            while_token.pos().clone(),
            None,
        ));
    }

    let condition = expressions.get(1).unwrap();
    ctx.borrow_mut().push_scope();
    let condition_result = emit_node(input, condition, ctx)?;
    let condition_len = condition_result.len() as i32;
    let body = &expressions[2..].to_vec();

    let body_result = emit_multi_node(input, body, ctx)?;
    ctx.borrow_mut().pop_scope();
    let body_len = body_result.len() as i32;

    // [cond, JNE, body, JMP-back]：退出时栈上遗留与旧 thunk 实现一致（留在本帧）
    let mut result = condition_result;
    result.push(Inst::JNE(body_len + 1)); // 为假跳出循环
    result.extend(body_result);
    result.push(Inst::JMP(-(condition_len + body_len + 2))); // 回到条件
    Ok(result)
}

#[test]
fn test_emit_while() {
    let code = "[while true 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("true".to_string()),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::JMP(-4),
        ]
    );
}

#[test]
fn test_emit_while_condition_scope() {
    let code = "[while [let x false] x]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("false".to_string()),
            Inst::STORE_LOCAL(0),
            Inst::JNE(2),
            Inst::LOAD_LOCAL(0),
            Inst::JMP(-5),
        ]
    );
}

fn emit_begin(
    input: &str,
    _begin_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    ctx.borrow_mut().push_scope();
    let body = emit_multi_node(input, &expressions[1..].to_vec(), ctx)?;
    ctx.borrow_mut().pop_scope();

    // 纯顺序执行：各表达式值全部留在栈上，begin 的值 = 栈顶（等价旧 thunk 的 RET top）。
    // 不逐个 POP——部分表达式（如 print）不压栈，无法静态计数
    Ok(body)
}

#[test]
fn test_emit_begin() {
    let code = "[begin 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![Inst::PUSH(Value::Num(42.0))]
    );
}

fn emit_cond(
    input: &str,
    match_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.is_empty() {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_cond, expect patterns".to_string(),
            match_token.pos().clone(),
            None,
        ));
    }

    ctx.borrow_mut().push_scope();
    let mut result = vec![];
    let mut jne_sites: Vec<usize> = vec![]; // 各 pattern 的 JNE 占位（跳到下一 pattern）
    let mut end_sites: Vec<usize> = vec![]; // 各 action 后的 JMP 占位（跳到结尾）

    for node in expressions[1..].to_vec().iter() {
        if let Node::Call(left_bracket, _, ref exprs) = node.as_ref() {
            if exprs.len() != 2 {
                return Err(SquareError::SyntaxError(
                    input.to_string(),
                    "failed to emit_cond, expect pattern and action".to_string(),
                    left_bracket.pos().clone(),
                    None,
                ));
            }

            result.extend(emit_node(input, &exprs[0], ctx)?); // pattern
            jne_sites.push(result.len());
            result.push(Inst::JNE(0));
            result.extend(emit_node(input, &exprs[1], ctx)?); // action
            end_sites.push(result.len());
            result.push(Inst::JMP(0));
        } else {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                "failed to emit_cond, expect call expression".to_string(),
                match_token.pos().clone(),
                None,
            ));
        }
    }
    result.push(Inst::PUSH(Value::Nil)); // 全不匹配 → nil
    ctx.borrow_mut().pop_scope();

    let end = result.len() as i32;
    for k in 0..jne_sites.len() {
        let site = jne_sites[k];
        result[site] = Inst::JNE((end_sites[k] + 1) as i32 - (site as i32 + 1));
    }
    for site in end_sites {
        result[site] = Inst::JMP(end - (site as i32 + 1));
    }

    Ok(result)
}

#[test]
fn test_emit_cond() {
    let code = "[cond
        [[let x false] 42]
        [true x]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("false".to_string()),
            Inst::STORE_LOCAL(0),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::JMP(5),
            Inst::LOAD_GLOBAL("true".to_string()),
            Inst::JNE(2),
            Inst::LOAD_LOCAL(0),
            Inst::JMP(1),
            Inst::PUSH(Value::Nil),
        ]
    );
}

fn emit_call(
    input: &str,
    left_bracket: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.is_empty() {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_call, expect function name".to_string(),
            left_bracket.pos().clone(),
            None,
        ));
    }

    let first = expressions.first().unwrap();
    let mut result = vec![];

    match first.as_ref() {
        Node::Token(id) => match id {
            Token::Id(_, name) => match name.as_str() {
                // build-in function with speciall syntax
                "if" => result.extend(emit_if(input, id, expressions, ctx)?),
                "while" => result.extend(emit_while(input, id, expressions, ctx)?),
                "begin" => result.extend(emit_begin(input, id, expressions, ctx)?),
                "cond" => result.extend(emit_cond(input, id, expressions, ctx)?),
                _ => {
                    // normal function call
                    result.push(load_inst(ctx, name));
                    result.extend(emit_multi_node(input, &expressions[1..].to_vec(), ctx)?); // provided params
                    result.push(Inst::CALL(expressions.len() as u16 - 1));
                }
            },
            _ => {
                return Err(SquareError::SyntaxError(
                    input.to_string(),
                    format!("failed to emit_call, expect function name, got {}", id),
                    id.pos().clone(),
                    None,
                ));
            }
        },
        Node::Fn(_, params, body) => {
            result.extend(emit_fn(input, params, body, ctx)?);
            result.extend(emit_multi_node(input, &expressions[1..].to_vec(), ctx)?);
            result.push(Inst::CALL(expressions.len() as u16 - 1));
        }
        Node::Op(op, body) => result.extend(emit_op(input, op, body, ctx)?),
        Node::Assign(eq, expansion, dot, body) => {
            result.extend(emit_assign(input, eq, expansion, dot, body, ctx)?)
        }
        Node::Call(left_bracket, _, exprs) => {
            result.extend(emit_call(input, left_bracket, exprs, ctx)?);
            result.extend(emit_multi_node(input, &expressions[1..].to_vec(), ctx)?);
            result.push(Inst::CALL(expressions.len() as u16 - 1));
        }
        Node::Dot(obj, props) => {
            result.extend(emit_dot(input, obj, props, ctx)?);
            result.extend(emit_multi_node(input, &expressions[1..].to_vec(), ctx)?);
            result.push(Inst::CALL(expressions.len() as u16 - 1));
        }
        _ => {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                format!("failed to emit_call, expect function name, got {}", first),
                left_bracket.pos().clone(),
                None,
            ));
        }
    }

    Ok(result)
}

#[test]
fn test_emit_call_no_params() {
    let code = "[foo]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(insts, vec![Inst::LOAD_GLOBAL("foo".to_string()), Inst::CALL(0)]);
}

#[test]
fn test_emit_call_with_params() {
    let code = "[foo bar]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("foo".to_string()),
            Inst::LOAD_GLOBAL("bar".to_string()),
            Inst::CALL(1),
        ]
    );
}

fn emit_fn(
    input: &str,
    params: &Box<Node>,
    body: &Box<Node>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let Node::Expand(.., placeholders) = params.as_ref() else {
        unreachable!()
    };

    ctx.borrow_mut().push_fn();

    // 定参直拷槽位（无入口指令）；含 . / ... / 嵌套展开的参数走 Pack 槽位 + PEEK 绑定
    let simple = placeholders
        .iter()
        .all(|p| matches!(p.as_ref(), Node::Token(Token::Id(_, _))));

    let (layout, params_result): (ParamLayout, Vec<Inst>) = if simple {
        let mut slots = Vec::new();
        for p in placeholders.iter() {
            let Node::Token(Token::Id(pos, id)) = p.as_ref() else {
                unreachable!()
            };
            slots.push(
                ctx.borrow_mut()
                    .add_local(id.clone())
                    .map_err(|e| match e {
                        SquareError::RuntimeError(msg) => SquareError::SyntaxError(
                            input.to_string(),
                            msg,
                            pos.clone(),
                            None,
                        ),
                        _ => e,
                    })?,
            );
        }
        (ParamLayout::Fixed(slots), vec![])
    } else {
        let pack_slot = ctx.borrow_mut().add_local("__args".to_string()).unwrap();
        // 参数包载回栈顶复用 PEEK 绑定（emit_expand 末尾自带 drop pack 的 POP）
        let mut result = vec![Inst::LOAD_LOCAL(pack_slot)];
        result.extend(emit_expand(input, true, placeholders, ctx)?);
        (ParamLayout::Pack(pack_slot), result)
    };

    let body_result = emit_node(input, body, ctx)?;
    let fnctx = ctx.borrow_mut().pop_fn();
    let offset = (params_result.len() + body_result.len()) as i32;

    let info = Rc::new(ClosureInfo {
        offset: -(offset + 2),
        n_slots: fnctx.next_slot,
        captures: fnctx.upvalues.iter().map(|(_, src)| src.clone()).collect(),
        params: layout,
        names: Rc::new(fnctx.names),
    });

    let mut result = vec![Inst::JMP(offset + 1)];
    result.extend(params_result);
    result.extend(body_result);
    result.push(Inst::RET);
    result.push(Inst::PUSH_CLOSURE(Function::ClosureMeta(info)));

    Ok(result)
}

#[test]
fn test_emit_fn() {
    let code = "/[] 42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::RET,
            Inst::PUSH_CLOSURE(closure_meta(-3, 0, vec![], ParamLayout::Fixed(vec![]), &[])),
        ]
    );
}

#[test]
fn test_emit_fn_params() {
    let code = "/[x] 42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::RET,
            Inst::PUSH_CLOSURE(closure_meta(
                -3,
                1,
                vec![],
                ParamLayout::Fixed(vec![0]),
                &["x"]
            )),
        ]
    );
}

#[test]
fn test_emit_fn_capture() {
    let code = "/[] y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(2),
            Inst::LOAD_GLOBAL("y".to_string()),
            Inst::RET,
            Inst::PUSH_CLOSURE(closure_meta(-3, 0, vec![], ParamLayout::Fixed(vec![]), &[])),
        ]
    );
}

#[test]
fn test_emit_fn_capture_nested() {
    let code = "/[] [begin 
        [let x 1]
        /[] [+ x y]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(9),
            Inst::PUSH(Value::Num(1.0)),
            Inst::STORE_LOCAL(0),
            Inst::JMP(4),
            Inst::LOAD_UP(0),
            Inst::LOAD_GLOBAL("y".to_string()),
            Inst::ADD,
            Inst::RET,
            Inst::PUSH_CLOSURE(closure_meta(
                -5,
                0,
                vec![CaptureSrc::Local(0)],
                ParamLayout::Fixed(vec![]),
                &[]
            )),
            Inst::RET,
            Inst::PUSH_CLOSURE(closure_meta(-10, 1, vec![], ParamLayout::Fixed(vec![]), &["x"])),
        ]
    );
}

fn emit_expand(
    input: &str,
    is_define: bool,
    placeholders: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let mut result = vec![];
    let mut greedy_pos: Option<usize> = None;

    for (i, placeholder) in placeholders.iter().enumerate() {
        let (offset, index) = if greedy_pos.is_some() {
            (i - 1, -((placeholders.len() - i) as i32))
        } else {
            (0, i as i32)
        };

        match placeholder.as_ref() {
            Node::Expand(.., nested) => {
                result.push(Inst::PEEK(offset, index));
                result.extend(emit_expand(input, is_define, nested, ctx)?);
            }
            Node::Token(id) => match id {
                Token::Id(_, source) => {
                    let store = if is_define {
                        Inst::STORE_LOCAL(
                            ctx.borrow_mut()
                                .add_local(source.clone())
                                .map_err(|e| match e {
                                    SquareError::RuntimeError(msg) => SquareError::SyntaxError(
                                        input.to_string(),
                                        msg,
                                        id.pos().clone(),
                                        None,
                                    ),
                                    _ => e,
                                })?,
                        )
                    } else {
                        store_inst(ctx, source)
                    };

                    result.push(Inst::PEEK(offset, index));
                    result.push(store);
                    result.push(Inst::POP);
                }
                Token::Op(pos, op) => {
                    if op == "." {
                        result.push(Inst::PEEK(offset, index));
                        result.push(Inst::POP);
                    } else if op == "..." {
                        if greedy_pos.is_some() {
                            return Err(SquareError::SyntaxError(
                                input.to_string(),
                                format!("failed to emit_expand, multiple greedy placeholder, first occurs at index {}", greedy_pos.unwrap()),
                                pos.clone(),
                                None,
                            ));
                        } else {
                            greedy_pos = Some(i);
                        }
                    } else {
                        return Err(SquareError::SyntaxError(
                            input.to_string(),
                            format!(
                                "failed to emit_expand, expect identifier or placeholder, got {}",
                                id
                            ),
                            pos.clone(),
                            None,
                        ));
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    result.push(Inst::POP); // drop pack

    Ok(result)
}

#[test]
fn test_emit_expand() {
    let code = "[= [a b] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("c".to_string()),
            Inst::PEEK(0, 0),
            Inst::STORE_GLOBAL("a".to_string()),
            Inst::POP,
            Inst::PEEK(0, 1),
            Inst::STORE_GLOBAL("b".to_string()),
            Inst::POP,
            Inst::POP,
        ]
    );
}

#[test]
fn test_emit_expand_dot() {
    let code = "[= [. b] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("c".to_string()),
            Inst::PEEK(0, 0),
            Inst::POP,
            Inst::PEEK(0, 1),
            Inst::STORE_GLOBAL("b".to_string()),
            Inst::POP,
            Inst::POP,
        ]
    );
}

#[test]
fn test_emit_expand_greed() {
    let code = "[= [... b] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("c".to_string()),
            Inst::PEEK(0, -1),
            Inst::STORE_GLOBAL("b".to_string()),
            Inst::POP,
            Inst::POP,
        ]
    );
}

#[test]
fn test_emit_expand_greed_offset() {
    let code = "[= [. ... . b] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("c".to_string()),
            Inst::PEEK(0, 0),
            Inst::POP,
            Inst::PEEK(1, -2),
            Inst::POP,
            Inst::PEEK(2, -1),
            Inst::STORE_GLOBAL("b".to_string()),
            Inst::POP,
            Inst::POP,
        ]
    );
}

#[test]
fn test_emit_expand_nested() {
    let code = "[= [[[b]]] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("c".to_string()),
            Inst::PEEK(0, 0),
            Inst::PEEK(0, 0),
            Inst::PEEK(0, 0),
            Inst::STORE_GLOBAL("b".to_string()),
            Inst::POP,
            Inst::POP,
            Inst::POP,
            Inst::POP,
        ]
    );
}

/// 超指令融合（peephole）：整程序出口对最终指令序列做模式替换，
/// 省一次派发与中间值往返：
///   CMP + JNE     → CMP_JNE（比较跳转，免中间 Bool）
///   LOAD_LOCAL ×2 → LOAD2_LOCAL（合一次借用）
/// 只挂在 [`emit`]（整程序）；`emit_multi_node` 不做，字节码断言不受影响。
/// 融合改变指令下标，所有相对偏移（JMP/JNE/CMP_JNE、PUSH_CLOSURE meta）
/// 经 old→new 索引映射统一改写。
fn fuse_superinsts(insts: Vec<Inst>) -> Vec<Inst> {
    fn cmp_op(inst: &Inst) -> Option<u8> {
        match inst {
            Inst::EQ => Some(11),
            Inst::NE => Some(12),
            Inst::LT => Some(13),
            Inst::LE => Some(14),
            Inst::GT => Some(15),
            Inst::GE => Some(16),
            _ => None,
        }
    }

    // pass 1：模式替换，记录 old→new 下标映射
    let mut fused: Vec<Inst> = Vec::with_capacity(insts.len());
    let mut map: Vec<usize> = Vec::with_capacity(insts.len() + 1);
    let mut src: Vec<usize> = Vec::with_capacity(insts.len()); // new idx -> 融合对首条（或自身）的旧下标
    let mut i = 0;
    while i < insts.len() {
        match (&insts[i], insts.get(i + 1)) {
            (cmp, Some(Inst::JNE(off))) if cmp_op(cmp).is_some() => {
                // 保留原 JNE 偏移，pass 2 以 JNE 位置为基准重定位
                fused.push(Inst::CMP_JNE(cmp_op(cmp).unwrap(), *off));
                map.push(fused.len() - 1);
                map.push(fused.len() - 1);
                src.push(i);
                i += 2;
            }
            (Inst::LOAD_LOCAL(a), Some(Inst::LOAD_LOCAL(b))) => {
                fused.push(Inst::LOAD2_LOCAL(*a, *b));
                map.push(fused.len() - 1);
                map.push(fused.len() - 1);
                src.push(i);
                i += 2;
            }
            (Inst::LOAD_LOCAL(a), Some(Inst::PUSH(v))) => {
                fused.push(Inst::LOADP_LOCAL(*a, v.clone()));
                map.push(fused.len() - 1);
                map.push(fused.len() - 1);
                src.push(i);
                i += 2;
            }
            (Inst::PUSH(v), Some(op @ (Inst::ADD | Inst::SUB | Inst::MUL | Inst::DIV | Inst::REM))) => {
                let op_id = match op {
                    Inst::ADD => 2,
                    Inst::SUB => 3,
                    Inst::MUL => 4,
                    Inst::DIV => 5,
                    _ => 6,
                };
                fused.push(Inst::BINOP_IMM(op_id, v.clone()));
                map.push(fused.len() - 1);
                map.push(fused.len() - 1);
                src.push(i);
                i += 2;
            }
            _ => {
                fused.push(insts[i].clone());
                map.push(fused.len() - 1);
                src.push(i);
                i += 1;
            }
        }
    }
    map.push(fused.len()); // target == len 的哨兵（run 以 pc < len 终止）

    // pass 2：按映射改写相对偏移
    for (new_idx, inst) in fused.iter_mut().enumerate() {
        let old_idx = src[new_idx];
        match inst {
            Inst::JMP(off) => {
                let target = (old_idx as i32 + 1 + *off) as usize;
                *off = map[target] as i32 - (new_idx as i32 + 1);
            }
            Inst::JNE(off) => {
                let target = (old_idx as i32 + 1 + *off) as usize;
                *off = map[target] as i32 - (new_idx as i32 + 1);
            }
            Inst::CMP_JNE(_, off) => {
                // 基准是融合前的 JNE（对的第二条）
                let target = (old_idx as i32 + 2 + *off) as usize;
                *off = map[target] as i32 - (new_idx as i32 + 1);
            }
            Inst::PUSH_CLOSURE(f) => {
                if let Function::ClosureMeta(info) = f {
                    let info = Rc::make_mut(info);
                    let ip = (old_idx as i32 + info.offset) as usize;
                    info.offset = map[ip] as i32 - new_idx as i32;
                }
            }
            _ => {}
        }
    }
    fused
}

/// Emit `base.k1.k2.…` as `base` followed by one GET per key.
fn emit_get_chain(
    input: &str,
    base: &Box<Node>,
    keys: &[String],
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let mut result = emit_node(input, base, ctx)?;
    result.extend(keys.iter().map(|k| Inst::GET(k.clone())));
    Ok(result)
}

fn emit_dot(
    input: &str,
    obj: &Box<Node>,
    properties: &[Box<Node>],
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let keys: Vec<String> = properties
        .iter()
        .map(|n| match n.as_ref() {
            Node::Prop(_, Token::Id(_, id)) => id.clone(),
            _ => unreachable!(),
        })
        .collect();
    emit_get_chain(input, obj, &keys, ctx)
}

#[test]
fn test_emit_dot() {
    let code = "o.x.y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit_multi_node(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    // o GET x GET y
    assert_eq!(
        insts,
        vec![
            Inst::LOAD_GLOBAL("o".to_string()),
            Inst::GET("x".to_string()),
            Inst::GET("y".to_string()),
        ]
    );
}

/// 节点在源码中的代表位置，供 source map 记录。
fn node_position(node: &Node) -> Option<Position> {
    Some(match node {
        Node::Token(t) => t.pos().clone(),
        Node::Expand(lb, _, _) => lb.pos().clone(),
        Node::Fn(slash, _, _) => slash.pos().clone(),
        Node::Prop(dot, _) => dot.pos().clone(),
        Node::Assign(eq, _, _, _) => eq.pos().clone(),
        Node::Op(op, _) => op.pos().clone(),
        Node::Call(lb, _, _) => lb.pos().clone(),
        Node::Dot(obj, _) => node_position(obj)?,
    })
}

fn emit_node(input: &str, node: &Box<Node>, ctx: &RefCell<EmitContext>) -> EmitResult {
    match node.as_ref() {
        Node::Token(token) => emit_token(input, token, ctx),
        Node::Assign(eq, target, properties, expression) => {
            emit_assign(input, eq, target, properties, expression, ctx)
        }
        Node::Fn(_, params, body) => emit_fn(input, params, body, ctx),
        Node::Op(op, expressions) => emit_op(input, op, expressions, ctx),
        Node::Call(left_bracket, _, expressions) => {
            emit_call(input, left_bracket, expressions, ctx)
        }
        Node::Dot(obj, properties) => emit_dot(input, obj, properties, ctx),
        _ => unreachable!(),
    }
}

fn emit_multi_node(
    input: &str,
    ast: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> Result<Vec<Inst>, SquareError> {
    let mut insts = vec![];

    for node in ast {
        insts.extend(emit_node(input, node, ctx)?);
    }

    Ok(insts)
}

pub fn emit(
    input: &str,
    ast: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> Result<(Vec<Inst>, SourceMap), SquareError> {
    let mut insts = vec![];
    let mut source_map = SourceMap::new();
    let mut mindex = ctx.borrow().base_mindex;

    let names_site = insts.len();
    insts.push(Inst::NAMES(Rc::new(Vec::new()))); // 占位，收尾回填根帧名表

    for node in ast {
        if let Some(pos) = node_position(node) {
            source_map.push(insts.len(), pos);
        }
        insts.push(Inst::DELIMITER(mindex));
        mindex += 1;
        insts.extend(emit_node(input, node, ctx)?);
    }
    insts.push(Inst::DELIMITER(mindex));

    if let Inst::NAMES(names) = &mut insts[names_site] {
        *names = Rc::new(ctx.borrow().root_names());
    }

    Ok((fuse_superinsts(insts), source_map))
}

