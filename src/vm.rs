use core::ops::Add;

#[cfg(not(test))]
use alloc::vec::Vec;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Int32(i32),
    Int64(i64),
}

impl Value {
    fn byte_length(self) -> usize {
        match self {
            Value::Int32(_) => 4,
            Value::Int64(_) => 8,
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, another: Value) -> Value {
        match self {
            Value::Int32(lhs) => match another {
                Value::Int32(rhs) => Value::Int32(lhs + rhs),
                Value::Int64(rhs) => Value::Int64(Into::<i64>::into(lhs) + rhs),
            },
            Value::Int64(lhs) => match another {
                Value::Int32(rhs) => Value::Int64(lhs + Into::<i64>::into(rhs)),
                Value::Int64(rhs) => Value::Int64(lhs + rhs),
            },
        }
    }
}

type Tag = Value;
type TaggedBytes = (Vec<u8>, Tag);

// used as tags
pub static I32: Tag = Tag::Int32(0);
pub static I64: Tag = Tag::Int64(0);

impl Into<TaggedBytes> for Value {
    fn into(self) -> TaggedBytes {
        match self {
            Value::Int32(value) => (value.to_le_bytes().to_vec(), I32),
            Value::Int64(value) => (value.to_le_bytes().to_vec(), I64),
        }
    }
}

impl From<TaggedBytes> for Value {
    fn from(bytes: TaggedBytes) -> Self {
        match bytes.1 {
            Tag::Int32(_) => {
                let value = i32::from_le_bytes(bytes.0.as_slice().try_into().unwrap());
                Value::Int32(value)
            }
            Tag::Int64(_) => {
                let value = i64::from_le_bytes(bytes.0.as_slice().try_into().unwrap());
                Value::Int64(value)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    RET,
    PUSH(Value),
    POP(Tag),
    ADD(Tag, Tag), // +
    SUB,           // -
    MUL,           // *
    DIV,           // /
    POW,           // ^
    MOD,           // %
    AND,           // &
    OR,            // |
    NOT,           // !
    EQ,            // ==
    NEQ,           // !=
    LT,            // <
    LTE,           // <=
    GT,            // >
    GTE,           // >=
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct VM {
    pub stack: Vec<u8>,
}

impl Default for VM {
    fn default() -> Self {
        Self::new(1024)
    }
}

impl VM {
    pub fn new(stack_size: usize) -> Self {
        Self {
            stack: Vec::with_capacity(stack_size), // item count, not byte length
        }
    }

    pub fn execute(&mut self, inst: Instruction) -> Option<Value> {
        match inst {
            Instruction::RET => None,
            Instruction::PUSH(value) => {
                self.stack
                    .extend_from_slice(Into::<TaggedBytes>::into(value).0.as_slice());
                None
            }
            Instruction::POP(tag) => {
                let bytes = self.stack.split_off(self.sp() - tag.byte_length());

                Value::try_from((bytes, tag)).ok()
            }
            Instruction::ADD(ltag, rtag) => {
                if let Some(rhs) = self.execute(Instruction::POP(rtag)) {
                    if let Some(lhs) = self.execute(Instruction::POP(ltag)) {
                        let result = lhs + rhs;

                        self.execute(Instruction::PUSH(result));
                        return Some(result);
                    }
                }

                None
            }
            _ => unimplemented!(),
        }
    }

    pub fn sp(&self) -> usize {
        return self.stack.len();
    }
}

#[test]
fn test_push_pop() {
    let mut vm = VM::default();

    vm.execute(Instruction::PUSH(Value::Int32(42)));
    vm.execute(Instruction::PUSH(Value::Int32(42)));

    assert_eq!(vm.sp(), 8);

    assert_eq!(
        vm.execute(Instruction::POP(I64)),
        Some(Value::Int64(0x2a0000002a))
    );

    assert_eq!(vm.sp(), 0);
}

#[test]
fn test_add() {
    let mut vm = VM::default();

    vm.execute(Instruction::PUSH(Value::Int32(42)));
    vm.execute(Instruction::PUSH(Value::Int32(42)));
    vm.execute(Instruction::PUSH(Value::Int32(0)));

    assert_eq!(
        vm.execute(Instruction::ADD(I32, I64)),
        Some(Value::Int64(84))
    );

    assert_eq!(vm.execute(Instruction::POP(I64)), Some(Value::Int64(84)));
}
