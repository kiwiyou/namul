use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

pub mod codegen;
pub mod resolver;
pub mod typeck;

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    var: HashMap<String, Variable>,
    ty: HashMap<String, TypeInstance>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInstance {
    I32,
    I64,
    Unit,
    Bool,
    Tuple {
        id: usize,
        args: Vec<TypeInstance>,
    },
    Function {
        args: Vec<TypeInstance>,
        result: Box<TypeInstance>,
    },
}

impl TypeInstance {
    fn remove_id(&self, out: &mut String) {
        use TypeInstance::*;
        match self {
            I32 => out.push('0'),
            Unit => out.push('1'),
            Bool => out.push('2'),
            Tuple { args, .. } => {
                out.push_str("3[");
                for arg in args.iter() {
                    arg.remove_id(out);
                    out.push(',');
                }
                out.push(']');
            }
            I64 => out.push('4'),
            Function { args, result } => {
                out.push_str("5[");
                result.remove_id(out);
                out.push(',');
                for arg in args.iter() {
                    arg.remove_id(out);
                    out.push(',');
                }
                out.push(']');
            }
        }
    }

    pub fn id_removed(&self) -> String {
        let mut ret = String::new();
        self.remove_id(&mut ret);
        ret
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInference {
    Exact(TypeInstance),
    Tuple(Vec<Rc<RefCell<TypeInference>>>),
    Function {
        args: Vec<Rc<RefCell<TypeInference>>>,
        result: Rc<RefCell<TypeInference>>,
    },
    Integer,
    Unknown,
    Never,
}

impl TypeInference {
    pub fn unify(&mut self, other: &mut TypeInference) {
        use TypeInference::*;
        let unified = match (&*self, &*other) {
            (Never, _) | (_, Never) => Never,
            (Unknown, known @ _) | (known @ _, Unknown) => known.clone(),
            (Tuple(a), Tuple(b)) => {
                if a.len() != b.len() {
                    Never
                } else {
                    for (a, b) in a.iter().zip(b.iter()) {
                        a.borrow_mut().unify(&mut b.borrow_mut());
                    }
                    self.clone()
                }
            }
            (Tuple(friend), Exact(TypeInstance::Tuple { args: model, .. }))
            | (Exact(TypeInstance::Tuple { args: model, .. }), Tuple(friend)) => {
                if model.len() != friend.len() {
                    Never
                } else {
                    for (model, friend) in model.iter().zip(friend.iter()) {
                        TypeInference::Exact(model.clone()).unify(&mut friend.borrow_mut());
                    }
                    self.clone()
                }
            }
            (Exact(TypeInstance::I32), Integer) | (Integer, Exact(TypeInstance::I32)) => {
                Exact(TypeInstance::I32)
            }
            (Exact(TypeInstance::I64), Integer) | (Integer, Exact(TypeInstance::I64)) => {
                Exact(TypeInstance::I64)
            }
            (
                Function {
                    args: largs,
                    result: lresult,
                },
                Function {
                    args: rargs,
                    result: rresult,
                },
            ) => {
                if largs.len() != rargs.len() {
                    Never
                } else {
                    let mut lresult_ty = lresult.borrow_mut();
                    lresult_ty.unify(&mut rresult.borrow_mut());
                    if *lresult_ty == Never {
                        Never
                    } else {
                        let mut is_never = false;
                        for (larg, rarg) in largs.iter().zip(rargs.iter()) {
                            let mut larg_ty = larg.borrow_mut();
                            larg_ty.unify(&mut rarg.borrow_mut());
                            if *larg_ty == Never {
                                is_never = true;
                                break;
                            }
                        }
                        if is_never {
                            Never
                        } else {
                            return;
                        }
                    }
                }
            }
            _ if self == other => return,
            _ => Never,
        };
        *self = unified.clone();
        *other = unified;
    }
}

impl Display for TypeInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInstance::I32 => f.write_str("i32"),
            TypeInstance::Unit => f.write_str("()"),
            TypeInstance::Bool => f.write_str("bool"),
            TypeInstance::Tuple { args, .. } => {
                f.write_str("(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ").unwrap();
                    }
                    arg.fmt(f)?;
                }
                f.write_str(")")
            }
            TypeInstance::I64 => f.write_str("i64"),
            TypeInstance::Function { args, result } => {
                f.write_str("fn (")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ").unwrap();
                    }
                    arg.fmt(f)?;
                }
                f.write_str(") ")?;
                result.fmt(f)
            }
        }
    }
}

impl TypeInstance {
    pub fn mapped(&self) -> String {
        match self {
            TypeInstance::I32 => "int32_t".into(),
            TypeInstance::Unit => "void".into(),
            TypeInstance::Bool => "char".into(),
            TypeInstance::Tuple { id, .. } => format!("T_{id}"),
            TypeInstance::I64 => "int64_t".into(),
            TypeInstance::Function { .. } => format!("void"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub mangle: String,
    pub ty: Rc<RefCell<TypeInference>>,
}

impl Scope {
    pub fn root() -> Self {
        let mut ty = HashMap::new();
        ty.insert("i32".into(), TypeInstance::I32);
        ty.insert("i64".into(), TypeInstance::I64);
        ty.insert("bool".into(), TypeInstance::Bool);
        Self {
            parent: None,
            var: Default::default(),
            ty,
        }
    }

    pub fn with_parent(parent: Rc<RefCell<Scope>>) -> Self {
        Self {
            parent: Some(parent),
            var: Default::default(),
            ty: Default::default(),
        }
    }

    pub fn get_var(&self, name: &str) -> Option<Variable> {
        if let Some(var) = self.var.get(name) {
            return Some(var.clone());
        }
        self.parent.as_ref()?.borrow().get_var(name)
    }

    pub fn get_ty(&self, name: &str) -> Option<TypeInstance> {
        if let Some(ty) = self.ty.get(name) {
            return Some(ty.clone());
        }
        self.parent.as_ref()?.borrow().get_ty(name)
    }
}
