use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

pub mod codegen;
pub mod name_resolve;
pub mod type_check;
pub mod type_construct;

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    var: HashMap<String, Variable>,
    ty: HashMap<String, Rc<RefCell<TypeInference>>>,
    is_boundary: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInstance {
    I32,
    I64,
    Bool,
    Tuple {
        id: usize,
        args: Vec<TypeInstance>,
    },
    Function {
        args: Vec<TypeInstance>,
        result: Box<TypeInstance>,
    },
    Never,
    Array {
        id: usize,
        element: Box<TypeInstance>,
        len: usize,
    },
    Range {
        id: usize,
        end: Box<TypeInstance>,
    },
    Slice {
        id: usize,
        element: Box<TypeInstance>,
    },
    Char,
}

impl TypeInstance {
    fn remove_id(&self, out: &mut String) {
        use std::fmt::Write;
        use TypeInstance::*;
        match self {
            I32 => out.push('0'),
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
            Never => out.push('6'),
            Array { element, len, .. } => {
                element.remove_id(out);
                writeln!(out, "${len}").unwrap();
            }
            Range { end, .. } => {
                out.push_str("7[");
                end.remove_id(out);
                out.push(']');
            }
            Slice { element, .. } => {
                element.remove_id(out);
                writeln!(out, "$?").unwrap();
            }
            Char => out.push('8'),
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
    Simple(TypeInstance),
    Tuple(Vec<Rc<RefCell<TypeInference>>>),
    Integer,
    Unknown,
    Error,
    Function {
        args: Vec<Rc<RefCell<TypeInference>>>,
        result: Rc<RefCell<TypeInference>>,
    },
    Never,
    Array {
        element: Rc<RefCell<TypeInference>>,
        len: usize,
    },
    Range {
        end: Rc<RefCell<TypeInference>>,
    },
    Slice {
        element: Rc<RefCell<TypeInference>>,
    },
}

impl TypeInference {
    pub fn unify(a: &Rc<RefCell<Self>>, b: &Rc<RefCell<Self>>) {
        if Rc::ptr_eq(&a, &b) {
            return;
        }
        use TypeInference::*;
        let unified = match (&*a.borrow(), &*b.borrow()) {
            (Error, _) | (_, Error) => Error,
            (Unknown, known @ _) | (known @ _, Unknown) => known.clone(),
            (Never, always @ _) | (always @ _, Never) => always.clone(),
            (Simple(TypeInstance::I32), Integer) | (Integer, Simple(TypeInstance::I32)) => {
                Simple(TypeInstance::I32)
            }
            (Simple(TypeInstance::I64), Integer) | (Integer, Simple(TypeInstance::I64)) => {
                Simple(TypeInstance::I64)
            }
            (Simple(TypeInstance::Char), Integer) | (Integer, Simple(TypeInstance::Char)) => {
                Simple(TypeInstance::Char)
            }
            (Tuple(aargs), Tuple(bargs)) => {
                if aargs.len() != bargs.len() {
                    Error
                } else {
                    for (aarg, barg) in aargs.iter().zip(bargs.iter()) {
                        Self::unify(aarg, barg);
                    }
                    a.borrow().clone()
                }
            }
            (Slice { element: l }, Slice { element: r }) => {
                TypeInference::unify(l, r);
                a.borrow().clone()
            }
            (Slice { element: model }, Array { element: other, .. })
            | (Array { element: other, .. }, Slice { element: model }) => {
                Self::unify(model, other);
                Slice {
                    element: Rc::clone(model),
                }
            }
            (
                Array {
                    element: aelem,
                    len: alen,
                },
                Array {
                    element: belem,
                    len: blen,
                },
            ) => {
                if alen != blen {
                    Error
                } else {
                    Self::unify(aelem, belem);
                    a.borrow().clone()
                }
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
                    Error
                } else {
                    Self::unify(lresult, rresult);
                    if *lresult.borrow() == Error {
                        Error
                    } else {
                        let mut is_never = false;
                        for (larg, rarg) in largs.iter().zip(rargs.iter()) {
                            Self::unify(larg, rarg);
                            if *larg.borrow() == Error {
                                is_never = true;
                                break;
                            }
                        }
                        if is_never {
                            Error
                        } else {
                            return;
                        }
                    }
                }
            }
            (Range { end: lend }, Range { end: rend }) => {
                Self::unify(lend, rend);
                lend.borrow().clone()
            }
            _ if a == b => return,
            _ => Error,
        };
        a.replace(unified.clone());
        b.replace(unified);
    }
}

impl Display for TypeInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInstance::I32 => f.write_str("i32"),
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
            TypeInstance::Never => f.write_str("!"),
            TypeInstance::Array { element, len, .. } => {
                f.write_str("[")?;
                element.fmt(f)?;
                f.write_str("; ")?;
                len.fmt(f)?;
                f.write_str("]")
            }
            TypeInstance::Range { end, .. } => {
                f.write_str("<")?;
                end.fmt(f)?;
                f.write_str("..")?;
                end.fmt(f)?;
                f.write_str(">")
            }
            TypeInstance::Slice { element, .. } => {
                f.write_str("[")?;
                element.fmt(f)?;
                f.write_str("]")
            }
            TypeInstance::Char => f.write_str("char"),
        }
    }
}

impl TypeInstance {
    pub fn mapped(&self) -> String {
        match self {
            TypeInstance::I32 => "int32_t".into(),
            TypeInstance::Bool | TypeInstance::Char => "char".into(),
            TypeInstance::Tuple { id, .. }
            | TypeInstance::Array { id, .. }
            | TypeInstance::Range { id, .. }
            | TypeInstance::Slice { id, .. } => format!("T{id}"),
            TypeInstance::I64 => "int64_t".into(),
            TypeInstance::Function { .. } => format!("void"),
            TypeInstance::Never => "void".into(),
        }
    }

    pub fn declare(&self, name: &str) -> String {
        format!("{} {name}", self.mapped())
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub mangle: String,
    pub ty: Rc<RefCell<TypeInference>>,
    pub is_global: bool,
}

impl Scope {
    pub fn root() -> Self {
        let mut ty = HashMap::new();
        ty.insert(
            "i32".into(),
            Rc::new(RefCell::new(TypeInference::Simple(TypeInstance::I32))),
        );
        ty.insert(
            "i64".into(),
            Rc::new(RefCell::new(TypeInference::Simple(TypeInstance::I64))),
        );
        ty.insert(
            "bool".into(),
            Rc::new(RefCell::new(TypeInference::Simple(TypeInstance::Bool))),
        );
        ty.insert(
            "char".into(),
            Rc::new(RefCell::new(TypeInference::Simple(TypeInstance::Char))),
        );
        Self {
            parent: None,
            var: Default::default(),
            ty,
            is_boundary: false,
        }
    }

    pub fn with_parent(parent: Rc<RefCell<Scope>>, is_boundary: bool) -> Self {
        Self {
            parent: Some(parent),
            var: Default::default(),
            ty: Default::default(),
            is_boundary,
        }
    }

    pub fn get_var(&self, name: &str) -> Option<GetVar> {
        if let Some(var) = self.var.get(name) {
            return Some(GetVar {
                is_global: false,
                var: var.clone(),
            });
        }
        let mut var = self.parent.as_ref()?.borrow().get_var(name)?;
        var.is_global |= self.is_boundary;
        Some(var)
    }

    pub fn mark_global(&mut self, name: &str) {
        if let Some(var) = self.var.get_mut(name) {
            var.is_global = true;
        } else {
            self.parent
                .as_ref()
                .map(|p| p.borrow_mut().mark_global(name));
        }
    }

    pub fn get_ty(&self, name: &str) -> Option<Rc<RefCell<TypeInference>>> {
        if let Some(ty) = self.ty.get(name) {
            return Some(Rc::clone(ty));
        }
        self.parent.as_ref()?.borrow().get_ty(name)
    }
}

pub struct GetVar {
    is_global: bool,
    var: Variable,
}
