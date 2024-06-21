use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Write,
    rc::Rc,
};

use crate::syntax::{
    expression::{
        Assignee, Assignment, BinaryOperation, Block, BlockExpression, Comparison,
        CompoundAssignment, Declaration, Expression, If, Index, Invocation, MakeArray, MakeTuple,
        NonblockExpression, Place, Print, Select,
    },
    format::FormatFragment,
    literal::Literal,
    path::Path,
    statement::{Pattern, Statement},
    Program, TokenKind,
};

use super::{
    name_resolve::NameResolver, type_check::TypeChecker, type_construct::TypeConstructor, Scope,
    TypeInference, TypeInstance,
};

pub struct Codegen {
    feature: HashSet<&'static str>,
    scopes: Vec<Rc<RefCell<Scope>>>,
    inference: Vec<Rc<RefCell<TypeInference>>>,
    type_id: HashMap<String, usize>,
    decl: String,
    structs: String,
    stack: Vec<Rc<RefCell<Scope>>>,
    block_stack: Vec<Rc<RefCell<Scope>>>,
    last_scope: usize,
    last_infer: usize,
    var_id: usize,
}

#[derive(Debug)]
pub struct Intermediate {
    ty: TypeInstance,
    decl: String,
    funcs: String,
    effect: String,
    immediate: String,
}

impl Codegen {
    pub fn synthesize(&mut self, inference: &TypeInference) -> Option<TypeInstance> {
        match inference {
            TypeInference::Exact(exact) => Some(exact.clone()),
            TypeInference::Integer => Some(TypeInstance::I32),
            TypeInference::Never => Some(TypeInstance::Never),
            TypeInference::Unknown => None,
            TypeInference::Error => None,
            TypeInference::Tuple(tuple) => {
                let mut args = vec![];
                for arg in tuple.iter() {
                    let ty = self.synthesize(&arg.borrow())?;
                    if ty == TypeInstance::Never {
                        return Some(TypeInstance::Never);
                    }
                    args.push(ty);
                }
                let mut instance = TypeInstance::Tuple {
                    id: 0,
                    args: args.clone(),
                };
                let last_id = self.type_id.len();
                let new = *self.type_id.entry(instance.id_removed()).or_insert(last_id);
                if let TypeInstance::Tuple { id, .. } = &mut instance {
                    *id = new;
                }
                if self.type_id.len() > last_id {
                    writeln!(self.decl, "typedef struct {0} {0};", instance.mapped()).unwrap();
                    writeln!(self.structs, "struct {} {{", instance.mapped()).unwrap();
                    for (i, arg) in args.iter().enumerate() {
                        writeln!(self.structs, "{};", arg.declare(&format!("m{i}"))).unwrap();
                    }
                    writeln!(self.structs, "}};").unwrap();
                }
                Some(instance)
            }
            TypeInference::Function { args, result } => {
                let mut param = vec![];
                for arg in args.iter() {
                    param.push(self.synthesize(&arg.borrow())?);
                }
                let result = self.synthesize(&result.borrow())?;
                Some(TypeInstance::Function {
                    args: param,
                    result: Box::new(result),
                })
            }
            TypeInference::Array { element, len } => {
                let element = self.synthesize(&element.borrow())?;
                let mut instance = TypeInstance::Array {
                    id: 0,
                    element: Box::new(element.clone()),
                    len: *len,
                };
                let last_id = self.type_id.len();
                let new = *self.type_id.entry(instance.id_removed()).or_insert(last_id);
                if let TypeInstance::Array { id, .. } = &mut instance {
                    *id = new;
                }
                if self.type_id.len() > last_id {
                    writeln!(self.decl, "typedef struct {0} {0};", instance.mapped()).unwrap();
                    writeln!(self.structs, "struct {} {{", instance.mapped()).unwrap();
                    writeln!(self.structs, "{} v[{len}];", element.mapped()).unwrap();
                    writeln!(self.structs, "}};").unwrap();
                }
                Some(instance)
            }
        }
    }

    pub fn path(&mut self, path: &Path) -> Intermediate {
        let result = match path {
            Path::Simple(ident) => {
                let scope = Rc::clone(&self.stack.last().unwrap());
                let scope = scope.borrow();
                let var = scope.get_var(&ident.content).unwrap();
                let current_infer = Rc::clone(&self.inference[self.last_infer]);
                self.last_infer += 1;
                let Some(ty) = self.synthesize(&current_infer.borrow()) else {
                    panic!("Could not deduce type of {}.", ident.content);
                };
                Intermediate {
                    decl: "".into(),
                    funcs: "".into(),
                    ty,
                    effect: "".into(),
                    immediate: format!("{}", var.mangle.clone()),
                }
            }
        };
        result
    }

    pub fn binary(&mut self, binary: &BinaryOperation) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let len = self.stack.len();
        let lhs = self.expression(&binary.lhs);
        self.stack.truncate(len);
        let rhs = self.expression(&binary.rhs);
        self.stack.truncate(len);
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let mut immediate = String::new();
        match (lhs.ty, binary.operator.kind, rhs.ty) {
            (
                TypeInstance::I32 | TypeInstance::I64,
                TokenKind::PunctPlusSign
                | TokenKind::PunctHyphenMinus
                | TokenKind::PunctAsterisk
                | TokenKind::PunctSolidus
                | TokenKind::PunctPercentSign,
                TypeInstance::I32 | TypeInstance::I64,
            )
            | (
                TypeInstance::Bool,
                TokenKind::PunctAmpersandAmpersand | TokenKind::PunctVerticalLineVerticalLine,
                TypeInstance::Bool,
            ) => {
                decl.push_str(&lhs.decl);
                decl.push_str(&rhs.decl);
                funcs.push_str(&lhs.funcs);
                funcs.push_str(&rhs.funcs);
                effect.push_str(&lhs.effect);
                effect.push_str(&rhs.effect);
                if ty != TypeInstance::Never {
                    let id = self.var_id;
                    self.var_id += 1;
                    let ident = format!("b{id}");
                    self.define_var(
                        &mut effect,
                        &ty,
                        &ident,
                        &format!(
                            "{} {} {}",
                            lhs.immediate, binary.operator.content, rhs.immediate
                        ),
                    );
                    immediate = ident;
                }
            }
            (l, _, r) => panic!(
                "Unsupported operation `{l}` {} `{r}`",
                binary.operator.content
            ),
        }
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate,
        }
    }

    pub fn literal(&mut self, literal: &Literal) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let result = match literal {
            Literal::Decimal(decimal) => Intermediate {
                ty,
                decl: "".into(),
                funcs: "".into(),
                effect: "".into(),
                immediate: decimal.0.content.clone(),
            },
            Literal::Bool(bool) => Intermediate {
                ty,
                decl: "".into(),
                funcs: "".into(),
                effect: "".into(),
                immediate: match bool.0.kind {
                    TokenKind::KeywordTrue => "1".into(),
                    TokenKind::KeywordFalse => "0".into(),
                    _ => unreachable!(),
                },
            },
        };
        result
    }

    pub fn comparison(&mut self, comparison: &Comparison) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let id = self.var_id;
        self.var_id += 1;
        let first = self.expression(&comparison.first);
        decl.push_str(&first.decl);
        funcs.push_str(&first.funcs);
        effect.push_str(&first.effect);
        let mut prev_ty = first.ty;
        let mut prev = first.immediate;
        let mut close = 0;
        let mut is_never = false;
        for (i, (op, next)) in comparison.chain.iter().enumerate() {
            let next = self.expression(next);
            if is_never {
                continue;
            }
            decl.push_str(&next.decl);
            funcs.push_str(&next.funcs);
            if i > 0 {
                writeln!(effect, "if (cmp{id}) {{").unwrap();
            }
            effect.push_str(&next.effect);
            match (&prev_ty, op.kind, &next.ty) {
                (TypeInstance::Never, _, _) | (_, _, TypeInstance::Never) => {
                    is_never = true;
                }
                (
                    TypeInstance::I32 | TypeInstance::I64,
                    _,
                    TypeInstance::I32 | TypeInstance::I64,
                ) => {
                    if i == 0 {
                        write!(effect, "char ").unwrap();
                    }
                    writeln!(
                        effect,
                        "cmp{id} = {prev} {} {};",
                        op.content, next.immediate
                    )
                    .unwrap();
                }
                (
                    TypeInstance::Bool,
                    TokenKind::PunctEqualsSignEqualsSign
                    | TokenKind::PunctExclamationMarkEqualsSign,
                    TypeInstance::Bool,
                ) => {
                    if i == 0 {
                        write!(effect, "char ").unwrap();
                    }
                    writeln!(
                        effect,
                        "cmp{id} = {prev} {} {};",
                        op.content, next.immediate
                    )
                    .unwrap();
                }
                _ => panic!(
                    "Could not compare type `{prev_ty}` {} `{}`.",
                    op.content, next.ty
                ),
            }
            close += 1;
            prev_ty = next.ty;
            prev = next.immediate;
        }
        for _ in 1..close {
            effect.push('}');
        }
        if close > 1 {
            effect.push('\n');
        }
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate: if is_never {
                "".into()
            } else {
                format!("cmp{id}")
            },
        }
    }

    pub fn print(&mut self, print: &Print) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let len = self.stack.len();
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let mut immediates = vec![];
        for arg in print.args.iter() {
            let expr = self.expression(arg);
            decl.push_str(&expr.decl);
            funcs.push_str(&expr.funcs);
            effect.push_str(&expr.effect);
            immediates.push((false, expr.immediate, expr.ty));
        }
        let scope = Rc::clone(&self.stack.last().unwrap());
        self.feature.insert("writer_def");
        let mut last_auto = 0;
        for fragment in print.format.fragment.iter() {
            match fragment {
                FormatFragment::Literal(literal) => {
                    self.feature.insert("write_literal");
                    writeln!(
                        effect,
                        "write_literal({}, {});",
                        double_quote(literal),
                        literal.len()
                    )
                    .unwrap();
                }
                FormatFragment::Ident(ident) => {
                    let Some(var) = scope.borrow().get_var(&ident.content) else {
                        panic!("Could not print undefined variable {}.", ident.content);
                    };
                    let Some(var_ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    match var_ty {
                        TypeInstance::I32 | TypeInstance::I64 => {
                            self.feature.insert("write_int");
                            writeln!(effect, "write_int({});", var.mangle).unwrap();
                        }
                        _ => panic!("Could not print value of type `{}`.", ty),
                    }
                }
                FormatFragment::Placeholder(index) => {
                    let index = if let &Some(index) = index {
                        index
                    } else {
                        let index = last_auto;
                        last_auto += 1;
                        if index > immediates.len() {
                            panic!("Too many auto format arguments.");
                        }
                        index
                    };
                    let Some((used, arg, ty)) = immediates.get_mut(index) else {
                        panic!(
                            "Format argument index was {} but length was {}.",
                            index,
                            immediates.len()
                        );
                    };
                    *used = true;
                    match &*ty {
                        TypeInstance::I32 | TypeInstance::I64 => {
                            self.feature.insert("write_int");
                            writeln!(effect, "write_int({});", arg).unwrap();
                        }
                        _ => panic!("Could not print value of type `{}`.", ty),
                    }
                }
            }
        }
        if let Some(position) = immediates.iter().position(|(used, _, _)| !used) {
            panic!("Unused format argument {}.", position);
        }
        self.stack.truncate(len);
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate: "".into(),
        }
    }

    pub fn if_(&mut self, if_: &If) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let len = self.stack.len();
        let condition = self.expression(&if_.condition);
        decl.push_str(&condition.decl);
        funcs.push_str(&condition.funcs);
        effect.push_str(&condition.effect);
        let mut immediate = String::new();
        if condition.ty == TypeInstance::Never {
            // empty
        } else if let Some(falsy) = &if_.falsy {
            let id = self.var_id;
            self.var_id += 1;
            let ident = format!("if{id}");
            if ty != TypeInstance::Never {
                writeln!(effect, "{};", ty.declare(&ident)).unwrap();
            }
            writeln!(effect, "if ({}) {{", condition.immediate).unwrap();
            let truthy = self.block(&if_.truthy);
            decl.push_str(&truthy.decl);
            funcs.push_str(&truthy.funcs);
            effect.push_str(&truthy.effect);
            if ty != TypeInstance::Never {
                self.assign_var(&mut effect, &ty, &ident, &truthy.immediate);
            }
            writeln!(effect, "}} else {{").unwrap();
            let falsy = self.block(falsy);
            decl.push_str(&falsy.decl);
            funcs.push_str(&falsy.funcs);
            effect.push_str(&falsy.effect);
            if ty != TypeInstance::Never {
                self.assign_var(&mut effect, &ty, &ident, &falsy.immediate);
            }
            writeln!(effect, "}}").unwrap();
            immediate = if ty == TypeInstance::Never {
                "".into()
            } else {
                format!("if{id}")
            };
        } else {
            writeln!(effect, "if ({}) {{", condition.immediate).unwrap();
            let truthy = self.block(&if_.truthy);
            decl.push_str(&truthy.decl);
            funcs.push_str(&truthy.funcs);
            effect.push_str(&truthy.effect);
            writeln!(effect, "}}").unwrap();
        };
        self.stack.truncate(len);
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate,
        }
    }

    pub fn select(&mut self, select: &Select) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let len = self.stack.len();
        let condition = self.expression(&select.condition);
        let inside = self.stack.len();
        decl.push_str(&condition.decl);
        funcs.push_str(&condition.funcs);
        effect.push_str(&condition.effect);
        let id = self.var_id;
        self.var_id += 1;
        let ident = format!("if{id}");
        if ty != TypeInstance::Never {
            writeln!(effect, "{};", ty.declare(&ident)).unwrap();
        }
        writeln!(effect, "if ({}) {{", condition.immediate).unwrap();
        let truthy = self.expression(&select.truthy);
        self.stack.truncate(inside);
        decl.push_str(&truthy.decl);
        funcs.push_str(&truthy.funcs);
        effect.push_str(&truthy.effect);
        if ty != TypeInstance::Never {
            self.assign_var(&mut effect, &ty, &ident, &truthy.immediate);
        }
        writeln!(effect, "}} else {{").unwrap();
        let falsy = self.expression(&select.falsy);
        self.stack.truncate(len);
        decl.push_str(&falsy.decl);
        funcs.push_str(&falsy.funcs);
        effect.push_str(&falsy.effect);
        if ty != TypeInstance::Never {
            self.assign_var(&mut effect, &ty, &ident, &falsy.immediate);
        }
        writeln!(effect, "}}").unwrap();
        let immediate = if ty == TypeInstance::Never {
            "".into()
        } else {
            ident
        };
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate,
        }
    }

    pub fn make_tuple(&mut self, make_tuple: &MakeTuple) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let ty_mapped = ty.mapped();
        let mut construct = String::new();
        let mt_id = self.var_id;
        self.var_id += 1;
        write!(construct, "{ty_mapped} mt{mt_id} = {{").unwrap();
        for arg in make_tuple.args.iter() {
            let arg = self.expression(arg);
            decl.push_str(&arg.decl);
            funcs.push_str(&arg.funcs);
            effect.push_str(&arg.effect);
            construct.push_str(&arg.immediate);
            construct.push_str(", ");
        }
        writeln!(construct, "}};").unwrap();
        let immediate = if ty != TypeInstance::Never {
            effect.push_str(&construct);
            format!("mt{mt_id}")
        } else {
            "".into()
        };
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate,
        }
    }

    pub fn make_array(&mut self, make_array: &MakeArray) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let ty_mapped = ty.mapped();
        let mut construct = String::new();
        let mt_id = self.var_id;
        self.var_id += 1;
        write!(construct, "{ty_mapped} ma{mt_id} = {{{{").unwrap();
        for arg in make_array.args.iter() {
            let arg = self.expression(arg);
            decl.push_str(&arg.decl);
            funcs.push_str(&arg.funcs);
            effect.push_str(&arg.effect);
            construct.push_str(&arg.immediate);
            construct.push_str(", ");
        }
        writeln!(construct, "}}}};").unwrap();
        let immediate = if ty != TypeInstance::Never {
            effect.push_str(&construct);
            format!("ma{mt_id}")
        } else {
            "".into()
        };
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate,
        }
    }

    pub fn statement(&mut self, statement: &Statement) -> Intermediate {
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        match statement {
            Statement::Nop => {}
            Statement::Input(parser) => {
                let scope = Rc::clone(&self.scopes[self.last_scope]);
                self.last_scope += 1;
                self.stack.push(Rc::clone(&scope));
                for arg in parser.arg.iter() {
                    self.assignee_input(&mut decl, &mut funcs, &mut effect, arg);
                }
            }
            Statement::Expression(expr) => {
                let expr = self.expression(expr);
                decl.push_str(&expr.decl);
                funcs.push_str(&expr.funcs);
                effect.push_str(&expr.effect);
            }
            Statement::Repeat(repeat) => {
                let len = self.stack.len();
                let times = self.expression(&repeat.times);
                decl.push_str(&times.decl);
                funcs.push_str(&times.funcs);
                effect.push_str(&times.effect);
                if !matches!(times.ty, TypeInstance::I32 | TypeInstance::I64) {
                    panic!("Could not repeat for non-integer type `{}`", times.ty);
                }
                writeln!(
                    effect,
                    "for ({} i = 0; i < {}; ++i) {{",
                    times.ty.mapped(),
                    times.immediate
                )
                .unwrap();
                if let Some(ident) = &repeat.var {
                    let scope = Rc::clone(&self.scopes[self.last_scope]);
                    self.last_scope += 1;
                    self.stack.push(Rc::clone(&scope));
                    let var = scope.borrow().get_var(&ident.content).unwrap();
                    let Some(ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    self.define_var(&mut effect, &ty, &var.mangle, "i");
                }
                let block = self.block(&repeat.block);
                self.stack.truncate(len);
                decl.push_str(&block.decl);
                funcs.push_str(&block.funcs);
                effect.push_str(&block.effect);
                writeln!(effect, "}}").unwrap();
            }
            Statement::While(while_) => {
                let len = self.stack.len();
                let condition = self.expression(&while_.condition);
                let block = self.block(&while_.block);
                decl.push_str(&condition.decl);
                funcs.push_str(&condition.funcs);
                if condition.ty != TypeInstance::Never {
                    if condition.ty != TypeInstance::Bool {
                        panic!(
                            "Could not repeat while condition of type `{}`",
                            condition.ty
                        );
                    }
                    writeln!(effect, "for(;;) {{").unwrap();
                    effect.push_str(&condition.effect);
                    writeln!(effect, "if (!{}) break;", condition.immediate).unwrap();
                    self.stack.truncate(len);
                    decl.push_str(&block.decl);
                    funcs.push_str(&block.funcs);
                    effect.push_str(&block.effect);
                    writeln!(effect, "}}").unwrap();
                } else {
                    effect.push_str(&condition.effect);
                }
            }
            Statement::Function(function) => {
                let block = Rc::clone(self.block_stack.last().unwrap());
                let block = block.borrow();
                let var = block.get_var(&function.ident.content).unwrap();
                let Some(TypeInstance::Function { args, result }) =
                    self.synthesize(&var.ty.borrow())
                else {
                    panic!("Could not deduce type.");
                };
                let scope = Rc::clone(&self.scopes[self.last_scope]);
                self.last_scope += 1;
                write!(decl, "{}(", &result.declare(&var.mangle)).unwrap();
                write!(funcs, "{}(", &result.declare(&var.mangle)).unwrap();
                for (i, (arg, ty)) in function.args.iter().zip(args.iter()).enumerate() {
                    if i > 0 {
                        decl.push_str(", ");
                        funcs.push_str(", ");
                    }
                    write!(decl, "{}", ty.mapped()).unwrap();
                    match arg {
                        Pattern::Ident(_) => unreachable!(),
                        Pattern::Declaration(declaration) => {
                            let scope = scope.borrow();
                            let var = scope.get_var(&declaration.ident.content).unwrap();
                            funcs.push_str(&ty.declare(&var.mangle));
                        }
                        Pattern::Tuple(_) => {
                            funcs.push_str(&ty.declare(&format!("a{i}")));
                        }
                    }
                }
                writeln!(decl, ");").unwrap();
                writeln!(funcs, ") {{").unwrap();
                for (i, arg) in function.args.iter().enumerate() {
                    match arg {
                        Pattern::Ident(_) => unreachable!(),
                        Pattern::Declaration(_) => {}
                        Pattern::Tuple(tuple) => {
                            let mut rhs = format!("a{i}");
                            let scope = scope.borrow();
                            self.assign_pattern(&mut funcs, &scope, &mut rhs, &tuple);
                        }
                    }
                }
                self.stack.push(scope);
                let block = self.block(&function.block);
                self.stack.pop();
                funcs.push_str(&block.effect);
                if block.ty != TypeInstance::Never {
                    writeln!(funcs, "return {};", block.immediate).unwrap();
                }
                writeln!(funcs, "}}").unwrap();
                decl.push_str(&block.decl);
                funcs.push_str(&block.funcs);
            }
        }
        Intermediate {
            ty: TypeInstance::Never,
            decl,
            funcs,
            effect,
            immediate: "".into(),
        }
    }

    pub fn block(&mut self, block: &Block) -> Intermediate {
        let len = self.stack.len();
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type");
        };
        self.last_infer += 1;
        let scope = Rc::clone(&self.scopes[self.last_scope]);
        self.last_scope += 1;
        self.stack.push(Rc::clone(&scope));
        self.block_stack.push(scope);
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let mut immediate = String::new();
        for statement in block.statement.iter() {
            let statement = self.statement(statement);
            decl.push_str(&statement.decl);
            funcs.push_str(&statement.funcs);
            effect.push_str(&statement.effect);
        }
        if let Some(result) = block.result.as_ref().map(|expr| self.expression(expr)) {
            decl.push_str(&result.decl);
            funcs.push_str(&result.funcs);
            effect.push_str(&result.effect);
            immediate = result.immediate;
        }
        self.stack.truncate(len);
        self.block_stack.pop();
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate,
        }
    }

    pub fn expression(&mut self, expr: &Expression) -> Intermediate {
        match expr {
            Expression::Block(block) => match block {
                BlockExpression::Block(block) => self.block(block),
                BlockExpression::If(if_) => self.if_(if_),
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Path(path) => self.path(path),
                NonblockExpression::Binary(binary) => self.binary(binary),
                NonblockExpression::Literal(literal) => self.literal(literal),
                NonblockExpression::Comparison(comparison) => self.comparison(comparison),
                NonblockExpression::Print(format) => self.print(format),
                NonblockExpression::Select(select) => self.select(select),
                NonblockExpression::MakeTuple(make_tuple) => self.make_tuple(make_tuple),
                NonblockExpression::MakeArray(make_array) => self.make_array(make_array),
                NonblockExpression::Parentheses(expr) => self.expression(expr),
                NonblockExpression::Invocation(invocation) => self.invocation(invocation),
                NonblockExpression::Declaration(declaration) => self.declaration(declaration),
                NonblockExpression::Assignment(assignment) => self.assignment(assignment),
                NonblockExpression::CompoundAssignment(compound) => {
                    self.compound_assignment(compound)
                }
                NonblockExpression::Index(index) => self.index(index),
            },
        }
    }

    pub fn translate(program: &Program) -> String {
        let name_resolver = NameResolver::new(program);
        let mut type_constructor = TypeConstructor::from_name_resolver(name_resolver);
        type_constructor.run(program);
        let mut type_checker = TypeChecker::from_type_constructor(type_constructor);
        type_checker.run(program);

        let mut translator = Codegen {
            feature: Default::default(),
            scopes: type_checker.scopes,
            inference: type_checker.inference,
            type_id: Default::default(),
            last_scope: 1,
            last_infer: 0,
            var_id: 0,
            stack: vec![],
            block_stack: vec![],
            decl: String::new(),
            structs: String::new(),
        };

        let intermediate = translator.block(&Block {
            statement: program.statement.clone(),
            result: None,
        });

        let mut out = String::new();
        writeln!(out, "// namul 0.0.0").unwrap();
        writeln!(out, "#pragma GCC diagnostic ignored \"-Wmain\"").unwrap();
        writeln!(out, "#pragma GCC diagnostic ignored \"-Wunused-variable\"").unwrap();
        writeln!(out, "#include <stdint.h>").unwrap();
        writeln!(out, "#include <unistd.h>").unwrap();
        out.push_str(include_str!("fragments/builtins.c"));
        if translator.feature.contains("writer_def") {
            out.push_str(include_str!("fragments/writer_struct.c"));
        }
        if translator.feature.contains("write_literal") {
            out.push_str(include_str!("fragments/write_literal.c"));
        }
        if translator.feature.contains("write_int") {
            out.push_str(include_str!("fragments/write_int.c"));
        }
        writeln!(out, "_Noreturn void halt() {{").unwrap();
        if translator.feature.contains("writer_def") {
            writeln!(out, "flush();").unwrap();
        }
        writeln!(out, "_exit(0);").unwrap();
        writeln!(out, "}}").unwrap();
        if translator.feature.contains("reader_def") {
            out.push_str(include_str!("fragments/reader_struct.c"));
        }
        if translator.feature.contains("read_white") {
            out.push_str(include_str!("fragments/read_white.c"));
        }
        if translator.feature.contains("read_int") {
            out.push_str(include_str!("fragments/read_int.c"));
        }
        out.push_str(&translator.decl);
        out.push_str(&intermediate.decl);
        out.push_str(&translator.structs);
        out.push_str(&intermediate.funcs);
        writeln!(out, "int main;").unwrap();
        writeln!(out, "int __libc_start_main() {{").unwrap();
        if translator.feature.contains("writer_def") {
            writeln!(out, "Writer w;").unwrap();
            writeln!(out, "w.back = 0;").unwrap();
            writeln!(out, "writer = &w;").unwrap();
        }
        if translator.feature.contains("reader_def") {
            writeln!(out, "Reader r;").unwrap();
            writeln!(out, "r.off = r.end = 0;").unwrap();
            writeln!(out, "reader = &r;").unwrap();
        }
        out.push_str(&intermediate.effect);
        writeln!(out, "halt();").unwrap();
        writeln!(out, "}}").unwrap();
        out
    }

    fn assign_pattern(
        &mut self,
        effect: &mut String,
        scope: &Scope,
        rhs: &mut String,
        args: &[Pattern],
    ) {
        for (i, arg) in args.iter().enumerate() {
            let len = rhs.len();
            write!(rhs, ".m{}", i).unwrap();
            match arg {
                Pattern::Ident(ident) => {
                    let var = scope.get_var(&ident.content).unwrap();
                    let Some(var_ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    self.assign_var(effect, &var_ty, &var.mangle, &rhs);
                }
                Pattern::Declaration(declaration) => {
                    let var = scope.get_var(&declaration.ident.content).unwrap();
                    let Some(var_ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    self.define_var(effect, &var_ty, &var.mangle, &rhs);
                }
                Pattern::Tuple(_) => todo!(),
            }
            rhs.truncate(len);
        }
    }

    fn invocation(&mut self, invocation: &Invocation) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let callee = self.expression(&invocation.callee);
        let len = self.stack.len();
        decl.push_str(&callee.decl);
        funcs.push_str(&callee.funcs);
        effect.push_str(&callee.effect);
        let mut call = String::new();
        let id = self.var_id;
        self.var_id += 1;
        let ident = format!("inv{id}");
        write!(call, "{}(", callee.immediate).unwrap();
        for (i, arg) in invocation.args.iter().enumerate() {
            if i > 0 {
                call.push_str(", ");
            }
            let arg = self.expression(arg);
            decl.push_str(&arg.decl);
            funcs.push_str(&arg.funcs);
            effect.push_str(&arg.effect);
            call.push_str(&arg.immediate);
        }
        self.stack.truncate(len);
        write!(call, ")").unwrap();
        if ty == TypeInstance::Never {
            effect.push_str(&call);
        } else {
            self.define_var(&mut effect, &ty, &ident, &call);
        }
        let immediate = if ty == TypeInstance::Never {
            "".into()
        } else {
            ident
        };
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate,
        }
    }

    fn declaration(&mut self, declaration: &Declaration) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut effect = String::new();
        let scope = Rc::clone(&self.scopes[self.last_scope]);
        self.stack.push(Rc::clone(&scope));
        self.last_scope += 1;
        let scope = scope.borrow();
        let var = scope.get_var(&declaration.ident.content).unwrap();
        let Some(var_ty) = self.synthesize(&var.ty.borrow()) else {
            panic!("Could not deduce type.");
        };
        writeln!(effect, "{};", &var_ty.declare(&var.mangle)).unwrap();
        Intermediate {
            ty,
            decl: "".into(),
            funcs: "".into(),
            effect,
            immediate: "".into(),
        }
    }

    fn assignment(&mut self, assignment: &Assignment) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut effect = String::new();
        let mut rhs = self.expression(&assignment.rhs);
        let mut decl = rhs.decl;
        let mut funcs = rhs.funcs;
        effect.push_str(&rhs.effect);
        let scope = Rc::clone(&self.scopes[self.last_scope]);
        self.last_scope += 1;
        self.stack.push(Rc::clone(&scope));
        self.assignee(
            &mut decl,
            &mut funcs,
            &mut effect,
            &mut rhs.immediate,
            &assignment.lhs,
        );
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate: "".into(),
        }
    }

    fn assignee(
        &mut self,
        decl: &mut String,
        funcs: &mut String,
        effect: &mut String,
        immediate: &mut String,
        assignee: &Assignee,
    ) {
        let scope = Rc::clone(&self.stack.last().unwrap());
        match assignee {
            Assignee::Declaration(declaration) => {
                let var = scope.borrow().get_var(&declaration.ident.content).unwrap();
                let Some(ty) = self.synthesize(&var.ty.borrow()) else {
                    panic!("Could not deduce type.");
                };
                self.define_var(effect, &ty, &var.mangle, &immediate);
            }
            Assignee::Path(path) => match path {
                Path::Simple(simple) => {
                    let var = scope.borrow().get_var(&simple.content).unwrap();
                    let Some(ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    self.assign_var(effect, &ty, &var.mangle, &immediate);
                }
            },
            Assignee::Tuple(args) => {
                let len = immediate.len();
                for (i, arg) in args.iter().enumerate() {
                    write!(immediate, ".m{i}").unwrap();
                    self.assignee(decl, funcs, effect, immediate, arg);
                    immediate.truncate(len);
                }
            }
            Assignee::Index(index) => {
                let len = self.stack.len();
                let target = self.expression(&index.target);
                decl.push_str(&target.decl);
                funcs.push_str(&target.funcs);
                effect.push_str(&target.effect);
                let idx = self.expression(&index.index);
                decl.push_str(&idx.decl);
                funcs.push_str(&idx.funcs);
                effect.push_str(&idx.effect);
                self.stack.truncate(len);
                let TypeInstance::Array { element, .. } = target.ty else {
                    panic!("Could not index into type `{}`.", target.ty);
                };
                self.assign_var(
                    effect,
                    &element,
                    &format!("{}.v[{}]", target.immediate, idx.immediate),
                    &immediate,
                );
            }
        }
    }

    fn assignee_input(
        &mut self,
        decl: &mut String,
        funcs: &mut String,
        effect: &mut String,
        assignee: &Assignee,
    ) {
        let scope = Rc::clone(&self.stack.last().unwrap());
        match assignee {
            Assignee::Declaration(declaration) => {
                let var = scope.borrow().get_var(&declaration.ident.content).unwrap();
                let Some(ty) = self.synthesize(&var.ty.borrow()) else {
                    panic!("Could not deduce type.");
                };
                let getter = match ty {
                    TypeInstance::I32 | TypeInstance::I64 => {
                        self.feature.insert("reader_def");
                        self.feature.insert("read_white");
                        self.feature.insert("read_int");
                        "read_int()"
                    }
                    _ => panic!("Could not find parser for type `{ty}`."),
                };
                self.define_var(effect, &ty, &var.mangle, &getter);
            }
            Assignee::Path(path) => match path {
                Path::Simple(simple) => {
                    let var = scope.borrow().get_var(&simple.content).unwrap();
                    let Some(ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    let getter = match ty {
                        TypeInstance::I32 | TypeInstance::I64 => {
                            self.feature.insert("reader_def");
                            self.feature.insert("read_white");
                            self.feature.insert("read_int");
                            "read_int()"
                        }
                        _ => panic!("Could not find parser for type `{ty}`."),
                    };
                    self.assign_var(effect, &ty, &var.mangle, &getter);
                }
            },
            Assignee::Tuple(args) => {
                for arg in args.iter() {
                    self.assignee_input(decl, funcs, effect, arg);
                }
            }
            Assignee::Index(index) => {
                let len = self.stack.len();
                let target = self.expression(&index.target);
                decl.push_str(&target.decl);
                funcs.push_str(&target.funcs);
                effect.push_str(&target.effect);
                let index = self.expression(&index.index);
                decl.push_str(&index.decl);
                funcs.push_str(&index.funcs);
                effect.push_str(&index.effect);
                self.stack.truncate(len);
                let TypeInstance::Array { element, .. } = target.ty else {
                    panic!("Could not index into type `{}`.", target.ty);
                };
                let getter = match &*element {
                    TypeInstance::I32 | TypeInstance::I64 => {
                        self.feature.insert("reader_def");
                        self.feature.insert("read_white");
                        self.feature.insert("read_int");
                        "read_int()"
                    }
                    _ => panic!("Could not find parser for type `{element}`."),
                };
                self.assign_var(
                    effect,
                    &element,
                    &format!("{}[{}]", target.immediate, index.immediate),
                    &getter,
                );
            }
        }
    }

    fn compound_assignment(&mut self, compound: &CompoundAssignment) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let len = self.stack.len();
        let rhs = self.expression(&compound.rhs);
        self.stack.truncate(len);
        let mut decl = rhs.decl;
        let mut funcs = rhs.funcs;
        let mut effect = rhs.effect;
        let scope = Rc::clone(self.stack.last().unwrap());
        match &compound.lhs {
            Place::Path(path) => match path {
                Path::Simple(simple) => {
                    let var = scope.borrow().get_var(&simple.content).unwrap();
                    let Some(ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    match (&ty, compound.op.kind, &rhs.ty) {
                        (
                            TypeInstance::I32 | TypeInstance::I64,
                            _,
                            TypeInstance::I32 | TypeInstance::I64,
                        ) => {
                            writeln!(
                                effect,
                                "{} {} {};",
                                var.mangle, compound.op.content, rhs.immediate
                            )
                            .unwrap();
                        }
                        _ => panic!(
                            "Could not assign type `{}` to type `{ty}` using `{}`.",
                            rhs.ty, compound.op.content
                        ),
                    }
                }
            },
            Place::Index(index) => {
                let len = self.stack.len();
                let target = self.expression(&index.target);
                decl.push_str(&target.decl);
                funcs.push_str(&target.funcs);
                effect.push_str(&target.effect);
                let index = self.expression(&index.index);
                decl.push_str(&index.decl);
                funcs.push_str(&index.funcs);
                effect.push_str(&index.effect);
                self.stack.truncate(len);
                let TypeInstance::Array { element, .. } = target.ty else {
                    panic!("Could not index into type `{}`.", target.ty);
                };
                match (&*element, compound.op.kind, &rhs.ty) {
                    (
                        TypeInstance::I32 | TypeInstance::I64,
                        _,
                        TypeInstance::I32 | TypeInstance::I64,
                    ) => {
                        writeln!(
                            effect,
                            "{}.v[{}] {} {};",
                            target.immediate, index.immediate, compound.op.content, rhs.immediate
                        )
                        .unwrap();
                    }
                    _ => panic!(
                        "Could not assign type `{}` to type `{element}` using `{}`.",
                        rhs.ty, compound.op.content
                    ),
                }
            }
        }
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate: "".into(),
        }
    }

    fn assign_var(&mut self, out: &mut String, _ty: &TypeInstance, lhs: &str, rhs: &str) {
        writeln!(out, "{} = {};", lhs, rhs).unwrap();
    }

    fn define_var(&mut self, out: &mut String, ty: &TypeInstance, lhs: &str, rhs: &str) {
        writeln!(out, "{} = {};", ty.declare(lhs), rhs).unwrap();
    }

    fn index(&mut self, index: &Index) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let len = self.stack.len();
        let mut decl = String::new();
        let mut funcs = String::new();
        let mut effect = String::new();
        let target = self.expression(&index.target);
        decl.push_str(&target.decl);
        funcs.push_str(&target.funcs);
        effect.push_str(&target.effect);
        let index = self.expression(&index.index);
        decl.push_str(&index.decl);
        funcs.push_str(&index.funcs);
        effect.push_str(&index.effect);
        self.stack.truncate(len);
        let immediate = if ty != TypeInstance::Never {
            format!("{}.v[{}]", target.immediate, index.immediate)
        } else {
            "".into()
        };
        Intermediate {
            ty,
            decl,
            funcs,
            effect,
            immediate,
        }
    }
}

fn double_quote(s: &str) -> String {
    let mut out = String::new();
    out.push('"');
    for byte in s.bytes() {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            0x07 => out.push_str("\\a"),
            0x08 => out.push_str("\\b"),
            0x0c => out.push_str("\\f"),
            b'\n' => out.push_str("\\n"),
            b'\r' => out.push_str("\\r"),
            b'\t' => out.push_str("\\t"),
            0x0b => out.push_str("\\v"),
            b' '..=b'~' => out.push(byte as char),
            _ => write!(out, "\\x{byte:02x}").unwrap(),
        }
    }
    out.push('"');
    out
}
