use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::syntax::{
    expr::{Block, BlockExpression, Expression, NonblockExpression},
    item::Type,
    literal::Literal,
    path::Path,
    stmt::{Pattern, Statement},
    Program,
};

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    var: HashMap<String, Variable>,
    ty: HashMap<String, TypeInstance>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInstance {
    I32,
    Unit,
    Bool,
    Tuple { id: usize, args: Vec<TypeInstance> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInference {
    Exact(TypeInstance),
    Tuple(Vec<Rc<RefCell<TypeInference>>>),
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
            _ if self == other => return,
            (Exact(TypeInstance::I32), Integer) | (Integer, Exact(TypeInstance::I32)) => {
                Exact(TypeInstance::I32)
            }
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
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub mangle: String,
    pub ty: Rc<RefCell<TypeInference>>,
}

impl Scope {
    pub fn root() -> Self {
        let mut ty = HashMap::new();
        ty.insert("i32".into(), TypeInstance::I32);
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

pub struct NameResolver {
    scopes: Vec<Rc<RefCell<Scope>>>,
    var_id: usize,
}

impl NameResolver {
    pub fn new(program: &Program) -> Self {
        let root = Rc::new(RefCell::new(Scope::root()));
        let mut resolver = NameResolver {
            scopes: vec![Rc::clone(&root)],
            var_id: 0,
        };
        resolver.block(
            root,
            &Block {
                statement: program.statement.clone(),
                result: None,
            },
        );
        resolver
    }

    pub fn statement(
        &mut self,
        parent: Rc<RefCell<Scope>>,
        stmt: &Statement,
    ) -> Rc<RefCell<Scope>> {
        let scope = Scope::with_parent(Rc::clone(&parent));
        let owned = Rc::new(RefCell::new(scope));
        self.scopes.push(Rc::clone(&owned));
        match stmt {
            Statement::Nop => {}
            Statement::Input(parser) => {
                for (_, name) in parser.arg.iter() {
                    owned.borrow_mut().var.insert(
                        name.content.clone(),
                        Variable {
                            name: name.content.clone(),
                            mangle: format!("_{}_{}", name.content, self.var_id),
                            ty: Rc::new(RefCell::new(TypeInference::Unknown)),
                        },
                    );
                    self.var_id += 1;
                }
            }
            Statement::Expression(expr) => {
                self.expression(Rc::clone(&owned), expr);
            }
            Statement::Assignment(assignment) => {
                self.expression(Rc::clone(&parent), &assignment.rhs);
                self.pattern(Rc::clone(&owned), &assignment.lhs);
            }
            Statement::Repeat(repeat) => {
                self.expression(Rc::clone(&owned), &repeat.times);
                self.block(Rc::clone(&owned), &repeat.block);
            }
            Statement::While(while_) => {
                self.expression(Rc::clone(&owned), &while_.condition);
                self.block(Rc::clone(&owned), &while_.block);
            }
        }
        owned
    }

    pub fn expression(
        &mut self,
        parent: Rc<RefCell<Scope>>,
        expr: &Expression,
    ) -> Rc<RefCell<Scope>> {
        match expr {
            Expression::Block(block) => match block {
                BlockExpression::Block(block) => {
                    self.block(parent.clone(), block);
                }
                BlockExpression::If(if_) => {
                    self.expression(parent.clone(), &if_.condition);
                    self.block(parent.clone(), &if_.truthy);
                    if let Some(falsy) = &if_.falsy {
                        self.block(parent.clone(), falsy);
                    }
                }
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Literal(_) => {}
                NonblockExpression::Path(_) => {}
                NonblockExpression::Binary(binary) => {
                    self.expression(parent.clone(), &binary.lhs);
                    self.expression(parent.clone(), &binary.rhs);
                }
                NonblockExpression::Comparison(comparison) => {
                    self.expression(parent.clone(), &comparison.first);
                    for (_, rest) in comparison.chain.iter() {
                        self.expression(parent.clone(), rest);
                    }
                }
                NonblockExpression::Print(_) => {}
                NonblockExpression::Select(select) => {
                    self.expression(parent.clone(), &select.condition);
                    self.expression(parent.clone(), &select.truthy);
                    self.expression(parent.clone(), &select.falsy);
                }
                NonblockExpression::MakeTuple(make_tuple) => {
                    for arg in make_tuple.args.iter() {
                        self.expression(parent.clone(), arg);
                    }
                }
            },
        }
        parent
    }

    pub fn block(&mut self, parent: Rc<RefCell<Scope>>, block: &Block) -> Rc<RefCell<Scope>> {
        let scope = Scope::with_parent(parent);
        let mut owned = Rc::new(RefCell::new(scope));
        self.scopes.push(Rc::clone(&owned));
        let result = Rc::clone(&owned);
        for stmt in block.statement.iter() {
            owned = self.statement(owned, stmt);
        }
        if let Some(result) = &block.result {
            self.expression(owned, &result);
        }
        result
    }

    fn pattern(&mut self, parent: Rc<RefCell<Scope>>, pattern: &Pattern) -> Rc<RefCell<Scope>> {
        match pattern {
            Pattern::Ident(_) => {}
            Pattern::Declaration(decl) => {
                parent.borrow_mut().var.insert(
                    decl.ident.content.clone(),
                    Variable {
                        name: decl.ident.content.clone(),
                        mangle: format!("_{}_{}", decl.ident.content, self.var_id),
                        ty: Rc::new(RefCell::new(TypeInference::Unknown)),
                    },
                );
                self.var_id += 1;
            }
            Pattern::Tuple(args) => {
                for arg in args.iter() {
                    self.pattern(Rc::clone(&parent), arg);
                }
            }
        }
        parent
    }
}

pub struct TypeChecker {
    pub scopes: Vec<Rc<RefCell<Scope>>>,
    pub inference: Vec<Rc<RefCell<TypeInference>>>,
    current: usize,
    tuple_id: usize,
}

impl TypeChecker {
    pub fn from_name_resolver(resolver: NameResolver) -> Self {
        Self {
            scopes: resolver.scopes,
            current: 0,
            inference: vec![],
            tuple_id: 0,
        }
    }

    pub fn run(&mut self, program: &Program) {
        self.block(&Block {
            statement: program.statement.clone(),
            result: None,
        });
    }

    fn statement(&mut self, stmt: &Statement) {
        self.current += 1;
        let scope = Rc::clone(&self.scopes[self.current]);
        match stmt {
            Statement::Nop => {}
            Statement::Input(parser) => {
                for (ty, name) in parser.arg.iter() {
                    let scope = scope.borrow_mut();
                    let Some(ty) = scope.get_ty(&ty.content) else {
                        panic!("Could not find type `{}` in scope.", ty.content);
                    };
                    let mut constraint = TypeInference::Exact(ty.clone());
                    let var = scope.get_var(&name.content).unwrap();
                    let mut var_ty = var.ty.borrow_mut();
                    var_ty.unify(&mut constraint);
                }
            }
            Statement::Expression(expr) => {
                self.expression(expr);
            }
            Statement::Assignment(assignment) => {
                self.current -= 1;
                let rhs = self.expression(&assignment.rhs);
                self.current += 1;
                let mut rhs_ty = rhs.borrow_mut();
                let pattern = self.pattern(&assignment.lhs);
                rhs_ty.unify(&mut pattern.borrow_mut());
            }
            Statement::Repeat(repeat) => {
                self.expression(&repeat.times);
                self.block(&repeat.block);
            }
            Statement::While(while_) => {
                self.expression(&while_.condition);
                self.block(&while_.block);
            }
        }
        self.current -= 1;
    }

    fn expression(&mut self, expr: &Expression) -> Rc<RefCell<TypeInference>> {
        let scope = Rc::clone(&self.scopes[self.current]);
        let ty = Rc::new(RefCell::new(TypeInference::Unknown));
        self.inference.push(Rc::clone(&ty));
        match expr {
            Expression::Block(block) => match block {
                BlockExpression::Block(block) => {
                    let block = self.block(block);
                    let mut block_ty = block.borrow_mut();
                    block_ty.unify(&mut ty.borrow_mut());
                }
                BlockExpression::If(if_) => {
                    let condition = self.expression(&if_.condition);
                    let mut condition_ty = condition.borrow_mut();
                    condition_ty.unify(&mut TypeInference::Exact(TypeInstance::Bool));
                    let truthy = self.block(&if_.truthy);
                    let mut truthy_ty = truthy.borrow_mut();
                    if let Some(falsy) = &if_.falsy {
                        let falsy = self.block(falsy);
                        let mut falsy_ty = falsy.borrow_mut();
                        truthy_ty.unify(&mut falsy_ty);
                    } else {
                        truthy_ty.unify(&mut TypeInference::Exact(TypeInstance::Unit));
                    }
                    truthy_ty.unify(&mut ty.borrow_mut());
                }
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Literal(literal) => match literal {
                    Literal::Decimal(_) => {
                        TypeInference::Integer.unify(&mut ty.borrow_mut());
                    }
                    Literal::Bool(_) => {
                        TypeInference::Exact(TypeInstance::Bool).unify(&mut ty.borrow_mut());
                    }
                },
                NonblockExpression::Path(path) => match path {
                    Path::Simple(simple) => {
                        let scope = scope.borrow();
                        let Some(var) = scope.get_var(&simple.content) else {
                            panic!("Could not find name {} in scope.", simple.content);
                        };
                        let mut var_ty = var.ty.borrow_mut();
                        var_ty.unify(&mut ty.borrow_mut());
                    }
                },
                NonblockExpression::Binary(binary) => {
                    let lhs = self.expression(&binary.lhs);
                    let rhs = self.expression(&binary.rhs);
                    let mut lhs_ty = lhs.borrow_mut();
                    let mut rhs_ty = rhs.borrow_mut();
                    lhs_ty.unify(&mut rhs_ty);
                    lhs_ty.unify(&mut ty.borrow_mut());
                }
                NonblockExpression::Comparison(comparison) => {
                    let mut prev = self.expression(&comparison.first);
                    for (_, rest) in comparison.chain.iter() {
                        let next = self.expression(rest);
                        {
                            let mut prev_ty = prev.borrow_mut();
                            let mut next_ty = next.borrow_mut();
                            prev_ty.unify(&mut next_ty);
                        }
                        prev = next;
                    }
                    TypeInference::Exact(TypeInstance::Bool).unify(&mut ty.borrow_mut());
                }
                NonblockExpression::Print(_) => {
                    TypeInference::Exact(TypeInstance::Unit).unify(&mut ty.borrow_mut());
                }
                NonblockExpression::Select(select) => {
                    let condition = self.expression(&select.condition);
                    let mut condition_ty = condition.borrow_mut();
                    condition_ty.unify(&mut TypeInference::Exact(TypeInstance::Bool));
                    let truthy = self.expression(&select.truthy);
                    let mut truthy_ty = truthy.borrow_mut();
                    let falsy = self.expression(&select.falsy);
                    let mut falsy_ty = falsy.borrow_mut();
                    truthy_ty.unify(&mut falsy_ty);
                    truthy_ty.unify(&mut ty.borrow_mut());
                }
                NonblockExpression::MakeTuple(make_tuple) => {
                    let mut args = vec![];
                    for arg in make_tuple.args.iter() {
                        args.push(self.expression(&arg));
                    }
                    TypeInference::Tuple(args).unify(&mut ty.borrow_mut())
                }
            },
        };
        ty
    }

    fn block(&mut self, block: &Block) -> Rc<RefCell<TypeInference>> {
        let ty = Rc::new(RefCell::new(TypeInference::Unknown));
        self.inference.push(Rc::clone(&ty));
        let result = self.current;
        self.current += 1;
        for stmt in block.statement.iter() {
            self.statement(stmt);
            self.current += 1;
        }
        if let Some(result) = &block.result {
            let expr = self.expression(&result);
            let mut expr_ty = expr.borrow_mut();
            expr_ty.unify(&mut ty.borrow_mut());
        } else {
            TypeInference::Exact(TypeInstance::Unit).unify(&mut ty.borrow_mut());
        };
        self.current = result;
        ty
    }

    fn construct_type(&mut self, scope: &Scope, ty: &Type) -> TypeInstance {
        match ty {
            Type::Path(path) => {
                let Some(ty) = scope.get_ty(&path.ident.content) else {
                    panic!("Could not find type `{}` in scope.", path.ident.content);
                };
                ty
            }
            Type::Tuple(args) => TypeInstance::Tuple {
                id: self.tuple_id,
                args: args
                    .iter()
                    .map(|ty| self.construct_type(scope, ty))
                    .collect(),
            },
        }
    }

    fn pattern(&mut self, pattern: &Pattern) -> Rc<RefCell<TypeInference>> {
        let scope = Rc::clone(&self.scopes[self.current]);
        match pattern {
            Pattern::Ident(ident) => {
                let scope = scope.borrow();
                let var = scope.get_var(&ident.content).unwrap();
                Rc::clone(&var.ty)
            }
            Pattern::Declaration(decl) => {
                let scope = scope.borrow();
                let mut decl_ty = TypeInference::Exact(self.construct_type(&scope, &decl.ty));
                let var = scope.get_var(&decl.ident.content).unwrap();
                let mut var_ty = var.ty.borrow_mut();
                var_ty.unify(&mut decl_ty);
                Rc::clone(&var.ty)
            }
            Pattern::Tuple(args) => Rc::new(RefCell::new(TypeInference::Tuple(
                args.iter().map(|arg| self.pattern(arg)).collect(),
            ))),
        }
    }
}
