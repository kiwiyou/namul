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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeInstance {
    I32,
    Unit,
    Bool,
}

impl Display for TypeInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInstance::I32 => f.write_str("i32"),
            TypeInstance::Unit => f.write_str("()"),
            TypeInstance::Bool => f.write_str("bool"),
        }
    }
}

impl TypeInstance {
    pub fn mapped(&self) -> String {
        match self {
            TypeInstance::I32 => "int32_t".into(),
            TypeInstance::Unit => "void".into(),
            TypeInstance::Bool => "char".into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub mangle: String,
    pub ty: TypeDeduction,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDeduction {
    Integer,
    Exact(TypeInstance),
    Unknown,
    Never,
}

impl TypeDeduction {
    pub fn deduce(&self) -> Option<TypeInstance> {
        match self {
            TypeDeduction::Integer => Some(TypeInstance::I32),
            TypeDeduction::Exact(exact) => Some(exact.clone()),
            TypeDeduction::Unknown => None,
            TypeDeduction::Never => None,
        }
    }
}

impl TypeDeduction {
    pub fn unify(&mut self, other: Self) {
        if *self == other {
            return;
        }
        use TypeDeduction::*;
        match (&self, other) {
            (_, Never) | (Never, _) => *self = Never,
            (Integer, Exact(exact)) => match exact {
                TypeInstance::I32 => *self = Exact(exact),
                _ => *self = Never,
            },
            (Exact(_), Integer) => {}
            (Unknown, other) => *self = other,
            _ => *self = Never,
        };
    }
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

struct NameResolver {
    scopes: Vec<Rc<RefCell<Scope>>>,
    counter: usize,
}

impl NameResolver {
    pub fn new(program: &Program) -> Self {
        let root = Rc::new(RefCell::new(Scope::root()));
        let mut resolver = NameResolver {
            scopes: vec![Rc::clone(&root)],
            counter: 0,
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
        let scope = Scope::with_parent(parent);
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
                            mangle: format!("_{}_{}", name.content, self.counter),
                            ty: TypeDeduction::Unknown,
                        },
                    );
                    self.counter += 1;
                }
            }
            Statement::Expression(expr) => {
                self.expression(Rc::clone(&owned), expr);
            }
            Statement::Print(_) => {}
            Statement::Assignment(assignment) => match &assignment.lhs {
                Pattern::Ident(_) => {}
                Pattern::Declaration(decl) => {
                    owned.borrow_mut().var.insert(
                        decl.ident.content.clone(),
                        Variable {
                            name: decl.ident.content.clone(),
                            mangle: format!("_{}_{}", decl.ident.content, self.counter),
                            ty: TypeDeduction::Unknown,
                        },
                    );
                    self.counter += 1;
                }
            },
            Statement::Repeat(repeat) => {
                self.expression(Rc::clone(&owned), &repeat.times);
                self.block(Rc::clone(&owned), &repeat.block);
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
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Literal(_) => {}
                NonblockExpression::Path(_) => {}
                NonblockExpression::Binary(binary) => {
                    self.expression(parent.clone(), &binary.lhs);
                    self.expression(parent.clone(), &binary.rhs);
                }
                NonblockExpression::If(if_) => {
                    self.expression(parent.clone(), &if_.condition);
                    self.expression(parent.clone(), &if_.truthy);
                    if let Some(falsy) = &if_.falsy {
                        self.expression(parent.clone(), falsy);
                    }
                }
                NonblockExpression::Comparison(comparison) => {
                    self.expression(parent.clone(), &comparison.first);
                    for (_, rest) in comparison.chain.iter() {
                        self.expression(parent.clone(), rest);
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
}

pub struct TypeChecker {
    pub scopes: Vec<Rc<RefCell<Scope>>>,
    pub expr_types: Vec<TypeDeduction>,
    current: usize,
}

impl TypeChecker {
    pub fn run(program: &Program) -> Self {
        let resolver = NameResolver::new(program);
        let mut checker = Self {
            scopes: resolver.scopes,
            expr_types: vec![],
            current: 0,
        };
        checker.block(&Block {
            statement: program.statement.clone(),
            result: None,
        });
        checker
    }

    fn statement(&mut self, stmt: &Statement) {
        self.current += 1;
        let scope = Rc::clone(&self.scopes[self.current]);
        match stmt {
            Statement::Nop => {}
            Statement::Input(parser) => {
                for (ty, name) in parser.arg.iter() {
                    let mut scope = scope.borrow_mut();
                    let Some(ty) = scope.get_ty(&ty.content) else {
                        panic!("Could not find type `{}` in scope.", ty.content);
                    };
                    let constraint = TypeDeduction::Exact(ty.clone());
                    let var = scope.var.get_mut(&name.content).unwrap();
                    var.ty.unify(constraint);
                }
            }
            Statement::Expression(expr) => {
                self.expression(expr);
            }
            Statement::Print(_) => {}
            Statement::Assignment(assignment) => {
                self.expression(&assignment.rhs);
                match &assignment.lhs {
                    Pattern::Ident(_) => {}
                    Pattern::Declaration(decl) => match &decl.ty {
                        Type::Path(path) => {
                            let mut scope = scope.borrow_mut();
                            let Some(ty) = scope.get_ty(&path.ident.content) else {
                                panic!("Could not find type `{}` in scope.", path.ident.content);
                            };
                            let constraint = TypeDeduction::Exact(ty.clone());
                            let var = scope.var.get_mut(&decl.ident.content).unwrap();
                            var.ty.unify(constraint);
                        }
                        Type::Tuple(_) => todo!(),
                    },
                }
            }
            Statement::Repeat(repeat) => {
                self.expression(&repeat.times);
                self.block(&repeat.block);
            }
        }
        self.current -= 1;
    }

    fn expression(&mut self, expr: &Expression) -> TypeDeduction {
        let scope = Rc::clone(&self.scopes[self.current]);
        let pos = self.expr_types.len();
        self.expr_types.push(TypeDeduction::Unknown);
        let deduction = match expr {
            Expression::Block(block) => match block {
                BlockExpression::Block(block) => self.block(block),
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Literal(literal) => match literal {
                    Literal::Decimal(_) => TypeDeduction::Integer,
                    Literal::Bool(_) => TypeDeduction::Exact(TypeInstance::Bool),
                },
                NonblockExpression::Path(path) => match path {
                    Path::Simple(simple) => {
                        let scope = scope.borrow();
                        let Some(var) = scope.get_var(&simple.content) else {
                            panic!("Could not find name {} in scope.", simple.content);
                        };
                        var.ty.clone()
                    }
                },
                NonblockExpression::Binary(binary) => {
                    let lhs = self.expression(&binary.lhs);
                    let rhs = self.expression(&binary.rhs);
                    match (lhs.deduce(), rhs.deduce()) {
                        (Some(TypeInstance::I32), Some(TypeInstance::I32)) => {
                            TypeDeduction::Exact(TypeInstance::I32)
                        }
                        _ => TypeDeduction::Never,
                    }
                }
                NonblockExpression::If(if_) => {
                    let condition = self.expression(&if_.condition);
                    if condition.deduce() != Some(TypeInstance::Bool) {
                        TypeDeduction::Never
                    } else {
                        let truthy = self.expression(&if_.truthy);
                        if let Some(falsy) = if_.falsy.as_ref() {
                            let falsy = self.expression(&falsy);
                            if matches!((truthy.deduce(), falsy.deduce()), (Some(t), Some(f)) if t == f)
                            {
                                truthy.clone()
                            } else {
                                TypeDeduction::Never
                            }
                        } else {
                            TypeDeduction::Exact(TypeInstance::Unit)
                        }
                    }
                }
                NonblockExpression::Comparison(comparison) => {
                    let mut prev = self.expression(&comparison.first);
                    let mut result = TypeDeduction::Exact(TypeInstance::Bool);
                    for (_, rest) in comparison.chain.iter() {
                        let next = self.expression(rest);
                        if !matches!((prev.deduce(), next.deduce()), (Some(t), Some(f)) if t == f) {
                            result = TypeDeduction::Never;
                        }
                        prev = next;
                    }
                    result
                }
            },
        };
        self.expr_types[pos] = deduction.clone();
        deduction
    }

    fn block(&mut self, block: &Block) -> TypeDeduction {
        let pos = self.expr_types.len();
        self.expr_types.push(TypeDeduction::Unknown);
        let result = self.current;
        self.current += 1;
        for stmt in block.statement.iter() {
            self.statement(stmt);
            self.current += 1;
        }
        let ty = if let Some(result) = &block.result {
            self.expression(&result)
        } else {
            TypeDeduction::Exact(TypeInstance::Unit)
        };
        self.current = result;
        self.expr_types[pos] = ty.clone();
        ty
    }
}
