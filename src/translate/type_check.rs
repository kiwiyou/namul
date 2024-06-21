use std::{cell::RefCell, rc::Rc};

use crate::syntax::{
    expression::{Assignee, Block, BlockExpression, Expression, NonblockExpression, Place},
    literal::Literal,
    path::Path,
    statement::{Pattern, Statement},
    Program,
};

use super::{type_construct::TypeConstructor, Scope, TypeInference, TypeInstance};

pub struct TypeChecker {
    pub scopes: Vec<Rc<RefCell<Scope>>>,
    pub inference: Vec<Rc<RefCell<TypeInference>>>,
    stack: Vec<Rc<RefCell<Scope>>>,
    block_stack: Vec<Rc<RefCell<Scope>>>,
    last: usize,
}

impl TypeChecker {
    pub fn from_type_constructor(constructor: TypeConstructor) -> Self {
        Self {
            scopes: constructor.scopes,
            inference: vec![],
            stack: vec![],
            block_stack: vec![],
            last: 1usize,
        }
    }

    pub fn run(&mut self, program: &Program) {
        self.block(&Block {
            statement: program.statement.clone(),
            result: None,
        });
    }

    fn statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Nop => {}
            Statement::Input(input) => {
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                self.stack.push(scope);
                for arg in input.arg.iter() {
                    self.assignee(arg);
                }
            }
            Statement::Expression(expr) => {
                self.expression(expr);
            }
            Statement::Repeat(repeat) => {
                let len = self.stack.len();
                let ty = self.expression(&repeat.times);
                if let Some(ident) = &repeat.var {
                    let scope = Rc::clone(&self.scopes[self.last]);
                    self.last += 1;
                    self.stack.push(Rc::clone(&scope));
                    let var = scope.borrow().get_var(&ident.content).unwrap();
                    TypeInference::unify(&ty, &var.ty);
                }
                self.block(&repeat.block);
                self.stack.truncate(len);
            }
            Statement::While(while_) => {
                let len = self.stack.len();
                self.expression(&while_.condition);
                self.block(&while_.block);
                self.stack.truncate(len);
            }
            Statement::Function(function) => {
                let block = Rc::clone(&self.block_stack.last().unwrap());
                let var = block.borrow().get_var(&function.ident.content).unwrap();
                let mut args = vec![];
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                self.stack.push(scope);
                for arg in function.args.iter() {
                    args.push(self.pattern(arg));
                }
                let result = self.block(&function.block);
                let actual = Rc::new(RefCell::new(TypeInference::Function { args, result }));
                TypeInference::unify(&actual, &var.ty);
                self.stack.pop();
            }
        }
    }

    fn expression(&mut self, expr: &Expression) -> Rc<RefCell<TypeInference>> {
        let ty = Rc::new(RefCell::new(TypeInference::Unknown));
        self.inference.push(Rc::clone(&ty));
        match expr {
            Expression::Block(block) => match block {
                BlockExpression::Block(block) => {
                    let block = self.block(block);
                    TypeInference::unify(&block, &ty);
                }
                BlockExpression::If(if_) => {
                    let len = self.stack.len();
                    let condition = self.expression(&if_.condition);
                    TypeInference::unify(
                        &condition,
                        &Rc::new(RefCell::new(TypeInference::Exact(TypeInstance::Bool))),
                    );
                    let truthy = self.block(&if_.truthy);
                    if let Some(falsy) = &if_.falsy {
                        let falsy = self.block(falsy);
                        TypeInference::unify(&truthy, &falsy);
                    } else {
                        TypeInference::unify(&truthy, &Rc::new(RefCell::new(TypeInference::Never)));
                    }
                    TypeInference::unify(&truthy, &ty);
                    self.stack.truncate(len);
                }
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Literal(literal) => match literal {
                    Literal::Decimal(_) => {
                        TypeInference::unify(&ty, &Rc::new(RefCell::new(TypeInference::Integer)));
                    }
                    Literal::Bool(_) => {
                        TypeInference::unify(
                            &ty,
                            &Rc::new(RefCell::new(TypeInference::Exact(TypeInstance::Bool))),
                        );
                    }
                },
                NonblockExpression::Path(path) => match path {
                    Path::Simple(simple) => {
                        let scope = Rc::clone(&self.stack.last().unwrap());
                        let scope = scope.borrow();
                        let Some(var) = scope.get_var(&simple.content) else {
                            panic!("Could not find name {} in scope.", simple.content);
                        };
                        TypeInference::unify(&ty, &var.ty);
                    }
                },
                NonblockExpression::Binary(binary) => {
                    let lhs = self.expression(&binary.lhs);
                    let rhs = self.expression(&binary.rhs);
                    TypeInference::unify(&rhs, &lhs);
                    TypeInference::unify(&ty, &lhs);
                }
                NonblockExpression::Comparison(comparison) => {
                    let mut prev = self.expression(&comparison.first);
                    for (_, rest) in comparison.chain.iter() {
                        let next = self.expression(rest);
                        TypeInference::unify(&prev, &next);
                        prev = next;
                    }
                    TypeInference::unify(
                        &ty,
                        &Rc::new(RefCell::new(TypeInference::Exact(TypeInstance::Bool))),
                    );
                }
                NonblockExpression::Print(print) => {
                    let len = self.stack.len();
                    for arg in print.args.iter() {
                        self.expression(arg);
                    }
                    self.stack.truncate(len);
                    TypeInference::unify(&ty, &Rc::new(RefCell::new(TypeInference::Never)));
                }
                NonblockExpression::Select(select) => {
                    let len = self.stack.len();
                    let condition = self.expression(&select.condition);
                    let inside = self.stack.len();
                    TypeInference::unify(
                        &condition,
                        &Rc::new(RefCell::new(TypeInference::Exact(TypeInstance::Bool))),
                    );
                    let truthy = self.expression(&select.truthy);
                    self.stack.truncate(inside);
                    let falsy = self.expression(&select.falsy);
                    TypeInference::unify(&truthy, &falsy);
                    if *condition.borrow() == TypeInference::Never {
                        TypeInference::unify(
                            &condition,
                            &Rc::new(RefCell::new(TypeInference::Never)),
                        );
                    } else {
                        TypeInference::unify(&ty, &truthy);
                    }
                    self.stack.truncate(len);
                }
                NonblockExpression::MakeTuple(make_tuple) => {
                    let mut args = vec![];
                    for arg in make_tuple.args.iter() {
                        args.push(self.expression(&arg));
                    }
                    TypeInference::unify(&ty, &Rc::new(RefCell::new(TypeInference::Tuple(args))));
                }
                NonblockExpression::Parentheses(expr) => {
                    self.inference.pop();
                    let expr = self.expression(&expr);
                    TypeInference::unify(&ty, &expr);
                }
                NonblockExpression::Invocation(invocation) => {
                    let func = self.expression(&invocation.callee);
                    let len = self.stack.len();
                    let mut args = vec![];
                    for arg in invocation.args.iter() {
                        args.push(Rc::clone(&self.expression(arg)));
                    }
                    self.stack.truncate(len);
                    TypeInference::unify(
                        &func,
                        &Rc::new(RefCell::new(TypeInference::Function {
                            args,
                            result: Rc::clone(&ty),
                        })),
                    );
                }
                NonblockExpression::Declaration(_) => {
                    let scope = Rc::clone(&self.scopes[self.last]);
                    self.last += 1;
                    self.stack.push(scope);
                    TypeInference::unify(&ty, &Rc::new(RefCell::new(TypeInference::Never)));
                }
                NonblockExpression::Assignment(assignment) => {
                    let rhs = self.expression(&assignment.rhs);
                    let scope = Rc::clone(&self.scopes[self.last]);
                    self.last += 1;
                    self.stack.push(scope);
                    let lhs = self.assignee(&assignment.lhs);
                    TypeInference::unify(&lhs, &rhs);
                    TypeInference::unify(&ty, &Rc::new(RefCell::new(TypeInference::Never)));
                }
                NonblockExpression::CompoundAssignment(compound) => {
                    let len = self.stack.len();
                    let rhs = self.expression(&compound.rhs);
                    self.stack.truncate(len);
                    match &compound.lhs {
                        Place::Path(path) => match path {
                            Path::Simple(simple) => {
                                let scope = Rc::clone(self.stack.last().unwrap());
                                let var = scope.borrow().get_var(&simple.content).unwrap();
                                TypeInference::unify(&var.ty, &rhs);
                            }
                        },
                        Place::Index(index) => {
                            let len = self.stack.len();
                            let target = self.expression(&index.target);
                            let index = self.expression(&index.index);
                            self.stack.truncate(len);
                            let target_ty = target.borrow();
                            match &*target_ty {
                                TypeInference::Array { element, .. } => {
                                    TypeInference::unify(
                                        &index,
                                        &Rc::new(RefCell::new(TypeInference::Integer)),
                                    );
                                    TypeInference::unify(&ty, &element);
                                }
                                _ => TypeInference::unify(
                                    &ty,
                                    &Rc::new(RefCell::new(TypeInference::Error)),
                                ),
                            }
                        }
                    }
                    TypeInference::unify(&ty, &Rc::new(RefCell::new(TypeInference::Never)));
                }
                NonblockExpression::Index(index) => {
                    let len = self.stack.len();
                    let target = self.expression(&index.target);
                    let idx = self.expression(&index.index);
                    self.stack.truncate(len);
                    let target_ty = target.borrow();
                    match &*target_ty {
                        TypeInference::Array { element, .. } => {
                            TypeInference::unify(
                                &idx,
                                &Rc::new(RefCell::new(TypeInference::Integer)),
                            );
                            TypeInference::unify(&ty, &element);
                        }
                        _ => {
                            TypeInference::unify(&ty, &Rc::new(RefCell::new(TypeInference::Error)))
                        }
                    }
                }
            },
        };
        ty
    }

    fn block(&mut self, block: &Block) -> Rc<RefCell<TypeInference>> {
        let begin = self.stack.len();
        let scope = Rc::clone(&self.scopes[self.last]);
        self.last += 1;
        self.stack.push(Rc::clone(&scope));
        self.block_stack.push(Rc::clone(&scope));
        let ty = Rc::new(RefCell::new(TypeInference::Unknown));
        self.inference.push(Rc::clone(&ty));
        for statement in block.statement.iter() {
            self.statement(statement);
        }
        let expr = if let Some(result) = &block.result {
            self.expression(result)
        } else {
            Rc::new(RefCell::new(TypeInference::Never))
        };
        TypeInference::unify(&ty, &expr);
        self.stack.truncate(begin);
        self.block_stack.pop();
        ty
    }

    fn pattern(&mut self, pattern: &Pattern) -> Rc<RefCell<TypeInference>> {
        let scope = Rc::clone(self.stack.last().unwrap());
        match pattern {
            Pattern::Ident(ident) => {
                let scope = scope.borrow();
                let var = scope.get_var(&ident.content).unwrap();
                Rc::clone(&var.ty)
            }
            Pattern::Declaration(decl) => {
                let scope = scope.borrow();
                let var = scope.get_var(&decl.ident.content).unwrap();
                Rc::clone(&var.ty)
            }
            Pattern::Tuple(args) => Rc::new(RefCell::new(TypeInference::Tuple(
                args.iter().map(|arg| self.pattern(arg)).collect(),
            ))),
        }
    }

    fn assignee(&mut self, assignee: &Assignee) -> Rc<RefCell<TypeInference>> {
        let scope = Rc::clone(self.stack.last().unwrap());
        match assignee {
            Assignee::Declaration(declaration) => {
                scope
                    .borrow()
                    .get_var(&declaration.ident.content)
                    .unwrap()
                    .ty
            }
            Assignee::Path(path) => match path {
                Path::Simple(simple) => scope.borrow().get_var(&simple.content).unwrap().ty,
            },
            Assignee::Tuple(args) => Rc::new(RefCell::new(TypeInference::Tuple(
                args.iter()
                    .map(|assignee| self.assignee(assignee))
                    .collect(),
            ))),
            Assignee::Index(index) => {
                let len = self.stack.len();
                let target = self.expression(&index.target);
                let index = self.expression(&index.index);
                self.stack.truncate(len);
                let target_ty = target.borrow();
                match &*target_ty {
                    TypeInference::Array { element, .. } => {
                        TypeInference::unify(
                            &index,
                            &Rc::new(RefCell::new(TypeInference::Integer)),
                        );
                        Rc::clone(&element)
                    }
                    _ => Rc::new(RefCell::new(TypeInference::Error)),
                }
            }
        }
    }
}
