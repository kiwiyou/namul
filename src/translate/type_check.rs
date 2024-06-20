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
            Statement::Input(_) => {
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                self.stack.push(scope);
            }
            Statement::Expression(expr) => {
                self.expression(expr);
            }
            Statement::Repeat(repeat) => {
                let len = self.stack.len();
                self.expression(&repeat.times);
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
                TypeInference::Function { args, result }.unify(&mut var.ty.borrow_mut());
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
                    let mut block_ty = block.borrow_mut();
                    block_ty.unify(&mut ty.borrow_mut());
                }
                BlockExpression::If(if_) => {
                    let len = self.stack.len();
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
                        truthy_ty.unify(&mut TypeInference::Never);
                    }
                    truthy_ty.unify(&mut ty.borrow_mut());
                    self.stack.truncate(len);
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
                        let scope = Rc::clone(&self.stack.last().unwrap());
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
                NonblockExpression::Print(print) => {
                    let len = self.stack.len();
                    for arg in print.args.iter() {
                        self.expression(arg);
                    }
                    self.stack.truncate(len);
                    TypeInference::Never.unify(&mut ty.borrow_mut());
                }
                NonblockExpression::Select(select) => {
                    let len = self.stack.len();
                    let condition = self.expression(&select.condition);
                    let inside = self.stack.len();
                    TypeInference::Exact(TypeInstance::Bool).unify(&mut condition.borrow_mut());
                    let truthy = self.expression(&select.truthy);
                    let mut truthy_ty = truthy.borrow_mut();
                    self.stack.truncate(inside);
                    let falsy = self.expression(&select.falsy);
                    let mut falsy_ty = falsy.borrow_mut();
                    truthy_ty.unify(&mut falsy_ty);
                    if *condition.borrow() == TypeInference::Never {
                        TypeInference::Never.unify(&mut condition.borrow_mut());
                    } else {
                        truthy_ty.unify(&mut ty.borrow_mut());
                    }
                    self.stack.truncate(len);
                }
                NonblockExpression::MakeTuple(make_tuple) => {
                    let mut args = vec![];
                    for arg in make_tuple.args.iter() {
                        args.push(self.expression(&arg));
                    }
                    TypeInference::Tuple(args).unify(&mut ty.borrow_mut())
                }
                NonblockExpression::Parentheses(expr) => {
                    let expr = self.expression(&expr);
                    let mut expr_ty = expr.borrow_mut();
                    expr_ty.unify(&mut ty.borrow_mut());
                }
                NonblockExpression::Invocation(invocation) => {
                    let func = self.expression(&invocation.callee);
                    let len = self.stack.len();
                    let mut args = vec![];
                    for arg in invocation.args.iter() {
                        args.push(Rc::clone(&self.expression(arg)));
                    }
                    self.stack.truncate(len);
                    let mut func_ty = TypeInference::Function {
                        args,
                        result: Rc::clone(&ty),
                    };
                    func_ty.unify(&mut func.borrow_mut());
                }
                NonblockExpression::Declaration(_) => {
                    let scope = Rc::clone(&self.scopes[self.last]);
                    self.last += 1;
                    self.stack.push(scope);
                    TypeInference::Never.unify(&mut ty.borrow_mut());
                }
                NonblockExpression::Assignment(assignment) => {
                    let rhs = self.expression(&assignment.rhs);
                    let scope = Rc::clone(&self.scopes[self.last]);
                    self.last += 1;
                    self.stack.push(scope);
                    let lhs = self.assignee(&assignment.lhs);
                    lhs.borrow_mut().unify(&mut rhs.borrow_mut());
                    TypeInference::Never.unify(&mut ty.borrow_mut());
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
                                var.ty.borrow_mut().unify(&mut rhs.borrow_mut());
                            }
                        },
                    }
                    TypeInference::Never.unify(&mut ty.borrow_mut());
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
        if let Some(result) = &block.result {
            let expr = self.expression(&result);
            let mut expr_ty = expr.borrow_mut();
            expr_ty.unify(&mut ty.borrow_mut());
        } else {
            TypeInference::Never.unify(&mut ty.borrow_mut());
        };
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
        }
    }
}