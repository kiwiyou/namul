use std::{cell::RefCell, rc::Rc};

use crate::syntax::{
    expression::{Assignee, Block, BlockExpression, Expression, NonblockExpression, Place},
    item::Type,
    path::Path,
    statement::{Pattern, Statement},
    Program,
};

use super::{name_resolve::NameResolver, Scope, TypeInference};

pub struct TypeConstructor {
    pub scopes: Vec<Rc<RefCell<Scope>>>,
    stack: Vec<Rc<RefCell<Scope>>>,
    block_stack: Vec<Rc<RefCell<Scope>>>,
    last: usize,
}

impl TypeConstructor {
    pub fn from_name_resolver(resolver: NameResolver) -> Self {
        Self {
            scopes: resolver.scopes,
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
            Statement::Input(parser) => {
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                self.stack.push(scope);
                for arg in parser.arg.iter() {
                    self.assignee(arg);
                }
            }
            Statement::Expression(expr) => {
                self.expression(expr);
            }
            Statement::Repeat(repeat) => {
                let len = self.stack.len();
                self.expression(&repeat.times);
                if repeat.var.is_some() {
                    let scope = Rc::clone(&self.scopes[self.last]);
                    self.last += 1;
                    self.stack.push(scope);
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
                let block = Rc::clone(self.block_stack.last().unwrap());
                let var = block.borrow_mut().get_var(&function.ident.content).unwrap();
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                self.stack.push(Rc::clone(&scope));
                let scope = scope.borrow();
                let actual = Rc::new(RefCell::new(TypeInference::Function {
                    args: self.construct_arg(&scope, &function.args),
                    result: function
                        .result
                        .as_ref()
                        .map_or(Rc::new(RefCell::new(TypeInference::Never)), |result| {
                            self.construct_type(&scope, result)
                        }),
                }));
                TypeInference::unify(&actual, &var.ty);
                for arg in function.args.iter() {
                    self.pattern(arg);
                }
                self.block(&function.block);
                self.stack.pop();
            }
        }
    }

    fn expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Block(block) => match block {
                BlockExpression::Block(block) => {
                    self.block(block);
                }
                BlockExpression::If(if_) => {
                    let len = self.stack.len();
                    self.expression(&if_.condition);
                    self.block(&if_.truthy);
                    if let Some(falsy) = &if_.falsy {
                        self.block(falsy);
                    }
                    self.stack.truncate(len);
                }
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Parentheses(expr) => {
                    self.expression(&expr);
                }
                NonblockExpression::Literal(_) => {}
                NonblockExpression::Path(path) => match path {
                    Path::Simple(simple) => {
                        let scope = Rc::clone(self.stack.last().unwrap());
                        if scope.borrow().get_var(&simple.content).is_none() {
                            panic!("Could not find {} in scope.", simple.content);
                        }
                    }
                },
                NonblockExpression::Binary(binary) => {
                    self.expression(&binary.lhs);
                    self.expression(&binary.rhs);
                }
                NonblockExpression::Comparison(comparison) => {
                    self.expression(&comparison.first);
                    for (_, expr) in comparison.chain.iter() {
                        self.expression(expr);
                    }
                }
                NonblockExpression::Print(print) => {
                    let len = self.stack.len();
                    for arg in print.args.iter() {
                        self.expression(arg);
                    }
                    self.stack.truncate(len);
                }
                NonblockExpression::Select(select) => {
                    let len = self.stack.len();
                    self.expression(&select.condition);
                    let inside = self.stack.len();
                    self.expression(&select.truthy);
                    self.stack.truncate(inside);
                    self.expression(&select.falsy);
                    self.stack.truncate(len);
                }
                NonblockExpression::MakeTuple(make_tuple) => {
                    for arg in make_tuple.args.iter() {
                        self.expression(arg);
                    }
                }
                NonblockExpression::MakeArray(make_array) => {
                    for arg in make_array.args.iter() {
                        self.expression(arg);
                    }
                }
                NonblockExpression::Invocation(invocation) => {
                    self.expression(&invocation.callee);
                    let len = self.stack.len();
                    for arg in invocation.args.iter() {
                        self.expression(arg);
                    }
                    self.stack.truncate(len);
                }
                NonblockExpression::Declaration(declaration) => {
                    let scope = Rc::clone(&self.scopes[self.last]);
                    self.last += 1;
                    self.stack.push(Rc::clone(&scope));
                    let scope = scope.borrow();
                    let ty = self.construct_type(&scope, &declaration.ty);
                    let var = scope.get_var(&declaration.ident.content).unwrap();
                    TypeInference::unify(&var.ty, &ty);
                }
                NonblockExpression::Assignment(assignment) => {
                    self.expression(&assignment.rhs);
                    let scope = Rc::clone(&self.scopes[self.last]);
                    self.last += 1;
                    self.stack.push(scope);
                    self.assignee(&assignment.lhs);
                }
                NonblockExpression::CompoundAssignment(compound) => {
                    let len = self.stack.len();
                    self.expression(&compound.rhs);
                    self.stack.truncate(len);
                    match &compound.lhs {
                        Place::Path(path) => match path {
                            Path::Simple(simple) => {
                                let scope = Rc::clone(self.stack.last().unwrap());
                                if scope.borrow().get_var(&simple.content).is_none() {
                                    panic!("Could not find {} in scope.", simple.content);
                                }
                            }
                        },
                        Place::Index(index) => {
                            let len = self.stack.len();
                            self.expression(&index.target);
                            self.expression(&index.index);
                            self.stack.truncate(len);
                        }
                    }
                }
                NonblockExpression::Index(index) => {
                    let len = self.stack.len();
                    self.expression(&index.target);
                    self.expression(&index.index);
                    self.stack.truncate(len);
                }
            },
        };
    }

    fn block(&mut self, block: &Block) {
        let len = self.stack.len();
        let scope = Rc::clone(&self.scopes[self.last]);
        self.last += 1;
        self.stack.push(Rc::clone(&scope));
        self.block_stack.push(Rc::clone(&scope));
        for stmt in block.statement.iter() {
            self.statement(stmt);
        }
        if let Some(result) = &block.result {
            self.expression(&result);
        }
        self.stack.truncate(len);
        self.block_stack.pop();
    }

    fn construct_arg(
        &mut self,
        scope: &Scope,
        args: &[Pattern],
    ) -> Vec<Rc<RefCell<TypeInference>>> {
        let mut params = vec![];
        for arg in args {
            params.push(match arg {
                Pattern::Ident(_) => unreachable!(),
                Pattern::Declaration(declaration) => self.construct_type(scope, &declaration.ty),
                Pattern::Tuple(args) => Rc::new(RefCell::new(TypeInference::Tuple(
                    self.construct_arg(scope, args),
                ))),
            });
        }
        params
    }

    fn construct_type(&mut self, scope: &Scope, ty: &Type) -> Rc<RefCell<TypeInference>> {
        let ans = match ty {
            Type::Path(path) => {
                let Some(ty) = scope.get_ty(&path.ident.content) else {
                    panic!("Could not find type `{}` in scope.", path.ident.content);
                };
                ty
            }
            Type::Tuple(args) => Rc::new(RefCell::new(TypeInference::Tuple(
                args.iter()
                    .map(|ty| self.construct_type(scope, ty))
                    .collect(),
            ))),
            Type::Array(array) => Rc::new(RefCell::new(TypeInference::Array {
                element: self.construct_type(scope, &array.element),
                len: array.len,
            })),
        };
        ans
    }

    fn pattern(&mut self, pattern: &Pattern) {
        let scope = Rc::clone(self.stack.last().unwrap());
        match pattern {
            Pattern::Ident(_) => {}
            Pattern::Declaration(decl) => {
                let scope = scope.borrow();
                let decl_ty = self.construct_type(&scope, &decl.ty);
                let var = scope.get_var(&decl.ident.content).unwrap();
                TypeInference::unify(&var.ty, &decl_ty);
            }
            Pattern::Tuple(args) => {
                for arg in args {
                    self.pattern(arg);
                }
            }
        }
    }

    fn assignee(&mut self, assignee: &Assignee) {
        let scope = Rc::clone(&self.stack.last().unwrap());
        match assignee {
            Assignee::Declaration(declaration) => {
                let var = scope.borrow().get_var(&declaration.ident.content).unwrap();
                TypeInference::unify(
                    &var.ty,
                    &self.construct_type(&scope.borrow(), &declaration.ty),
                );
            }
            Assignee::Path(path) => match path {
                Path::Simple(simple) => {
                    if scope.borrow().get_var(&simple.content).is_none() {
                        panic!("Could not find {} in scope.", simple.content);
                    }
                }
            },
            Assignee::Tuple(args) | Assignee::Array(args) => {
                for arg in args.iter() {
                    self.assignee(arg);
                }
            }
            Assignee::Index(index) => {
                let len = self.stack.len();
                self.expression(&index.target);
                self.expression(&index.index);
                self.stack.truncate(len);
            }
        }
    }
}
