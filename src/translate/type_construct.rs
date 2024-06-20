use std::{cell::RefCell, rc::Rc};

use crate::syntax::{
    expr::{Block, BlockExpression, Expression},
    item::Type,
    stmt::{Pattern, Statement},
    Program,
};

use super::{name_resolve::NameResolver, Scope, TypeInference, TypeInstance};

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
                for (ty, name) in parser.arg.iter() {
                    let scope = scope.borrow_mut();
                    let Some(ty) = scope.get_ty(&ty.content) else {
                        panic!("Could not find type `{}` in scope.", ty.content);
                    };
                    let var = scope.get_var(&name.content).unwrap();
                    let mut var_ty = var.ty.borrow_mut();
                    var_ty.unify(&mut ty.borrow_mut());
                }
                self.stack.push(scope);
            }
            Statement::Expression(expr) => {
                self.expression(expr);
            }
            Statement::Assignment(assignment) => {
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                self.expression(&assignment.rhs);
                self.stack.push(scope);
                self.pattern(&assignment.lhs);
            }
            Statement::Repeat(repeat) => {
                self.expression(&repeat.times);
                self.block(&repeat.block);
            }
            Statement::While(while_) => {
                self.expression(&while_.condition);
                self.block(&while_.block);
            }
            Statement::Function(function) => {
                let block = Rc::clone(self.block_stack.last().unwrap());
                let var = {
                    let block = block.borrow_mut();
                    block.get_var(&function.ident.content).unwrap()
                };
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                self.stack.push(Rc::clone(&scope));
                let scope = scope.borrow();
                TypeInference::Function {
                    args: self.construct_arg(&scope, &function.args),
                    result: function.result.as_ref().map_or(
                        Rc::new(RefCell::new(TypeInference::Exact(TypeInstance::Unit))),
                        |result| self.construct_type(&scope, result),
                    ),
                }
                .unify(&mut var.ty.borrow_mut());
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
                    self.expression(&if_.condition);
                    self.block(&if_.truthy);
                    if let Some(falsy) = &if_.falsy {
                        self.block(falsy);
                    }
                }
            },
            Expression::Nonblock(_) => {}
        };
    }

    fn block(&mut self, block: &Block) {
        let begin = self.stack.len();
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
        self.stack.truncate(begin);
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
        match ty {
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
        }
    }

    fn pattern(&mut self, pattern: &Pattern) {
        let scope = Rc::clone(self.stack.last().unwrap());
        match pattern {
            Pattern::Ident(_) => {}
            Pattern::Declaration(decl) => {
                let scope = scope.borrow();
                let decl_ty = self.construct_type(&scope, &decl.ty);
                let var = scope.get_var(&decl.ident.content).unwrap();
                let mut var_ty = var.ty.borrow_mut();
                var_ty.unify(&mut decl_ty.borrow_mut());
            }
            Pattern::Tuple(args) => {
                for arg in args {
                    self.pattern(arg);
                }
            }
        }
    }
}
