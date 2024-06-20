use std::{cell::RefCell, rc::Rc};

use crate::syntax::{
    expr::{Block, BlockExpression, Expression, NonblockExpression},
    item::Type,
    literal::Literal,
    path::Path,
    stmt::{Pattern, Statement},
    Program,
};

use super::{resolver::NameResolver, Scope, TypeInference, TypeInstance};

pub struct TypeChecker {
    pub scopes: Vec<Rc<RefCell<Scope>>>,
    pub inference: Vec<Rc<RefCell<TypeInference>>>,
    stack: Vec<Rc<RefCell<Scope>>>,
    block_stack: Vec<Rc<RefCell<Scope>>>,
    last: usize,
    tuple_id: usize,
}

impl TypeChecker {
    pub fn from_name_resolver(resolver: NameResolver) -> Self {
        Self {
            scopes: resolver.scopes,
            inference: vec![],
            stack: vec![],
            block_stack: vec![],
            last: 1usize,
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
                    let mut constraint = TypeInference::Exact(ty.clone());
                    let var = scope.get_var(&name.content).unwrap();
                    let mut var_ty = var.ty.borrow_mut();
                    var_ty.unify(&mut constraint);
                }
                self.stack.push(scope);
            }
            Statement::Expression(expr) => {
                self.expression(expr);
            }
            Statement::Assignment(assignment) => {
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                let rhs = self.expression(&assignment.rhs);
                let mut rhs_ty = rhs.borrow_mut();
                self.stack.push(scope);
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
            Statement::Function(function) => {
                let block = Rc::clone(self.block_stack.last().unwrap());
                let var = {
                    let block = block.borrow_mut();
                    block.get_var(&function.ident.content).unwrap()
                };
                let scope = Rc::clone(&self.scopes[self.last]);
                self.last += 1;
                self.stack.push(Rc::clone(&scope));
                let mut args = vec![];
                for arg in function.args.iter() {
                    args.push(self.pattern(arg));
                }
                let result = self.block(&function.block);
                {
                    let mut result_ty = result.borrow_mut();
                    let scope = scope.borrow();
                    TypeInference::Exact(
                        function
                            .result
                            .as_ref()
                            .map_or(TypeInstance::Unit, |result| {
                                self.construct_type(&scope, result)
                            }),
                    )
                    .unify(&mut result_ty);
                }
                TypeInference::Function { args, result }.unify(&mut var.ty.borrow_mut());
                self.stack.pop();
            }
        }
    }

    fn expression(&mut self, expr: &Expression) -> Rc<RefCell<TypeInference>> {
        let scope = Rc::clone(&self.stack.last().unwrap());
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
                NonblockExpression::Parentheses(expr) => {
                    let expr = self.expression(&expr);
                    let mut expr_ty = expr.borrow_mut();
                    expr_ty.unify(&mut ty.borrow_mut());
                }
                NonblockExpression::Invocation(invocation) => {
                    let func = self.expression(&invocation.callee);
                    let mut args = vec![];
                    for arg in invocation.args.iter() {
                        args.push(Rc::clone(&self.expression(arg)));
                    }
                    TypeInference::Function {
                        args,
                        result: Rc::clone(&ty),
                    }
                    .unify(&mut func.borrow_mut());
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
        for stmt in block.statement.iter() {
            self.statement(stmt);
        }
        if let Some(result) = &block.result {
            let expr = self.expression(&result);
            let mut expr_ty = expr.borrow_mut();
            expr_ty.unify(&mut ty.borrow_mut());
        } else {
            TypeInference::Exact(TypeInstance::Unit).unify(&mut ty.borrow_mut());
        };
        self.stack.truncate(begin);
        self.block_stack.pop();
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
            Type::Tuple(args) => {
                let id = self.tuple_id;
                self.tuple_id += 1;
                TypeInstance::Tuple {
                    id,
                    args: args
                        .iter()
                        .map(|ty| self.construct_type(scope, ty))
                        .collect(),
                }
            }
        }
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
