use std::{cell::RefCell, rc::Rc};

use crate::syntax::{
    expression::{Assignee, Block, BlockExpression, Expression, NonblockExpression},
    statement::{Pattern, Statement},
    Program,
};

use super::{Scope, TypeInference, Variable};

pub struct NameResolver {
    pub scopes: Vec<Rc<RefCell<Scope>>>,
    block_scopes: Vec<Rc<RefCell<Scope>>>,
    var_id: usize,
}

impl NameResolver {
    pub fn new(program: &Program) -> Self {
        let root = Rc::new(RefCell::new(Scope::root()));
        let mut resolver = NameResolver {
            scopes: vec![Rc::clone(&root)],
            block_scopes: vec![],
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
        match stmt {
            Statement::Nop => parent,
            Statement::Input(parser) => {
                let scope = Rc::new(RefCell::new(Scope::with_parent(parent)));
                self.scopes.push(Rc::clone(&scope));
                for (_, name) in parser.arg.iter() {
                    scope.borrow_mut().var.insert(
                        name.content.clone(),
                        Variable {
                            mangle: format!("_{}_{}", name.content, self.var_id),
                            ty: Rc::new(RefCell::new(TypeInference::Unknown)),
                        },
                    );
                    self.var_id += 1;
                }
                scope
            }
            Statement::Expression(expr) => self.expression(parent, expr),
            Statement::Repeat(repeat) => {
                let scope = self.expression(Rc::clone(&parent), &repeat.times);
                self.block(scope, &repeat.block);
                parent
            }
            Statement::While(while_) => {
                let scope = self.expression(Rc::clone(&parent), &while_.condition);
                self.block(scope, &while_.block);
                parent
            }
            Statement::Function(function) => {
                let block = Rc::clone(self.block_scopes.last().unwrap());
                let mut block = block.borrow_mut();
                let name = function.ident.content.clone();
                let id = self.var_id;
                self.var_id += 1;
                block.var.insert(
                    name.clone(),
                    Variable {
                        mangle: format!("_{name}_{id}"),
                        ty: Rc::new(RefCell::new(TypeInference::Unknown)),
                    },
                );
                let scope = Rc::new(RefCell::new(Scope::with_parent(Rc::clone(&parent))));
                self.scopes.push(Rc::clone(&scope));
                for arg in function.args.iter() {
                    self.pattern(Rc::clone(&scope), &arg);
                }
                self.block(Rc::clone(&scope), &function.block);
                parent
            }
        }
    }

    pub fn expression(
        &mut self,
        parent: Rc<RefCell<Scope>>,
        expr: &Expression,
    ) -> Rc<RefCell<Scope>> {
        match expr {
            Expression::Block(block) => match block {
                BlockExpression::Block(block) => {
                    self.block(Rc::clone(&parent), block);
                    parent
                }
                BlockExpression::If(if_) => {
                    let scope = self.expression(Rc::clone(&parent), &if_.condition);
                    self.block(Rc::clone(&scope), &if_.truthy);
                    if let Some(falsy) = &if_.falsy {
                        self.block(scope, falsy);
                    }
                    parent
                }
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Literal(_) => parent,
                NonblockExpression::Path(_) => parent,
                NonblockExpression::Binary(binary) => {
                    let mut scope = parent;
                    scope = self.expression(scope, &binary.lhs);
                    scope = self.expression(scope, &binary.rhs);
                    scope
                }
                NonblockExpression::Comparison(comparison) => {
                    let mut scope = parent;
                    scope = self.expression(scope, &comparison.first);
                    for (_, rest) in comparison.chain.iter() {
                        scope = self.expression(scope, rest);
                    }
                    scope
                }
                NonblockExpression::Print(print) => {
                    let mut scope = Rc::clone(&parent);
                    for arg in print.args.iter() {
                        scope = self.expression(scope, arg);
                    }
                    parent
                }
                NonblockExpression::Select(select) => {
                    let scope = self.expression(Rc::clone(&parent), &select.condition);
                    self.expression(Rc::clone(&scope), &select.truthy);
                    self.expression(scope, &select.falsy);
                    parent
                }
                NonblockExpression::MakeTuple(make_tuple) => {
                    let mut scope = Rc::clone(&parent);
                    for arg in make_tuple.args.iter() {
                        scope = self.expression(scope, arg);
                    }
                    scope
                }
                NonblockExpression::Parentheses(expr) => self.expression(Rc::clone(&parent), &expr),
                NonblockExpression::Invocation(invocation) => {
                    let mut scope = parent;
                    scope = self.expression(scope, &invocation.callee);
                    let mut inside = Rc::clone(&scope);
                    for arg in invocation.args.iter() {
                        inside = self.expression(inside, arg);
                    }
                    scope
                }
                NonblockExpression::Declaration(declaration) => {
                    let scope = Rc::new(RefCell::new(Scope::with_parent(parent)));
                    self.scopes.push(Rc::clone(&scope));
                    let id = self.var_id;
                    self.var_id += 1;
                    scope.borrow_mut().var.insert(
                        declaration.ident.content.clone(),
                        Variable {
                            mangle: format!("_{}_{}", declaration.ident.content, id),
                            ty: Rc::new(RefCell::new(TypeInference::Unknown)),
                        },
                    );
                    scope
                }
                NonblockExpression::Assignment(assignment) => {
                    self.expression(Rc::clone(&parent), &assignment.rhs);
                    let scope = Rc::new(RefCell::new(Scope::with_parent(parent)));
                    self.scopes.push(Rc::clone(&scope));
                    self.assignee(&mut scope.borrow_mut(), &assignment.lhs);
                    scope
                }
                NonblockExpression::CompoundAssignment(compound) => {
                    self.expression(Rc::clone(&parent), &compound.rhs);
                    parent
                }
            },
        }
    }

    pub fn block(&mut self, parent: Rc<RefCell<Scope>>, block: &Block) {
        let mut scope = Rc::new(RefCell::new(Scope::with_parent(Rc::clone(&parent))));
        self.scopes.push(Rc::clone(&scope));
        self.block_scopes.push(Rc::clone(&scope));
        for stmt in block.statement.iter() {
            scope = self.statement(scope, stmt);
        }
        if let Some(result) = &block.result {
            self.expression(scope, &result);
        }
        self.block_scopes.pop();
    }

    fn pattern(&mut self, parent: Rc<RefCell<Scope>>, pattern: &Pattern) {
        match pattern {
            Pattern::Ident(_) => {}
            Pattern::Declaration(decl) => {
                parent.borrow_mut().var.insert(
                    decl.ident.content.clone(),
                    Variable {
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
    }

    fn assignee(&mut self, scope: &mut Scope, assignee: &Assignee) {
        match assignee {
            Assignee::Declaration(declaration) => {
                let id = self.var_id;
                self.var_id += 1;
                let name = &declaration.ident.content;
                scope.var.insert(
                    name.clone(),
                    Variable {
                        mangle: format!("_{name}_{id}"),
                        ty: Rc::new(RefCell::new(TypeInference::Unknown)),
                    },
                );
            }
            Assignee::Path(_) => {}
            Assignee::Tuple(args) => {
                for arg in args.iter() {
                    self.assignee(scope, arg);
                }
            }
        }
    }
}