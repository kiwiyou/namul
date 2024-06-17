use typeck::{Scope, TypeChecker, TypeDeduction, TypeInstance};

use crate::syntax::{
    expr::{BinaryOperation, Block, BlockExpression, Expression, NonblockExpression},
    format::FormatFragment,
    item::Type,
    literal::Literal,
    path::{Identifier, Path},
    stmt::{Pattern, Statement},
    Program,
};
use std::{cell::RefCell, collections::HashSet, fmt::Write, rc::Rc};

pub mod typeck;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
    name: Identifier,
    mapped: String,
    construction: TypeConstruction,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConstruction {
    Path(Identifier),
    Tuple(Vec<TypeConstruction>),
}

pub struct Translator {
    scopes: Vec<Rc<RefCell<Scope>>>,
    expr_types: Vec<TypeDeduction>,
    current: usize,
    current_expr: usize,
}

#[derive(Debug)]
pub struct Intermediate {
    ty: TypeInstance,
    feature: HashSet<&'static str>,
    decl: String,
    global: String,
    effect: String,
    immediate: String,
}

impl Translator {
    pub fn current_scope(&self) -> Rc<RefCell<Scope>> {
        Rc::clone(&self.scopes[self.current])
    }

    pub fn current_expr_type(&self) -> &TypeDeduction {
        &self.expr_types[self.current_expr]
    }

    pub fn path(&mut self, path: &Path) -> Intermediate {
        let scope = self.current_scope();
        let result = match path {
            Path::Simple(ident) => {
                let scope = scope.borrow();
                let var = scope.get_var(ident).unwrap();
                let Some(ty) = self.current_expr_type().deduce() else {
                    panic!("Could not deduce type of {ident}.");
                };
                Intermediate {
                    feature: Default::default(),
                    decl: "".into(),
                    global: "".into(),
                    ty,
                    effect: "".into(),
                    immediate: format!("({})", var.mangle.clone()),
                }
            }
        };
        self.current_expr += 1;
        result
    }

    pub fn binary(&mut self, binary: &BinaryOperation) -> Intermediate {
        let lhs = self.expression(&binary.lhs);
        let rhs = self.expression(&binary.rhs);
        let Some(ty) = self.current_expr_type().deduce() else {
            panic!("Could not deduce type.");
        };
        let result = match (lhs.ty, binary.operator, rhs.ty) {
            (TypeInstance::I32, op @ _, TypeInstance::I32) => Intermediate {
                ty,
                feature: lhs.feature.union(&rhs.feature).copied().collect(),
                decl: lhs.decl + &rhs.decl,
                global: lhs.global + &rhs.global,
                effect: lhs.effect + &rhs.effect,
                immediate: format!("({} {op} {})", lhs.immediate, rhs.immediate),
            },
            (l, op, r) => panic!("Unsupported operation `{l}` {op} `{r}`",),
        };
        self.current_expr += 1;
        result
    }

    pub fn literal(&mut self, literal: &Literal) -> Intermediate {
        let Some(ty) = self.current_expr_type().deduce() else {
            panic!("Could not deduce type.");
        };
        let result = match literal {
            Literal::Decimal(decimal) => Intermediate {
                ty,
                feature: Default::default(),
                decl: "".into(),
                global: "".into(),
                effect: "".into(),
                immediate: decimal.0.clone(),
            },
        };
        self.current_expr += 1;
        result
    }

    pub fn block(&mut self, block: &Block) -> Intermediate {
        let result = self.current;
        self.current += 1;
        let mut ty = TypeInstance::Unit;
        let mut feature = HashSet::new();
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        let mut immediate = String::new();
        for statement in block.statement.iter() {
            self.current += 1;
            match statement {
                Statement::Nop => {}
                Statement::Input(parser) => {
                    let scope = self.current_scope();
                    let scope = scope.borrow();
                    for (ty, ident) in parser.arg.iter() {
                        let Some(ty) = scope.get_ty(ty) else {
                            panic!("Could not find type `{}` in scope.", ty);
                        };
                        let getter = match ty {
                            TypeInstance::I32 => {
                                feature.insert("reader_def");
                                feature.insert("read_white");
                                feature.insert("read_i32");
                                "read_i32()"
                            }
                            _ => panic!("Could not find parser for type `{ty}`."),
                        };
                        let var = scope.get_var(ident).unwrap();
                        writeln!(effect, "{} {} = {};", ty.mapped(), var.mangle, getter).unwrap();
                    }
                }
                Statement::Expression(expr) => {
                    let expr = self.expression(expr);
                    feature.extend(expr.feature);
                    decl.push_str(&expr.decl);
                    global.push_str(&expr.global);
                    effect.push_str(&expr.effect);
                }
                Statement::Print(format) => {
                    feature.insert("writer_def");
                    for fragment in format.fragment.iter() {
                        match fragment {
                            FormatFragment::Literal(literal) => {
                                feature.insert("write_literal");
                                writeln!(
                                    effect,
                                    "write_literal({}, {});",
                                    double_quote(literal),
                                    literal.len()
                                )
                                .unwrap();
                            }
                            FormatFragment::Ident(ident) => {
                                let scope = self.current_scope();
                                let scope = scope.borrow();
                                let Some(var) = scope.get_var(ident) else {
                                    panic!("Could not print undefined variable {ident}.");
                                };
                                let Some(ty) = var.ty.deduce() else {
                                    panic!("Could not deduce type of {ident}.");
                                };
                                match ty {
                                    TypeInstance::I32 => {
                                        feature.insert("write_i32");
                                        writeln!(effect, "write_i32({});", var.mangle).unwrap();
                                    }
                                    _ => panic!("Could not print value of type `{}`.", ty),
                                }
                            }
                        }
                    }
                }
                Statement::Assignment(assignment) => {
                    let scope = self.current_scope();
                    let scope = scope.borrow();
                    let rhs = self.expression(&assignment.rhs);
                    feature.extend(rhs.feature);
                    decl.push_str(&rhs.decl);
                    global.push_str(&rhs.global);
                    effect.push_str(&rhs.effect);
                    match &assignment.lhs {
                        Pattern::Ident(ident) => {
                            let Some(var) = scope.get_var(ident) else {
                                panic!("Could not assign to undefined variable {ident}.");
                            };
                            writeln!(effect, "{} = {};", var.mangle, rhs.immediate).unwrap();
                        }
                        Pattern::Declaration(declaration) => match &declaration.ty {
                            Type::Path(path) => {
                                let Some(ty) = scope.get_ty(&path.ident) else {
                                    panic!("Could not find type `{}` in scope.", path.ident);
                                };
                                let var = scope.get_var(&declaration.ident).unwrap();
                                writeln!(
                                    effect,
                                    "{} {} = {};",
                                    ty.mapped(),
                                    var.mangle,
                                    rhs.immediate
                                )
                                .unwrap();
                            }
                            Type::Tuple(_) => todo!(),
                        },
                    }
                }
                Statement::Repeat(repeat) => {
                    let times = self.expression(&repeat.times);
                    feature.extend(times.feature);
                    decl.push_str(&times.decl);
                    global.push_str(&times.global);
                    effect.push_str(&times.effect);
                    if times.ty != TypeInstance::I32 {
                        panic!("Could not repeat for non-integer type `{}`", times.ty);
                    }
                    writeln!(
                        effect,
                        "for ({} i = 0; i < {}; ++i) {{",
                        times.ty.mapped(),
                        times.immediate
                    )
                    .unwrap();
                    let block = self.block(&repeat.block);
                    feature.extend(block.feature);
                    decl.push_str(&block.decl);
                    global.push_str(&block.global);
                    effect.push_str(&block.effect);
                    writeln!(effect, "}}").unwrap();
                }
            }
        }
        if let Some(result) = block.result.as_ref().map(|expr| self.expression(expr)) {
            ty = result.ty;
            feature.extend(result.feature);
            decl.push_str(&result.decl);
            global.push_str(&result.global);
            effect.push_str(&result.effect);
            immediate = result.immediate;
        }
        self.current = result;
        self.current_expr += 1;
        Intermediate {
            ty,
            feature,
            decl,
            global,
            effect,
            immediate,
        }
    }

    pub fn expression(&mut self, expr: &Expression) -> Intermediate {
        match expr {
            Expression::Block(block) => match block {
                BlockExpression::Block(block) => self.block(block),
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Path(path) => self.path(path),
                NonblockExpression::Binary(binary) => self.binary(binary),
                NonblockExpression::Literal(literal) => self.literal(literal),
            },
        }
    }

    pub fn translate(program: &Program) -> String {
        let type_checker = TypeChecker::run(program);

        let mut translator = Translator {
            scopes: type_checker.scopes,
            expr_types: type_checker.expr_types,
            current: 0,
            current_expr: 0,
        };

        let intermediate = translator.block(&Block {
            statement: program.statement.clone(),
            result: None,
        });

        let mut out = String::new();
        writeln!(out, "// namul 0.0.0").unwrap();
        writeln!(out, "#include <stdint.h>").unwrap();
        writeln!(out, "#include <unistd.h>").unwrap();
        if intermediate.feature.contains("writer_def") {
            out.push_str(include_str!("fragments/writer_struct.c"));
        }
        if intermediate.feature.contains("write_literal") {
            out.push_str(include_str!("fragments/write_literal.c"));
        }
        if intermediate.feature.contains("write_i32") {
            out.push_str(include_str!("fragments/write_i32.c"));
        }
        writeln!(out, "void halt() {{").unwrap();
        if intermediate.feature.contains("writer_def") {
            writeln!(out, "flush();").unwrap();
        }
        writeln!(out, "_Exit(0);").unwrap();
        writeln!(out, "}}").unwrap();
        if intermediate.feature.contains("reader_def") {
            out.push_str(include_str!("fragments/reader_struct.c"));
        }
        if intermediate.feature.contains("read_white") {
            out.push_str(include_str!("fragments/read_white.c"));
        }
        if intermediate.feature.contains("read_i32") {
            out.push_str(include_str!("fragments/read_i32.c"));
        }
        out.push_str(&intermediate.decl);
        writeln!(out, "int main;").unwrap();
        writeln!(out, "int __libc_start_main() {{").unwrap();
        if intermediate.feature.contains("writer_def") {
            writeln!(out, "Writer w;").unwrap();
            writeln!(out, "w.back = 0;").unwrap();
            writeln!(out, "writer = &w;").unwrap();
        }
        if intermediate.feature.contains("reader_def") {
            writeln!(out, "Reader r;").unwrap();
            writeln!(out, "r.off = r.end = 0;").unwrap();
            writeln!(out, "reader = &r;").unwrap();
        }
        out.push_str(&intermediate.effect);
        writeln!(out, "halt();").unwrap();
        writeln!(out, "}}").unwrap();
        out.push_str(&intermediate.global);
        out
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
