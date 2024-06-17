use crate::syntax::{
    expr::{BinaryOperation, Block, BlockExpression, Expression, NonblockExpression},
    format::FormatFragment,
    item::Type,
    path::{Identifier, Path},
    stmt::{Pattern, Statement},
    Program,
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

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
    var: HashMap<Identifier, Vec<Variable>>,
    ty: HashMap<Identifier, Vec<TypeDefinition>>,
    counter: usize,
}

#[derive(Debug, Clone)]
struct Variable {
    name: Identifier,
    ty: TypeDefinition,
    mapped: String,
}

#[derive(Debug)]
pub struct Intermediate {
    ty: TypeDefinition,
    feature: HashSet<&'static str>,
    decl: String,
    global: String,
    effect: String,
    immediate: String,
}

impl Translator {
    fn push_type(&mut self, definition: TypeDefinition) {
        self.ty
            .entry(definition.name.clone())
            .or_default()
            .push(definition);
    }

    fn push_var(&mut self, variable: Variable) {
        self.var
            .entry(variable.name.clone())
            .or_default()
            .push(variable);
    }

    fn pop_var(&mut self, name: &Identifier) {
        let Some(v) = self.var.get_mut(name) else {
            unreachable!("Pop undefined {name}");
        };
        v.pop();
        if v.is_empty() {
            self.var.remove(name);
        }
    }

    pub fn path(&mut self, path: &Path) -> Intermediate {
        match path {
            Path::Simple(ident) => {
                let Some(symbol) = self.var.get(ident).and_then(|v| v.last()) else {
                    panic!("Could not find name {ident} in scope.");
                };
                Intermediate {
                    feature: Default::default(),
                    decl: "".into(),
                    global: "".into(),
                    ty: symbol.ty.clone(),
                    effect: "".into(),
                    immediate: symbol.mapped.clone(),
                }
            }
        }
    }

    pub fn binary(&mut self, binary: &BinaryOperation) -> Intermediate {
        let lhs = self.expression(&binary.lhs);
        let rhs = self.expression(&binary.rhs);
        match (lhs.ty.name.as_str(), binary.operator, rhs.ty.name.as_str()) {
            ("i32", op @ _, "i32") => Intermediate {
                ty: lhs.ty.clone(),
                feature: lhs.feature.union(&rhs.feature).copied().collect(),
                decl: lhs.decl + &rhs.decl,
                global: lhs.global + &rhs.global,
                effect: lhs.effect + &rhs.effect,
                immediate: format!("({}) {op} ({})", lhs.immediate, rhs.immediate),
            },
            (l, op, r) => panic!("Unsupported operation `{l}` {op} `{r}`",),
        }
    }

    pub fn block(&mut self, block: &Block) -> Intermediate {
        let mut stack = vec![];
        let mut ty = TypeDefinition {
            name: "()".into(),
            mapped: "void".into(),
            construction: TypeConstruction::Path("()".into()),
        };
        let mut feature = HashSet::new();
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        let mut immediate = String::new();
        for statement in block.statement.iter() {
            match statement {
                Statement::Nop => {}
                Statement::Input(parser) => {
                    for (ty, ident) in parser.arg.iter() {
                        match ty {
                            Type::Path(path) => {
                                let Some(ty) = self.ty.get(&path.ident).and_then(|v| v.last())
                                else {
                                    panic!("Could not find type `{}` in scope.", path.ident);
                                };
                                let getter = match ty.name.as_str() {
                                    "i32" => {
                                        feature.insert("reader_def");
                                        feature.insert("read_white");
                                        feature.insert("read_i32");
                                        "read_i32()"
                                    }
                                    _ => panic!("Could not find parser for type `{}`.", ty.name),
                                };
                                stack.push(ident.clone());
                                let var = Variable {
                                    name: ident.clone(),
                                    ty: ty.clone(),
                                    mapped: format!("{}_{}", ident, self.counter),
                                };
                                self.counter += 1;
                                writeln!(effect, "{} {} = {};", ty.mapped, var.mapped, getter)
                                    .unwrap();
                                self.push_var(var);
                            }
                            Type::Tuple(_) => todo!(),
                        }
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
                                let Some(var) = self.var.get(ident).and_then(|v| v.last()) else {
                                    panic!("Could not print undefined variable {ident}.");
                                };
                                match var.ty.name.as_str() {
                                    "i32" => {
                                        feature.insert("write_i32");
                                        writeln!(effect, "write_i32({});", var.mapped).unwrap();
                                    }
                                    _ => panic!("Could not print value of type `{}`.", var.ty.name),
                                }
                            }
                        }
                    }
                }
                Statement::Assignment(assignment) => {
                    let rhs = self.expression(&assignment.rhs);
                    feature.extend(rhs.feature);
                    decl.push_str(&rhs.decl);
                    global.push_str(&rhs.global);
                    effect.push_str(&rhs.effect);
                    match &assignment.lhs {
                        Pattern::Ident(ident) => {
                            let Some(var) = self.var.get(ident).and_then(|v| v.last()) else {
                                panic!("Could not assign to undefined variable {ident}.");
                            };
                            if var.ty != rhs.ty {
                                panic!(
                                    "Expected rhs to be `{}`, found `{}`.",
                                    var.ty.name, rhs.ty.name
                                );
                            }
                            writeln!(effect, "{} = ({});", var.mapped, rhs.immediate).unwrap();
                        }
                        Pattern::Declaration(declaration) => match &declaration.ty {
                            Type::Path(path) => {
                                let Some(ty) = self.ty.get(&path.ident).and_then(|v| v.last())
                                else {
                                    panic!("Could not find type `{}` in scope.", path.ident);
                                };
                                let var = Variable {
                                    name: declaration.ident.clone(),
                                    ty: ty.clone(),
                                    mapped: format!("{}_{}", declaration.ident, self.counter),
                                };
                                stack.push(var.name.clone());
                                self.counter += 1;
                                writeln!(
                                    effect,
                                    "{} {} = ({});",
                                    ty.mapped, var.mapped, rhs.immediate
                                )
                                .unwrap();
                                self.push_var(var);
                            }
                            Type::Tuple(tuple) => todo!(),
                        },
                    }
                }
                Statement::Repeat(repeat) => {
                    let times = self.expression(&repeat.times);
                    feature.extend(times.feature);
                    decl.push_str(&times.decl);
                    global.push_str(&times.global);
                    effect.push_str(&times.effect);
                    if times.ty.name != "i32" {
                        panic!("Could not repeat for non-integer type `{}`", times.ty.name);
                    }
                    writeln!(
                        effect,
                        "for ({} i = 0; i < ({}); ++i) {{",
                        times.ty.mapped, times.immediate
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
        for ident in stack {
            self.pop_var(&ident);
        }
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
            },
        }
    }

    pub fn translate(program: &Program) -> String {
        let mut translator = Translator {
            var: Default::default(),
            ty: Default::default(),
            counter: 0,
        };

        translator.push_type(TypeDefinition {
            name: "i32".into(),
            mapped: "int32_t".into(),
            construction: TypeConstruction::Path("i32".into()),
        });

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
