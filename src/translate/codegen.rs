use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Write,
    rc::Rc,
};

use crate::syntax::{
    expr::{
        BinaryOperation, Block, BlockExpression, Comparison, Expression, If, Invocation, MakeTuple,
        NonblockExpression, Select,
    },
    format::{FormatFragment, FormatString},
    literal::Literal,
    path::Path,
    stmt::{Pattern, Statement},
    Program, TokenKind,
};

use super::{
    name_resolve::NameResolver, type_check::TypeChecker, type_construct::TypeConstructor, Scope,
    TypeInference, TypeInstance,
};

pub struct Codegen {
    feature: HashSet<&'static str>,
    scopes: Vec<Rc<RefCell<Scope>>>,
    inference: Vec<Rc<RefCell<TypeInference>>>,
    tuples: HashMap<String, usize>,
    stack: Vec<Rc<RefCell<Scope>>>,
    block_stack: Vec<Rc<RefCell<Scope>>>,
    last_scope: usize,
    last_infer: usize,
    counter: usize,
}

#[derive(Debug)]
pub struct Intermediate {
    ty: TypeInstance,
    decl: String,
    global: String,
    effect: String,
    immediate: String,
}

impl Codegen {
    pub fn synthesize(&mut self, inference: &TypeInference) -> Option<TypeInstance> {
        match inference {
            TypeInference::Exact(exact) => Some(exact.clone()),
            TypeInference::Integer => Some(TypeInstance::I32),
            TypeInference::Unknown => None,
            TypeInference::Error => None,
            TypeInference::Tuple(tuple) => {
                let mut args = vec![];
                for arg in tuple.iter() {
                    args.push(self.synthesize(&arg.borrow())?);
                }
                let instance = TypeInstance::Tuple {
                    id: 0,
                    args: args.clone(),
                };
                let len = self.tuples.len();
                let id = *self.tuples.entry(instance.id_removed()).or_insert(len);
                Some(TypeInstance::Tuple { id, args })
            }
            TypeInference::Function { args, result } => {
                let mut param = vec![];
                for arg in args.iter() {
                    param.push(self.synthesize(&arg.borrow())?);
                }
                let result = self.synthesize(&result.borrow())?;
                Some(TypeInstance::Function {
                    args: param,
                    result: Box::new(result),
                })
            }
        }
    }

    pub fn path(&mut self, path: &Path) -> Intermediate {
        let result = match path {
            Path::Simple(ident) => {
                let scope = Rc::clone(&self.stack.last().unwrap());
                let scope = scope.borrow();
                let var = scope.get_var(&ident.content).unwrap();
                let current_infer = Rc::clone(&self.inference[self.last_infer]);
                self.last_infer += 1;
                let Some(ty) = self.synthesize(&current_infer.borrow()) else {
                    panic!("Could not deduce type of {}.", ident.content);
                };
                Intermediate {
                    decl: "".into(),
                    global: "".into(),
                    ty,
                    effect: "".into(),
                    immediate: format!("{}", var.mangle.clone()),
                }
            }
        };
        result
    }

    pub fn binary(&mut self, binary: &BinaryOperation) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let lhs = self.expression(&binary.lhs);
        let rhs = self.expression(&binary.rhs);
        let result = match (lhs.ty, binary.operator.kind, rhs.ty) {
            (
                TypeInstance::I32 | TypeInstance::I64,
                TokenKind::PunctPlusSign
                | TokenKind::PunctHyphenMinus
                | TokenKind::PunctAsterisk
                | TokenKind::PunctSolidus
                | TokenKind::PunctPercentSign,
                TypeInstance::I32 | TypeInstance::I64,
            ) => Intermediate {
                ty,
                decl: lhs.decl + &rhs.decl,
                global: lhs.global + &rhs.global,
                effect: lhs.effect + &rhs.effect,
                immediate: format!(
                    "({} {} {})",
                    lhs.immediate, binary.operator.content, rhs.immediate
                ),
            },
            (
                TypeInstance::Bool,
                TokenKind::PunctAmpersandAmpersand | TokenKind::PunctVerticalLineVerticalLine,
                TypeInstance::Bool,
            ) => Intermediate {
                ty,
                decl: lhs.decl + &rhs.decl,
                global: lhs.global + &rhs.global,
                effect: lhs.effect + &rhs.effect,
                immediate: format!(
                    "({} {} {})",
                    lhs.immediate, binary.operator.content, rhs.immediate
                ),
            },
            (l, _, r) => panic!(
                "Unsupported operation `{l}` {} `{r}`",
                binary.operator.content
            ),
        };
        result
    }

    pub fn literal(&mut self, literal: &Literal) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let result = match literal {
            Literal::Decimal(decimal) => Intermediate {
                ty,
                decl: "".into(),
                global: "".into(),
                effect: "".into(),
                immediate: decimal.0.content.clone(),
            },
            Literal::Bool(bool) => Intermediate {
                ty,
                decl: "".into(),
                global: "".into(),
                effect: "".into(),
                immediate: match bool.0.kind {
                    TokenKind::KeywordTrue => "1".into(),
                    TokenKind::KeywordFalse => "0".into(),
                    _ => unreachable!(),
                },
            },
        };
        result
    }

    pub fn comparison(&mut self, comparison: &Comparison) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        let id = self.counter;
        self.counter += 1;
        writeln!(effect, "char cmp{id} = 1;").unwrap();
        let first = self.expression(&comparison.first);
        decl.push_str(&first.decl);
        global.push_str(&first.decl);
        effect.push_str(&first.effect);
        let mut prev_ty = first.ty;
        let mut prev = first.immediate;
        for (op, next) in comparison.chain.iter() {
            let next = self.expression(next);
            decl.push_str(&next.decl);
            global.push_str(&next.decl);
            writeln!(effect, "if (cmp{id}) {{").unwrap();
            effect.push_str(&next.effect);
            match (&prev_ty, op.kind, &next.ty) {
                (
                    TypeInstance::I32 | TypeInstance::I64,
                    _,
                    TypeInstance::I32 | TypeInstance::I64,
                ) => {
                    writeln!(
                        effect,
                        "cmp{id} = {prev} {} {};",
                        op.content, next.immediate
                    )
                    .unwrap();
                }
                (
                    TypeInstance::Bool,
                    TokenKind::PunctEqualsSignEqualsSign
                    | TokenKind::PunctExclamationMarkEqualsSign,
                    TypeInstance::Bool,
                ) => {
                    writeln!(
                        effect,
                        "cmp{id} = {prev} {} {};",
                        op.content, next.immediate
                    )
                    .unwrap();
                }
                _ => panic!(
                    "Could not compare type `{prev_ty}` {} `{}`.",
                    op.content, next.ty
                ),
            }
            prev_ty = next.ty;
            prev = next.immediate;
        }
        for _ in 0..comparison.chain.len() {
            effect.push('}');
        }
        effect.push('\n');
        Intermediate {
            ty,
            decl,
            global,
            effect,
            immediate: format!("cmp{id}"),
        }
    }

    pub fn print(&mut self, format: &FormatString) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut effect = String::new();
        self.feature.insert("writer_def");
        for fragment in format.fragment.iter() {
            match fragment {
                FormatFragment::Literal(literal) => {
                    self.feature.insert("write_literal");
                    writeln!(
                        effect,
                        "write_literal({}, {});",
                        double_quote(literal),
                        literal.len()
                    )
                    .unwrap();
                }
                FormatFragment::Ident { ident, .. } => {
                    let scope = Rc::clone(&self.stack.last().unwrap());
                    let scope = scope.borrow();
                    let Some(var) = scope.get_var(&ident.content) else {
                        panic!("Could not print undefined variable {}.", ident.content);
                    };
                    let Some(var_ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    match var_ty {
                        TypeInstance::I32 | TypeInstance::I64 => {
                            self.feature.insert("write_int");
                            writeln!(effect, "write_int({});", var.mangle).unwrap();
                        }
                        _ => panic!("Could not print value of type `{}`.", ty),
                    }
                }
            }
        }
        Intermediate {
            ty,
            decl: "".into(),
            global: "".into(),
            effect,
            immediate: "".into(),
        }
    }

    pub fn if_(&mut self, if_: &If) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        let condition = self.expression(&if_.condition);
        decl.push_str(&condition.decl);
        global.push_str(&condition.decl);
        effect.push_str(&condition.effect);
        let result = if let Some(falsy) = &if_.falsy {
            let id = self.counter;
            self.counter += 1;
            if ty != TypeInstance::Unit {
                writeln!(effect, "{} if{id};", ty.mapped()).unwrap();
            }
            writeln!(effect, "if ({}) {{", condition.immediate).unwrap();
            let truthy = self.block(&if_.truthy);
            decl.push_str(&truthy.decl);
            global.push_str(&truthy.global);
            effect.push_str(&truthy.effect);
            if ty != TypeInstance::Unit {
                writeln!(effect, "if{id} = {};", truthy.immediate).unwrap();
            }
            writeln!(effect, "}} else {{").unwrap();
            let falsy = self.block(falsy);
            decl.push_str(&falsy.decl);
            global.push_str(&falsy.global);
            effect.push_str(&falsy.effect);
            if ty != TypeInstance::Unit {
                writeln!(effect, "if{id} = {};", falsy.immediate).unwrap();
            }
            writeln!(effect, "}}").unwrap();
            let immediate = if ty == TypeInstance::Unit {
                "".into()
            } else {
                format!("if{id}")
            };
            Intermediate {
                ty,
                decl,
                global,
                effect,
                immediate,
            }
        } else {
            writeln!(effect, "if ({}) {{", condition.immediate).unwrap();
            let truthy = self.block(&if_.truthy);
            decl.push_str(&truthy.decl);
            global.push_str(&truthy.global);
            effect.push_str(&truthy.effect);
            writeln!(effect, "}}").unwrap();
            Intermediate {
                ty,
                decl,
                global,
                effect,
                immediate: "".into(),
            }
        };
        result
    }

    pub fn select(&mut self, select: &Select) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        let condition = self.expression(&select.condition);
        decl.push_str(&condition.decl);
        global.push_str(&condition.decl);
        effect.push_str(&condition.effect);
        let if_id = self.counter;
        self.counter += 1;
        if ty != TypeInstance::Unit {
            writeln!(effect, "{} if_{if_id};", ty.mapped()).unwrap();
        }
        writeln!(effect, "if ({}) {{", condition.immediate).unwrap();
        let truthy = self.expression(&select.truthy);
        decl.push_str(&truthy.decl);
        global.push_str(&truthy.global);
        effect.push_str(&truthy.effect);
        if ty != TypeInstance::Unit {
            writeln!(effect, "if_{if_id} = {};", truthy.immediate).unwrap();
        }
        writeln!(effect, "}} else {{").unwrap();
        let falsy = self.expression(&select.falsy);
        decl.push_str(&falsy.decl);
        global.push_str(&falsy.global);
        effect.push_str(&falsy.effect);
        if ty != TypeInstance::Unit {
            writeln!(effect, "if_{if_id} = {};", falsy.immediate).unwrap();
        }
        writeln!(effect, "}}").unwrap();
        Intermediate {
            ty,
            decl,
            global,
            effect,
            immediate: format!("if_{if_id}"),
        }
    }

    pub fn make_tuple(&mut self, make_tuple: &MakeTuple) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let tuple_count = self.tuples.len();
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        let is_new = self.tuples.len() > tuple_count;
        self.last_infer += 1;
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        let ty_mapped = ty.mapped();
        if is_new {
            writeln!(decl, "typedef struct {ty_mapped} {ty_mapped};").unwrap();
        }
        let mut definition = String::new();
        if is_new {
            writeln!(definition, "struct {ty_mapped} {{").unwrap();
        }
        let mut construct = String::new();
        let mt_id = self.counter;
        self.counter += 1;
        write!(construct, "{ty_mapped} mt{mt_id} = {{").unwrap();
        for (i, arg) in make_tuple.args.iter().enumerate() {
            let arg = self.expression(arg);
            decl.push_str(&arg.decl);
            global.push_str(&arg.global);
            effect.push_str(&arg.effect);
            construct.push_str(&arg.immediate);
            construct.push_str(", ");
            if is_new {
                writeln!(definition, "{} m{};", arg.ty.mapped(), i).unwrap();
            }
        }
        writeln!(construct, "}};").unwrap();
        effect.push_str(&construct);
        if is_new {
            writeln!(definition, "}};").unwrap();
            global.push_str(&definition);
        }
        Intermediate {
            ty,
            decl,
            global,
            effect,
            immediate: format!("mt{mt_id}"),
        }
    }

    pub fn statement(&mut self, statement: &Statement) -> Intermediate {
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        match statement {
            Statement::Nop => {}
            Statement::Input(parser) => {
                let scope = Rc::clone(&self.scopes[self.last_scope]);
                self.last_scope += 1;
                self.stack.push(Rc::clone(&scope));
                let scope = scope.borrow();
                for (ty, ident) in parser.arg.iter() {
                    let Some(ty) = scope.get_ty(&ty.content) else {
                        panic!("Could not find type `{}` in scope.", ty.content);
                    };
                    let ty = self.synthesize(&ty.borrow()).unwrap();
                    let getter = match ty {
                        TypeInstance::I32 | TypeInstance::I64 => {
                            self.feature.insert("reader_def");
                            self.feature.insert("read_white");
                            self.feature.insert("read_int");
                            "read_int()"
                        }
                        _ => panic!("Could not find parser for type `{ty}`."),
                    };
                    let var = scope.get_var(&ident.content).unwrap();
                    writeln!(effect, "{} {} = {};", ty.mapped(), var.mangle, getter).unwrap();
                }
            }
            Statement::Expression(expr) => {
                let expr = self.expression(expr);
                decl.push_str(&expr.decl);
                global.push_str(&expr.global);
                effect.push_str(&expr.effect);
            }
            Statement::Assignment(assignment) => {
                let scope = Rc::clone(&self.scopes[self.last_scope]);
                self.last_scope += 1;
                let mut rhs = self.expression(&assignment.rhs);
                decl.push_str(&rhs.decl);
                global.push_str(&rhs.global);
                effect.push_str(&rhs.effect);
                self.stack.push(Rc::clone(&scope));
                let scope = scope.borrow();
                match &assignment.lhs {
                    Pattern::Ident(ident) => {
                        let var = scope.get_var(&ident.content).unwrap();
                        writeln!(effect, "{} = {};", var.mangle, rhs.immediate).unwrap();
                    }
                    Pattern::Declaration(declaration) => {
                        let var = scope.get_var(&declaration.ident.content).unwrap();
                        let Some(var_ty) = self.synthesize(&var.ty.borrow()) else {
                            panic!("Could not deduce type.");
                        };
                        writeln!(
                            effect,
                            "{} {} = {};",
                            var_ty.mapped(),
                            var.mangle,
                            rhs.immediate
                        )
                        .unwrap();
                    }
                    Pattern::Tuple(args) => {
                        self.assign_pattern(&mut effect, &scope, &mut rhs.immediate, &args);
                    }
                }
            }
            Statement::Repeat(repeat) => {
                let times = self.expression(&repeat.times);
                decl.push_str(&times.decl);
                global.push_str(&times.global);
                effect.push_str(&times.effect);
                if !matches!(times.ty, TypeInstance::I32 | TypeInstance::I64) {
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
                decl.push_str(&block.decl);
                global.push_str(&block.global);
                effect.push_str(&block.effect);
                writeln!(effect, "}}").unwrap();
            }
            Statement::While(while_) => {
                let condition = self.expression(&while_.condition);
                decl.push_str(&condition.decl);
                global.push_str(&condition.global);
                if condition.ty != TypeInstance::Bool {
                    panic!(
                        "Could not repeat while condition of type `{}`",
                        condition.ty
                    );
                }
                writeln!(effect, "for(;;) {{").unwrap();
                effect.push_str(&condition.effect);
                writeln!(effect, "if (!{}) break;", condition.immediate).unwrap();
                let block = self.block(&while_.block);
                decl.push_str(&block.decl);
                global.push_str(&block.global);
                effect.push_str(&block.effect);
                writeln!(effect, "}}").unwrap();
            }
            Statement::Function(function) => {
                let block = Rc::clone(self.block_stack.last().unwrap());
                let block = block.borrow();
                let var = block.get_var(&function.ident.content).unwrap();
                let Some(TypeInstance::Function { args, result }) =
                    self.synthesize(&var.ty.borrow())
                else {
                    panic!("Could not deduce type.");
                };
                let scope = Rc::clone(&self.scopes[self.last_scope]);
                self.last_scope += 1;
                write!(decl, "{} {}(", result.mapped(), var.mangle).unwrap();
                write!(global, "{} {}(", result.mapped(), var.mangle).unwrap();
                for (i, (arg, ty)) in function.args.iter().zip(args.iter()).enumerate() {
                    if i > 0 {
                        decl.push_str(", ");
                        global.push_str(", ");
                    }
                    write!(decl, "{}", ty.mapped()).unwrap();
                    match arg {
                        Pattern::Ident(_) => unreachable!(),
                        Pattern::Declaration(declaration) => {
                            let scope = scope.borrow();
                            let var = scope.get_var(&declaration.ident.content).unwrap();
                            write!(global, "{} {}", ty.mapped(), var.mangle).unwrap();
                        }
                        Pattern::Tuple(_) => {
                            write!(decl, "{} a{i}", ty.mapped()).unwrap();
                        }
                    }
                }
                writeln!(decl, ");").unwrap();
                writeln!(global, ") {{").unwrap();
                for (i, arg) in function.args.iter().enumerate() {
                    match arg {
                        Pattern::Ident(_) => unreachable!(),
                        Pattern::Declaration(_) => {}
                        Pattern::Tuple(tuple) => {
                            let mut rhs = format!("a{i}");
                            let scope = scope.borrow();
                            self.assign_pattern(&mut effect, &scope, &mut rhs, &tuple);
                        }
                    }
                }
                self.stack.push(scope);
                let block = self.block(&function.block);
                self.stack.pop();
                global.push_str(&block.effect);
                writeln!(global, "return {};", block.immediate).unwrap();
                writeln!(global, "}}").unwrap();
                decl.push_str(&block.decl);
                global.push_str(&block.global);
            }
        }
        Intermediate {
            ty: TypeInstance::Unit,
            decl,
            global,
            effect,
            immediate: "".into(),
        }
    }

    pub fn block(&mut self, block: &Block) -> Intermediate {
        let len = self.stack.len();
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type");
        };
        self.last_infer += 1;
        let scope = Rc::clone(&self.scopes[self.last_scope]);
        self.last_scope += 1;
        self.stack.push(Rc::clone(&scope));
        self.block_stack.push(scope);
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        let mut immediate = String::new();
        for statement in block.statement.iter() {
            let statement = self.statement(statement);
            decl.push_str(&statement.decl);
            global.push_str(&statement.global);
            effect.push_str(&statement.effect);
        }
        if let Some(result) = block.result.as_ref().map(|expr| self.expression(expr)) {
            decl.push_str(&result.decl);
            global.push_str(&result.global);
            effect.push_str(&result.effect);
            immediate = result.immediate;
        }
        self.stack.truncate(len);
        self.block_stack.pop();
        Intermediate {
            ty,
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
                BlockExpression::If(if_) => self.if_(if_),
            },
            Expression::Nonblock(nonblock) => match nonblock {
                NonblockExpression::Path(path) => self.path(path),
                NonblockExpression::Binary(binary) => self.binary(binary),
                NonblockExpression::Literal(literal) => self.literal(literal),
                NonblockExpression::Comparison(comparison) => self.comparison(comparison),
                NonblockExpression::Print(format) => self.print(format),
                NonblockExpression::Select(select) => self.select(select),
                NonblockExpression::MakeTuple(make_tuple) => self.make_tuple(make_tuple),
                NonblockExpression::Parentheses(expr) => self.expression(expr),
                NonblockExpression::Invocation(invocation) => self.invocation(invocation),
            },
        }
    }

    pub fn translate(program: &Program) -> String {
        let name_resolver = NameResolver::new(program);
        let mut type_constructor = TypeConstructor::from_name_resolver(name_resolver);
        type_constructor.run(program);
        let mut type_checker = TypeChecker::from_type_constructor(type_constructor);
        type_checker.run(program);

        let mut translator = Codegen {
            feature: Default::default(),
            scopes: type_checker.scopes,
            inference: type_checker.inference,
            tuples: Default::default(),
            last_scope: 1,
            last_infer: 0,
            counter: 0,
            stack: vec![],
            block_stack: vec![],
        };

        let intermediate = translator.block(&Block {
            statement: program.statement.clone(),
            result: None,
        });

        let mut out = String::new();
        writeln!(out, "// namul 0.0.0").unwrap();
        writeln!(out, "#include <stdint.h>").unwrap();
        writeln!(out, "#include <unistd.h>").unwrap();
        if translator.feature.contains("writer_def") {
            out.push_str(include_str!("fragments/writer_struct.c"));
        }
        if translator.feature.contains("write_literal") {
            out.push_str(include_str!("fragments/write_literal.c"));
        }
        if translator.feature.contains("write_int") {
            out.push_str(include_str!("fragments/write_int.c"));
        }
        writeln!(out, "_Noreturn void halt() {{").unwrap();
        if translator.feature.contains("writer_def") {
            writeln!(out, "flush();").unwrap();
        }
        writeln!(out, "_exit(0);").unwrap();
        writeln!(out, "}}").unwrap();
        if translator.feature.contains("reader_def") {
            out.push_str(include_str!("fragments/reader_struct.c"));
        }
        if translator.feature.contains("read_white") {
            out.push_str(include_str!("fragments/read_white.c"));
        }
        if translator.feature.contains("read_int") {
            out.push_str(include_str!("fragments/read_int.c"));
        }
        out.push_str(&intermediate.decl);
        out.push_str(&intermediate.global);
        writeln!(out, "int main;").unwrap();
        writeln!(out, "int __libc_start_main() {{").unwrap();
        if translator.feature.contains("writer_def") {
            writeln!(out, "Writer w;").unwrap();
            writeln!(out, "w.back = 0;").unwrap();
            writeln!(out, "writer = &w;").unwrap();
        }
        if translator.feature.contains("reader_def") {
            writeln!(out, "Reader r;").unwrap();
            writeln!(out, "r.off = r.end = 0;").unwrap();
            writeln!(out, "reader = &r;").unwrap();
        }
        out.push_str(&intermediate.effect);
        writeln!(out, "halt();").unwrap();
        writeln!(out, "}}").unwrap();
        out
    }

    fn assign_pattern(
        &mut self,
        effect: &mut String,
        scope: &Scope,
        rhs: &mut String,
        args: &[Pattern],
    ) {
        for (i, arg) in args.iter().enumerate() {
            let len = rhs.len();
            write!(rhs, ".m{}", i).unwrap();
            match arg {
                Pattern::Ident(ident) => {
                    let var = scope.get_var(&ident.content).unwrap();
                    writeln!(effect, "{} = {};", var.mangle, rhs).unwrap();
                }
                Pattern::Declaration(declaration) => {
                    let var = scope.get_var(&declaration.ident.content).unwrap();
                    let Some(var_ty) = self.synthesize(&var.ty.borrow()) else {
                        panic!("Could not deduce type.");
                    };
                    writeln!(effect, "{} {} = {};", var_ty.mapped(), var.mangle, rhs).unwrap();
                }
                Pattern::Tuple(_) => todo!(),
            }
            rhs.truncate(len);
        }
    }

    fn invocation(&mut self, invocation: &Invocation) -> Intermediate {
        let current_infer = Rc::clone(&self.inference[self.last_infer]);
        let Some(ty) = self.synthesize(&current_infer.borrow()) else {
            panic!("Could not deduce type.");
        };
        self.last_infer += 1;
        let mut decl = String::new();
        let mut global = String::new();
        let mut effect = String::new();
        let callee = self.expression(&invocation.callee);
        decl.push_str(&callee.decl);
        global.push_str(&callee.global);
        effect.push_str(&callee.effect);
        let mut call = String::new();
        let id = self.counter;
        self.counter += 1;
        if ty == TypeInstance::Unit {
            write!(call, "{}(", callee.immediate).unwrap();
        } else {
            write!(call, "{} inv{id} = {}(", ty.mapped(), callee.immediate).unwrap();
        }
        for (i, arg) in invocation.args.iter().enumerate() {
            if i > 0 {
                call.push_str(", ");
            }
            let arg = self.expression(arg);
            decl.push_str(&arg.decl);
            global.push_str(&arg.global);
            effect.push_str(&arg.effect);
            call.push_str(&arg.immediate);
        }
        writeln!(call, ");").unwrap();
        effect.push_str(&call);
        Intermediate {
            ty,
            decl,
            global,
            effect,
            immediate: format!("inv{id}"),
        }
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
