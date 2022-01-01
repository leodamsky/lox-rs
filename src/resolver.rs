use crate::{AssignExpr, Expr, FunctionStmt, Lox, Stmt, SuperExpr, ThisExpr, Token, VariableExpr};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub(crate) trait Resolve {
    fn resolve(&self, ctx: &mut Context);
}

impl Resolve for Stmt {
    fn resolve(&self, ctx: &mut Context) {
        fn resolve_function(
            function: &FunctionStmt,
            ctx: &mut Context,
            function_type: FunctionType,
        ) {
            let enclosing = ctx.cur_function;
            ctx.cur_function = function_type;

            ctx.begin_scope();
            for param in &function.params {
                ctx.declare(param);
                ctx.define(param);
            }
            for statement in &function.body {
                statement.resolve(ctx);
            }
            ctx.end_scope();

            ctx.cur_function = enclosing;
        }

        match self {
            Stmt::Block(statements) => {
                ctx.begin_scope();
                for statement in statements {
                    statement.resolve(ctx);
                }
                ctx.end_scope();
            }
            Stmt::Class {
                name,
                superclass,
                methods,
            } => {
                let enclosing = ctx.cur_class;
                ctx.cur_class = ClassType::Class;

                ctx.declare(name);
                ctx.define(name);
                if let Some(superclass) = superclass {
                    match superclass {
                        Expr::Variable(variable) => {
                            if variable.name.lexeme == name.lexeme {
                                ctx.lox.syntax_error(
                                    &variable.name,
                                    "A class can't inherit from itself.",
                                );
                            }
                        }
                        _ => unreachable!(),
                    }
                    ctx.cur_class = ClassType::Subclass;
                    superclass.resolve(ctx);
                    ctx.begin_scope();
                    ctx.scopes
                        .last_mut()
                        .unwrap()
                        .insert_variable("super".to_string().into(), true);
                }

                ctx.begin_scope();
                ctx.scopes
                    .last_mut()
                    .unwrap()
                    .insert_variable("this".to_string().into(), true);

                for method in methods {
                    let mut declaration = FunctionType::Method;
                    if method.name.lexeme.as_str() == "init" {
                        declaration = FunctionType::Initializer;
                    }
                    resolve_function(method, ctx, declaration);
                }

                ctx.end_scope();

                if superclass.is_some() {
                    ctx.end_scope();
                }

                ctx.cur_class = enclosing;
            }
            Stmt::Expression(expr) => {
                expr.resolve(ctx);
            }
            Stmt::Function(stmt) => {
                ctx.declare(&stmt.name);
                ctx.define(&stmt.name);
                resolve_function(stmt, ctx, FunctionType::Function);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.resolve(ctx);
                then_branch.resolve(ctx);
                if let Some(else_branch) = else_branch {
                    else_branch.resolve(ctx);
                }
            }
            Stmt::Print(expr) => {
                expr.resolve(ctx);
            }
            Stmt::Return { keyword, value } => {
                if let FunctionType::None = ctx.cur_function {
                    ctx.lox
                        .syntax_error(keyword, "Can't return from top-level code.");
                }

                if let Some(value) = value {
                    if let FunctionType::Initializer = ctx.cur_function {
                        ctx.lox.syntax_error(
                            keyword,
                            "Can't return a value from from an initializer.",
                        );
                    }
                    value.resolve(ctx);
                }
            }
            Stmt::While { condition, body } => {
                condition.resolve(ctx);
                body.resolve(ctx);
            }
            Stmt::Var { name, initializer } => {
                ctx.declare(name);
                if let Some(expr) = initializer {
                    expr.resolve(ctx);
                }
                ctx.define(name);
            }
        }
    }
}

impl Resolve for Expr {
    fn resolve(&self, ctx: &mut Context) {
        fn resolve_local(ctx: &mut Context, expr_id: usize, name: &Token) {
            for i in (0..ctx.scopes.len()).rev() {
                if let Some(variable) = ctx.scopes[i].variables.get(&name.lexeme) {
                    ctx.binder.borrow_mut().bind(
                        expr_id,
                        ctx.scopes.len() - 1 - i,
                        variable.index,
                    );
                    return;
                }
            }
        }

        match self {
            Expr::Assign(AssignExpr { id, name, value }) => {
                value.resolve(ctx);
                resolve_local(ctx, *id, name);
            }
            Expr::Binary { left, right, .. } => {
                left.resolve(ctx);
                right.resolve(ctx);
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                callee.resolve(ctx);
                for argument in arguments {
                    argument.resolve(ctx);
                }
            }
            Expr::Get { object, .. } => {
                object.resolve(ctx);
            }
            Expr::Grouping(expr) => {
                expr.resolve(ctx);
            }
            Expr::Literal(_) => {}
            Expr::Logical { left, right, .. } => {
                left.resolve(ctx);
                right.resolve(ctx);
            }
            Expr::Set { object, value, .. } => {
                object.resolve(ctx);
                value.resolve(ctx);
            }
            Expr::This(ThisExpr { id, keyword }) => {
                if let ClassType::None = ctx.cur_class {
                    ctx.lox
                        .syntax_error(keyword, "Can't use 'this' outside of a class.");
                }
                resolve_local(ctx, *id, keyword);
            }
            Expr::Unary { right, .. } => {
                right.resolve(ctx);
            }
            Expr::Variable(VariableExpr { id, name }) => {
                if let Some(variable) = ctx
                    .scopes
                    .last()
                    .and_then(|s| s.variables.get(&name.lexeme))
                {
                    if !variable.defined {
                        ctx.lox.syntax_error(
                            name,
                            "Can't read local variable in its own initializer.",
                        );
                    }
                }

                resolve_local(ctx, *id, name);
            }
            Expr::Super(SuperExpr { id, keyword, .. }) => {
                match ctx.cur_class {
                    ClassType::None => ctx
                        .lox
                        .syntax_error(keyword, "Can't use 'super' outside of a class."),
                    ClassType::Class => ctx
                        .lox
                        .syntax_error(keyword, "Can't use 'super' in a class with no superclass."),
                    // we could move resolve_local here but we shouldn't
                    // because resolver tries to resolve as many errors as possible
                    // and it's reasonable to pretend we're following a happy path
                    // (and we actually aren't broken here)
                    ClassType::Subclass => {}
                }
                resolve_local(ctx, *id, keyword);
            }
        }
    }
}

pub(crate) struct Context<'a> {
    lox: &'a mut Lox,
    binder: Rc<RefCell<Binder>>,
    scopes: Vec<Scope>,
    cur_function: FunctionType,
    cur_class: ClassType,
}

impl<'a> Context<'a> {
    pub(crate) fn new(lox: &mut Lox) -> Context {
        let binder = Rc::clone(&lox.binder);
        Context {
            lox,
            binder,
            scopes: vec![],
            cur_function: FunctionType::default(),
            cur_class: ClassType::default(),
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.variables.contains_key(&name.lexeme) {
                self.lox
                    .syntax_error(name, "Already a variable with this name in this scope.");
            }

            scope.insert_variable(Rc::clone(&name.lexeme), false);
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            let variable = scope.variables.get_mut(&name.lexeme).unwrap();
            variable.defined = true;
        }
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

#[derive(Default)]
struct Scope {
    next_index: usize,
    variables: HashMap<Rc<String>, Variable>,
}

impl Scope {
    fn insert_variable(&mut self, name: Rc<String>, defined: bool) {
        let index = self.next_index;
        self.next_index += 1;
        let variable = Variable { defined, index };
        self.variables.insert(name, variable);
    }
}

struct Variable {
    defined: bool,
    index: usize,
}

#[derive(Copy, Clone)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

impl Default for FunctionType {
    fn default() -> Self {
        FunctionType::None
    }
}

#[derive(Copy, Clone)]
enum ClassType {
    None,
    Class,
    Subclass,
}

impl Default for ClassType {
    fn default() -> Self {
        ClassType::None
    }
}

pub(crate) struct Binding {
    /// Number of scopes that we need to just through to get the variable.
    pub(crate) hops: usize,
    /// Positional index of the variable in a single scope.
    pub(crate) index: usize,
}

#[derive(Default)]
pub(crate) struct Binder {
    locals: HashMap<usize, Binding>,
}

impl Binder {
    pub(crate) fn resolve(&self, id: usize) -> Option<&Binding> {
        self.locals.get(&id)
    }

    fn bind(&mut self, id: usize, hops: usize, index: usize) {
        self.locals.insert(id, Binding { hops, index });
    }
}
