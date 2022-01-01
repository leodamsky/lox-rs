# Lox in Rust (Tree-Walk Version)

Implementation of the [Lox](https://craftinginterpreters.com/the-lox-language.html) language in Rust.
`main` branch doesn't include additional features from book's challenges. Checkout a specific branch
e.g. `static-methods` to test some extra features.

## How To Run

Interpreter can interpret file contents as well as run as a REPL.

### File Interpretation

To run interpret agaist a file with a source code, run: `cargo run <file-path>`. There are some examples in
the `examples/` folder. So you can try:

```shell
# tests 'super' keyword
cargo run --release examples/super.lox
```

### REPL

To run it as a REPL, run: `cargo run --release`. Then try typing in some commands e.g.:

```
> // some comment
> var variableName = "hello";
> print variableName;
```

## How To Install

Lox can be installed as a global binary from sources:
```shell
cargo install --path .
```
Now try: `lox examples/return.lox` to run inefficient Fibonacci number computation.

## How To Uninstall

Global binary can be uninstalled via:
```shell
cargo uninstall lox
```

## Grammar

### Statements

```
program -> declaration* EOF ;

declaration -> classDecl
             | funDecl
             | varDecl
             | statement ;

classDecl -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;

funDecl -> "fun" function ;

varDecl -> "var" IDENTIFIER ( "=" expression )? ";" ;

statement -> exprStmt
           | forStmt
           | ifStmt
           | printStmt
           | returnStmt
           | whileStmt
           | block ;

exprStmt -> expression ";" ;

forStmt -> "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;

ifStmt -> "if" "(" expression ")" statement
          ( "else" statement )? ;

printStmt -> "print" expression ";" ;

returnStmt -> "return" expression? ";" ;

whileStmt -> "while" "(" expression ")" statement ;

block -> "{" declaration* "}" ;

function -> INDENTIFIER "(" parameters? ")" block ;

parameters -> IDENTIFIER ( "," IDENTIFIER )* ;
```

### Expressions

```
expression -> assignement ;

assignment -> ( call "." )? IDENTIFIER "=" assignment
            | logic_or ;

logic_or -> logic_and ( "or" logic_and )* ;

logic_and -> equality ( "and" equality )* ;

equality -> comparison ( ( "!=" | "==" ) comparison )* ;

comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term -> factor ( ( "-" | "+" ) factor )* ;

factor -> unary ( ( "/" | "*" ) unary )* ;

unary -> ( "!" | "-" ) unary
       | call ;

call -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;

arguments -> expression ( "," expression )* ;

primary -> NUMBER
         | STRING
         | "true"
         | "false"
         | "nil"
         | "this"
         | "(" expression ")"
         | IDENTIFIER
         | "super" "." IDENTIFIER;
```
