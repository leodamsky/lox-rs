# Lox in Rust (Tree-Walk Version)

## Grammar

### Statements

```
program -> declaration* EOF ;

declaration -> funDecl
             | varDecl
             | statement ;

funDecl -> "fun" function ;

statement -> exprStmt
           | forStmt
           | ifStmt
           | printStmt
           | whileStmt
           | block
           | function;

exprStmt -> expression ";" ;

forStmt -> "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;

ifStmt -> "if" "(" expression ")" statement
          ( "else" statement )? ;

printStmt -> "print" expression ";" ;

whileStmt -> "while" "(" expression ")" statement ;

block -> "{" declaration* "}" ;

function -> INDENTIFIER "(" parameters? ")" block ;

parameters -> IDENTIFIER ( "," IDENTIFIER )* ;
```

### Expressions

```
expression -> assignement ;

assignment -> IDENTIFIER "=" assignment
            | logic_or ;

logic_or -> logic_and ( "or" logic_and )* ;

logic_and -> equality ( "and" equality )* ;

equality -> comparison ( ( "!=" | "==" ) comparison )* ;

comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term -> factor ( ( "-" | "+" ) factor )* ;

factor -> unary ( ( "/" | "*" ) unary )* ;

unary -> ( "!" | "-" ) unary
       | call ;

call -> primary ( "(" arguments? ")" )* ;

arguments -> expression ( "," expression )* ;

primary -> NUMBER
         | STRING
         | "true"
         | "false"
         | "nil"
         | "(" expression ")"
         | IDENTIFIER;
```
