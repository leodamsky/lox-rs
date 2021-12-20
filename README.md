# Lox in Rust (Tree-Walk Version)

## Grammar

### Statements

```
program -> declaration* EOF ;

declaration -> varDecl
             | statement ;

statement -> exprStmt
           | printStmt
           | block ;

exprStmt -> expression ";" ;

printStmt -> "print" expression ";" ;

block -> "{" declaration* "}" ;
```

### Expressions

```
expression -> equality ;

equality -> comparison ( ( "!=" | "==" ) comparison )* ;

comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term -> factor ( ( "-" | "+" ) factor )* ;

factor -> unary ( ( "/" | "*" ) unary )* ;

unary -> ( "!" | "-" ) unary
       | primary ;

primary -> NUMBER
         | STRING
         | "true"
         | "false"
         | "nil"
         | "(" expression ")"
         | IDENTIFIER;
```
