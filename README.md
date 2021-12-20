# Lox in Rust (Tree-Walk Version)

## Grammar

### Statements

```
program -> declaration* EOF ;

declaration -> varDecl
             | statement ;

statement -> exprStmt
           | ifStmt
           | printStmt
           | block ;

exprStmt -> expression ";" ;

ifStmt -> "if" "(" expression ")" statement
          ( "else" statement )? ;

printStmt -> "print" expression ";" ;

block -> "{" declaration* "}" ;
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
       | primary ;

primary -> NUMBER
         | STRING
         | "true"
         | "false"
         | "nil"
         | "(" expression ")"
         | IDENTIFIER;
```
