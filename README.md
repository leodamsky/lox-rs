# Lox in Rust (Tree-Walk Version)

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
         | "(" expression ")"
         | IDENTIFIER;
```
