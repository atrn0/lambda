%{
open Syntax
%}

%token BACKSLASH LPAREN RPAREN DOT 
%token <Syntax.id> ID
%token EOL

%start <Syntax.exp>  main

%%

let main :=
    ~ = expr; EOL; <>

// expr = id | "\" id "." expr | expr expr
let expr :=
    ~ = application; <>

// associativity: left
let application :=
    ~ = application; ~ = abstraction; { Application (application, abstraction) }
  | ~ = abstraction; <>

// associativity: right
let abstraction :=
    BACKSLASH; id = ID; DOT; ~ = abstraction; { Abstraction (id, abstraction) }
  | ~ = a; <>

let a :=
  | id = ID; { Var id }
  | LPAREN; ~ = expr; RPAREN; <>
