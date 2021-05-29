open Syntax

exception Error of string

let err s = raise (Error s)

let rec string_of_exp = function
    Var id -> id
  | Abstraction (id, exp) -> "(" ^ "λ" ^ id ^ ". " ^ (string_of_exp exp) ^ ")"
  | Application (exp1, exp2) -> 
    "(" ^ (string_of_exp exp1) ^ " " ^ (string_of_exp exp2) ^ ")"

(* read eval print *)
let rep =
  print_string "# ";
  flush stdout;
  let exp = Parser.main Lexer.main (Lexing.from_channel stdin) in
  print_string (string_of_exp exp);
  print_newline ();
