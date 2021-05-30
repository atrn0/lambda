open Evaluation
open Syntax

exception Error of string

let err s = raise (Error s)

let initial_env =
  Environment.extend "id" (Abstraction ("_x", Var "_x")) Environment.empty


(* read eval print *)
let rec rep env =
  print_string "# ";
  flush stdout;
  let exp = Parser.main Lexer.main (Lexing.from_channel stdin) in
  let b = eval initial_env exp in
  List.iter (fun b ->
  print_string ("β " ^ (string_of_exp b));
  print_newline ()) b;
  rep env
