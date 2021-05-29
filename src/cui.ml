open Reduction
open Syntax

exception Error of string

let err s = raise (Error s)

(* read eval print *)
let rec rep env =
  print_string "# ";
  flush stdout;
  let exp = Parser.main Lexer.main (Lexing.from_channel stdin) in
  print_string (string_of_exp exp);
  print_newline ();
  let b = eval exp in
  List.iter (fun b ->
  print_string ("β " ^ (string_of_exp b));
  print_newline ()) b;
  rep env
