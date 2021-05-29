open Reduction

exception Error of string

let err s = raise (Error s)

(* read eval print *)
let rep =
  print_string "# ";
  flush stdout;
  let exp = Parser.main Lexer.main (Lexing.from_channel stdin) in
  print_string (string_of_exp exp);
  print_newline ();
  let b = beta exp in
  print_string ("β " ^ (string_of_exp b));
  print_newline ();

