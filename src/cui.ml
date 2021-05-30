open Evaluation
open Syntax

exception Error of string

let err s = raise (Error s)

let parse s = Parser.main Lexer.main (Lexing.from_string (s ^ "\n"))

let initial_env = Environment.from_list [
  ("test", parse "λl. λm. λn. l m n");
  ("tru", parse "λt. λf. t");
  ("fls", parse "λt. λf. f");
  ("iszro", parse "λm. m (λx. fls) tru");
  ("c0", parse "λs. λz. z");
  ("c1", parse "λs. λz. s z");
  ("scc", parse "λn. λs. λz. s (n s z)");
  ("plus", parse "λm. λn. λs. λz. m s (n s z)");
  ("times", parse "λm. λn. m (plus n) c0");
  ("pair", parse "λf. λs. λb. b f s");
  ("fst", parse "λp. p tru");
  ("snd", parse "λp. p fls");
  ("prd", parse "λm. fst (m (λp. pair (snd p) (plus c1 (snd p))) (pair c0 c0))");
  ("fix", parse "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))");
  ("factorial", parse "fix (λf. λn. test (iszro n) (λx. c1) (λx. (times n (f (prd n)))) c0)");
  ("nil", parse "λc. λn. n");
  ("cons", parse "λh. λt. λc. λn. c h (t c n)");
  ("head", parse "λh. λt. λc. λn. c h (t c n)");
]


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
