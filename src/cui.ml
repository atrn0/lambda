open Evaluation
open Syntax

exception Error of string

let err s = raise (Error s)

let parse s = Parser.main Lexer.main (Lexing.from_string (s ^ "\n"))

let initial_env = Environment.from_list [
  ("test", parse "λl. λm. λn. l m n");
  ("tru", parse "λt. λf. t");
  ("fls", parse "λt. λf. f");
  ("and", parse "λx. λy. x y fls");
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
  ("sub", parse "λm. λn. n prd m");
  ("equal", parse "λm. λn. and (iszro (sub m n)) (iszro (sub n m))");
  ("fix", parse "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))");
  ("factorial", parse "fix (λf. λn. test (iszro n) (λx. c1) (λx. (times n (f (prd n)))) c0)");
  ("nil", parse "λc. λn. n");
  ("cons", parse "λh. λt. λc. λn. c h (t c n)");
  ("head", parse "λh. λt. λc. λn. c h (t c n)");
]

let usage_msg = "lambda [-v] [-s <strategy>]"
let opt_verbose = ref false
let opt_strategy = ref "full"
let speclist =
  [("-v", Arg.Set opt_verbose, "Output all reduction steps");
   ("-s", Arg.Set_string opt_strategy, "Set strategy (full or call_by_value). default strategy is call_by_value")]
let strategy_of_str s = match s with "full" -> Full | _ -> CallByValue


(* read eval print loop *)
let repl () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  while true do
    print_string "# ";
    flush stdout;
    let t = Parser.main Lexer.main (Lexing.from_channel stdin) in
    let b = eval initial_env t { verbose = !opt_verbose; strategy = strategy_of_str !opt_strategy } in
    print_string ("→ " ^ (string_of_exp b));
    print_newline ();
  done
