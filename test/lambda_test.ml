open OUnit2
open Lambda

exception Error of string

let err s = raise (Error s)

let parse s = Parser.main Lexer.main (Lexing.from_string s)
let eval s =
  let e = Evaluation.eval Lambda.Environment.empty (parse s) in
  let len = List.length e in
  if len > 0 then List.nth e (len - 1) else err ("Failed to evaluate")

type testcase = {input: string; expected: Syntax.exp}

let tests = [
  { input = "x"; expected = Var "x" };
  { input = "λx.x"; expected = Abstraction ("x", Var "x") };
  { input = "λx.x y"; expected = Abstraction ("x", Application (Var "x", Var "y")) };
  { input = "λx.x y"; expected = Abstraction ("x", Application (Var "x", Var "y")) };
  { input = "(λx.x) x"; expected = Var "x" };
  { input = "(λl. λm. λn. l m n) (λt. λf. t) v w"; expected = Var "v" };
  { 
    input = "(λx. x x) (λx. x x)"; 
    expected = Application (
      Abstraction ("x", Application (Var "x", Var "x")), 
      Abstraction ("x", Application (Var "x", Var "x"))) 
  };
]

let suite = "test suite for lambda" >:::
  List.map (fun tt ->
    let i = Syntax.string_of_exp (eval(tt.input ^ "\n")) in
    let e = Syntax.string_of_exp tt.expected in
    tt.input >:: (fun _ -> assert_equal
    ~printer: (fun x -> x)
    i e)
    ) tests

let _ = run_test_tt_main suite
