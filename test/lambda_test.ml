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
  { input = "(λx. (λx. x)) y"; expected = Abstraction ("x'", Var "x'") };
  { input = "(λx. (λx. (λx. x))) y z"; expected = Abstraction ("x''", Var "x''") };
  { input = "(λx. (λz. x)) z"; expected = Abstraction ("z'", Var "z") };
  { input = "(λx. (λz. x)) z"; expected = Abstraction ("z'", Var "z") };
  { 
    input = "(λn. λs. λz. s (n s z)) (λs. λz. z)"; 
    expected = Abstraction ("s", Abstraction ("z", Application (Var "s", Var "z")))
  };
  { 
    input = "(λm. λn. λs. λz. m s (n s z)) (λs. λz. s z) ((λn. λs. λz. s (n s z)) (λs. λz. z))"; 
    expected = Abstraction ("s", Abstraction ("z", Application (Var "s", Application (Var "s", Var "z"))))
  };
  { 
    input = "(λm. λn. m ((λm. λn. λs. λz. m s (n s z)) n) (λs. λz. z)) (λs. λz. s z) (λs. λz. s(s z))"; 
    expected = Abstraction ("s", Abstraction ("z'", Application (Var "s", Application (Var "s", Var "z'"))))
  };
  { 
    input = "(λp. p (λt. λf. t)) ((λf. λs. λb. b f s) x y)"; 
    expected = Var "x"
  };
  { 
    input = "(λp. p (λt. λf. f)) ((λf. λs. λb. b f s) x y)"; 
    expected = Var "y"
  };
  { 
    input = "(λm. (((m (λp. ((λs. (λb. ((b (p (λt. (λf. f)))) s))) (((λm. (λn. (λs. (λz. ((m s) ((n s) z)))))) (λs. (λz. (s z)))) ((λp. (p (λt. (λf. f)))) p))))) (((λf. (λs. (λb. ((b f) s)))) (λs. (λz. z))) (λs. (λz. z)))) (λt. (λf. t)))) ((λn. λs. λz. s (n s z)) (λs. λz. s z))";
    expected = Abstraction ("s", Abstraction ("z''", Application (Var "s", Var "z''")))
  };
]

let suite = "test suite for lambda" >:::
  List.map (fun tt ->
    let i = Syntax.string_of_exp (eval(tt.input ^ "\n")) in
    let e = Syntax.string_of_exp tt.expected in
    tt.input >:: (fun _ -> assert_equal
    ~printer: (fun x -> x)
    e i)
    ) tests

let _ = run_test_tt_main suite
