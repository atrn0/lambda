open OUnit2
open Lambda

exception Error of string

let err s = raise (Error s)

let eval s = 
  let t = try Lambda.Cui.parse s with _ -> err ("parse error") in
  Evaluation.eval Lambda.Cui.initial_env t
  {verbose=false; strategy=Lambda.Evaluation.CallByValue}

type testcase = {input: string; expected: Syntax.exp}

let tests = [
  { input = "x"; expected = Var "x" };
  { input = "λx.x"; expected = Abstraction ("x", Var "x") };
  { input = "λx.x y"; expected = Abstraction ("x", Application (Var "x", Var "y")) };
  { input = "λx.x y"; expected = Abstraction ("x", Application (Var "x", Var "y")) };
  { input = "(λx.x) x"; expected = Application (Abstraction ("x", Var "x"), Var "x") };
  { input = "(λx.x) (λz.z)"; expected = Abstraction ("z", Var "z") };
  { input = "test tru (λv.v) (λw.w)"; expected = Abstraction ("v", Var "v") };
  { 
    (* omega *)
    input = "(λx. x x) (λx. x x)"; 
    expected = Application (
      Abstraction ("x", Application (Var "x", Var "x")),
      Abstraction ("x", Application (Var "x", Var "x")))
  };
  { input = "(λx. (λx. x)) (λv.v)"; expected = Abstraction ("x'", Var "x'") };
  { input = "(λx. (λx. (λx. x))) (λv.v) (λw.w)"; expected = Abstraction ("x''", Var "x''") };
  { input = "equal c0 c0"; expected = Abstraction ("t", Abstraction ("f'", Var "t")) };
  {
    input = "equal (scc c0) c1"; 
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
  { 
    input = "equal (plus c1 (scc c1)) (scc (scc c1))";
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
  { 
    input = "equal (times (scc c1) (scc (scc c1))) (scc (scc (scc (scc (scc c1)))))"; 
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
  { 
    input = "equal (fst (pair c0 c1)) c0";
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
  {
    input = "equal (snd (pair c0 c1)) c1"; 
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
  { 
    input = "equal (prd c1) c0";
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
  { 
    input = "equal (prd c0) c0";
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
  { 
    input = "equal (sub (scc c1) c1) c1";
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
  { 
    input = "factorial c0";
    expected = Abstraction ("s", Abstraction ("z", Application (Var "s", Var "z")))
  };
  { 
    input = "equal (factorial (scc (scc c1))) (scc (scc (scc (scc (scc c1)))))";
    expected = Abstraction ("t", Abstraction ("f'", Var "t"))
  };
]

let suite = "test suite for lambda" >:::
  List.map (fun tt ->
    let i = Syntax.string_of_exp (eval(tt.input)) in
    let e = Syntax.string_of_exp tt.expected in
    tt.input >:: (fun _ -> assert_equal
    ~printer: (fun x -> x)
    e i)
    ) tests

let _ = run_test_tt_main suite
