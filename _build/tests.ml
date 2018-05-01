open Expr ;;
open Evaluation ;;
open Miniml ;;


(* TESTS FOR FUNCTIONS IN EXPR *)

(* test expressions for all paths *)
let ste = str_to_exp ;;
let va = ste "x;;" ;;
let nu = ste "1;;" ;;
let bo = ste "true;;" ;;
let un = ste "~-x;;" ;;
let bip = ste "x + 1;;" ;;
let bim = ste "x - 3;;" ;;
let bit = ste "3 * x;;" ;;
let bie = ste "x = 2;;" ;;
let bil = ste "x < 7;;" ;;
let co = ste "if x = 7 then true else y;;" ;;
let fu = ste "fun x -> x + x;;" ;;
let fu1 = ste "fun y -> f (x + y);;" ;;
let fu2 = ste "fun y -> x + y;;" ;;
let le = ste "let x = 1 in x + x;;" ;;
let le1 = ste "let x = 1 in x + y;;" ;;
let re = ste "let rec x = fun y -> x in x;;" ;;
let ap = ste "let d = fun x -> 2 * x in d 3;;" ;;
let ap1 = ste "f (x + y);;" ;;

(* tests for exp_to_concrete_string *)
let test_exp_to_concrete_string () =
  let etcs = exp_to_concrete_string in
  assert(etcs va = "x") ;
  assert(etcs nu = "1") ;
  assert(etcs bo = "true") ;
  assert(etcs un = "~-x") ;
  assert(etcs bip = "x + 1") ;
  assert(etcs bim = "x - 3") ;
  assert(etcs bit = "3 * x") ;
  assert(etcs bie = "x = 2") ;
  assert(etcs bil = "x < 7") ;
  assert(etcs co = "if x = 7 then true else y") ;
  assert(etcs fu = "fun x -> x + x") ;
  assert(etcs le = "let x = 1 in x + x") ;
  assert(etcs re = "let rec x = fun y -> x in x") ;
  assert(etcs ap = "let d = fun x -> 2 * x in d 3") ;
  assert(etcs Raise = "Raise ") ;
  assert(etcs Unassigned = "Unassigned") ;;

(* tests for exp_to_abstract_string *)
let test_exp_to_abstract_string () =
  let etas = exp_to_abstract_string in
  assert(etas va = "Var x") ;
  assert(etas nu = "Num 1") ;
  assert(etas bo = "Bool true") ;
  assert(etas un = "Unop(Negate, Var x)") ;
  assert(etas bip = "Binop(Plus, Var x, Num 1)") ;
  assert(etas bim = "Binop(Minus, Var x, Num 3)") ;
  assert(etas bit = "Binop(Times, Num 3, Var x)") ;
  assert(etas bie = "Binop(Equals, Var x, Num 2)") ;
  assert(etas bil = "Binop(LessThan, Var x, Num 7)") ;
  assert(etas co = "Conditional(Binop(Equals, Var x, Num 7), Bool true, Var y)") ;
  assert(etas fu = "Fun(x, Binop(Plus, Var x, Var x))") ;
  assert(etas le = "Let(x, Num 1, Binop(Plus, Var x, Var x))") ;
  assert(etas re = "Letrec(x, Fun(y, Var x), Var x)") ;
  assert(etas ap = "Let(d, Fun(x, Binop(Times, Num 2, Var x)), App(Var d, Num 3))") ;
  assert(etas Raise = "Raise ") ;
  assert(etas Unassigned = "Unassigned") ;;

(* tests for free_vars *)
let test_free_vars () =
  let test_fv (exp : expr) (vars : string list) : bool =
    same_vars (free_vars exp) (vars_of_list vars) in
  assert(test_fv va ["x"]) ;
  assert(test_fv nu []) ;
  assert(test_fv bo []) ;
  assert(test_fv un ["x"]) ;
  assert(test_fv bip ["x"]) ;
  assert(test_fv bim ["x"]) ;
  assert(test_fv bit ["x"]) ;
  assert(test_fv bie ["x"]) ;
  assert(test_fv bil ["x"]) ;
  assert(test_fv co ["x"; "y"]) ;
  assert(test_fv fu []) ;
  assert(test_fv fu1 ["f"; "x"]) ;
  assert(test_fv le []) ;
  assert(test_fv re []) ;
  assert(test_fv ap []) ;
  assert(test_fv ap1 ["f"; "x"; "y"]) ;;

(* tests for new_varname *)
let test_new_varname () =
  assert(new_varname () = "var0") ;
  assert(new_varname () = "var1") ;
  assert(new_varname () = "var2") ;;

(* tests for subst *)
let test_subst () =
  let repl = ste "2;;" in
  let repl' = ste "x2;;" in
  assert(subst "x" repl va = ste "2;;") ;
  assert(subst "x" repl nu = nu) ;
  assert(subst "x" repl bo = bo) ;
  assert(subst "x" repl un = ste "~-2;;") ;
  assert(subst "x" repl' un = ste "~-x2;;") ;
  assert(subst "x" repl bip = ste "2 + 1;;") ;
  assert(subst "x" repl' bil = ste "x2 < 7;;") ;
  assert(subst "x" repl co = ste "if 2 = 7 then true else y;;") ;
  assert(subst "y" repl' co = ste "if x = 7 then true else x2;;") ;
  assert(subst "x" repl fu = ste "fun x -> x + x;;") ;
  assert(subst "x" repl fu1 = ste "fun y -> f (2 + y);;") ;
  assert(subst "y" repl fu1 = ste "fun y -> f (x + y);;") ;
  assert(subst "x" (ste "y + 2;;") fu2 = ste "fun var3 -> y + 2 + var3;;") ;
  assert(subst "y" repl le1 = ste "let x = 1 in x + 2;;") ;
  assert(subst "x" repl ap1 = ste "f (2 + y);;") ;;


(* TESTS FOR FUNCTIONS IN EVALUATION *)

(* test expressions for all paths in evaluations *)
let nu = ("1;;", "1;;") ;;
let bo = ("true;;", "true;;") ;;
let un = ("~-(~-3);;", "3;;") ;;
let bi = ("~-(2 - 10);;", "8;;") ;;
let bi' = ("true=false;;", "false;;")
let co = ("if 2 = 7 then true else false;;", "false;;") ;;
let fu = ("fun x -> x + x;;", "fun x -> x + x;;") ;;
let le = ("let x = 1 in x + x;;", "2;;") ;;
let le' = ("let f = fun x -> x in f f 3;;", "3;;") ;;
let re = ("let rec f = fun x -> if x=0 then 1 else x*f(x-1) in f 4;;", "24;;");;
let re' = ("let rec f = fun x -> if x=0 then x else f(x-1) in f 2 ;;", "0;;") ;;
let ap = ("let double = fun x -> 2 * x in double (double 3);;", "12;;") ;;
let e = ("let square = fun x -> x * x in let y = 3 in square y;;", "9;;") ;;
let e1 = ("let i=fun x->x in let s=fun x->x*x in let y=3 in i s y;;", "9;;") ;;
let exp = "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;" ;;
let lex = (exp, "4;;") ;;
let dyn = (exp, "5;;") ;;

let test_env_module () =
  let open Env in
  (* some values for testing *)
  let one = Val (Num 1) in
  let two = Val (Num 2) in
  let y = Val (ste "y;;") in
  let three = Val (Num 3) in
  (* test create and close*)
  let env = create() in
  let closure = close va env in
  assert((close va env) = Closure(va, env)) ;
  (* test lookup and extend *)
  assert(try (lookup env "notincluded" = Val(va)) with EvalError _ -> true) ;
  let env = extend env "x" (ref one) in
  assert(lookup env "x" = one) ;
  let env = extend env "x2" (ref two) in
  assert(lookup env "x" = one) ;
  assert(lookup env "x2" = two) ;
  let env = extend env "x3" (ref y) in
  assert(lookup env "x3" = y) ;
  let env = extend env "x3" (ref three) in
  assert(lookup env "x3" = three) ;
  let env = extend env "x4" (ref closure) in
  (* test to_string functions *)
  let vts = value_to_string in
  let ets = env_to_string in
  assert(vts one = "Val(1)") ;
  assert(vts two = "Val(2)") ;
  assert(vts y = "Val(y)") ;
  assert(vts closure = "Closure(x, [])") ;
  assert(ets env = "x -> Val(1), x2 -> Val(2), x3 -> Val(3), x4 -> Closure(x, []), []") ;;


let test_eval eval dynamic =
  let test_e (str : string * string) : bool =
    let env = Env.create () in
    match str with
    | exp , sol -> eval (ste exp) env = Env.Val (ste sol) in
  let test_error (exp : expr) : bool =
    let env = Env.create () in
    try
      eval exp env = Env.Val (ste "let x = 2 in x;;")
    with
      EvalError _ | EvalException-> true
  in
  assert(test_error (ste "x;;")) ;
  assert(test_error (ste "3 + x;;")) ;
  assert(test_error (ste "true + false;;")) ;
  assert(test_error (ste "if 3 then 2 else 1;;")) ;
  assert(test_error Raise) ;
  assert(test_error Unassigned) ;
  assert(test_e nu) ;
  assert(test_e bo) ;
  assert(test_e un) ;
  assert(test_e bi) ;
  assert(test_e bi') ;
  assert(test_e co) ;
  assert(test_e fu) ;
  assert(test_e le) ;
  assert(test_e le') ;
  assert(test_e re) ;
  assert(test_e ap) ;
  assert(test_e e) ;
  assert(test_e e1) ;
  if dynamic then assert(test_e dyn)
  else assert(test_e lex) ;;


(* run all tests *)
let run_tests () =
  test_exp_to_concrete_string () ;
  test_exp_to_abstract_string () ;
  test_free_vars () ;
  test_new_varname () ;
  test_subst () ;
  test_env_module () ;
  test_eval eval_s false ;
  test_eval eval_d true ;
  test_eval eval_l false ;
  () ;;

let _ = run_tests () ;;

print_endline "All tests passed :)" ;;

