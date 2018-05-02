(*
                         CS 51 Final Project
                        MiniML -- Expressions
                             Spring 2018
*)

(*......................................................................
  Abstract syntax of MiniML expressions
 *)

type unop =
  | Negate
;;

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;

(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;

(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  let free_union (lst : expr list) : varidset =
    List.fold_left (fun set el -> SS.union set (free_vars el)) SS.empty lst in
  match exp with
  | Var x                 -> SS.singleton x
  | Num _ | Bool _        -> SS.empty
  | Unop (_, e)           -> free_vars e
  | Binop (_, e1, e2)     -> free_union [e1; e2]
  | Conditional (i, t, e) -> free_union [i; t; e]
  | Fun (x, def)          -> SS.remove x (free_vars def)
  | Let (x, def, body)    -> SS.union (SS.remove x (free_vars body))
                             (free_vars def)
  | Letrec (x, def, body) -> SS.remove x (free_union [def; body])
  | App (f, app)          -> free_union [f; app]
  | Raise | Unassigned    -> SS.empty ;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname : unit -> varid =
  let ctr = ref ~-1 in
  fun () ->
    incr ctr;
    "var" ^ (string_of_int !ctr) ;;
(*......................................................................
  Substitution

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rec sub (exp : expr) : expr =
    match exp with
    | Var x                 -> if x = var_name then repl else exp
    | Num _ | Bool _        -> exp
    | Unop (u, e)           -> Unop(u, sub e)
    | Binop (b, e1, e2)     -> Binop(b, sub e1, sub e2)
    | Conditional (i, t, e) -> Conditional(sub i, sub t, sub e)
    | Fun (x, def)          -> if x = var_name then
                                 exp
                               else if not (SS.mem x (free_vars repl)) then
                                 Fun(x, sub def)
                               else
                                 let x' = new_varname () in
                                 Fun(x', sub (subst x (Var x') def))
    | Let (x, def, body)    -> if x = var_name then
                                 Let(x, sub def, body)
                               else if not (SS.mem x (free_vars repl)) then
                                 Let(x, sub def, sub body)
                               else
                                 let x' = new_varname () in
                                 Let(x', sub def, sub (subst x (Var x') body))
    | Letrec (x, def, body) -> if x = var_name then exp
                               else Letrec(x, sub def, sub body)
    | Raise | Unassigned    -> exp
    | App (f, app)          -> App(sub f, sub app)
  in
  sub exp ;;
(*......................................................................
  String representations of expressions
 *)

(* binop_to_str : binop -> string
   Returns concrete or abstract string representation of binop *)
let binop_to_str (b : binop) (concrete : bool) : string =
  let either (c : string) (a : string) : string =
    if concrete then c else a in
  match b with
  | Plus     -> either " + " "Plus"
  | Minus    -> either " - " "Minus"
  | Times    -> either " * " "Times"
  | Equals   -> either " = " "Equals"
  | LessThan -> either " < " "LessThan" ;;

(* unop_to_str : unop -> string
   Returns concrete or abstract string representation of unop
   Not strictly neccessary pattern match but avoids "underscore of hybris" *)
let unop_to_str (u : unop) (concrete : bool) : string =
  match u with
  | Negate -> if concrete then "~-" else "Negate" ;;

(* build : string -> string list -> string
   Builds abstract string representation from operator in string form and
   arguments as a list of strings *)
let build (op : string) (arguments : string list) : string =
  let separate =
    List.fold_left (fun ac st -> if ac <> "" then ac ^ ", " ^ st else st) "" in
  op ^ "(" ^ separate arguments ^ ")" ;;

(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let exp_to_concrete_string (exp : expr) : string =
  let par (s : string) : string =
    if String.length s < 2 then s else "(" ^ s ^ ")"
  in
  let append (lst : string list) : string =
    List.fold_left (fun a s -> a ^ s) "" lst in
  let rec etcs (exp : expr) : string =
    match exp with
    | Var x                 -> x
    | Num n                 -> string_of_int n
    | Bool b                -> string_of_bool b
    | Unop (u, e)           -> append [unop_to_str u true; par (etcs e)]
    | Binop (b, e1, e2)     -> append [par (etcs e1); binop_to_str b true;
                                       par (etcs e2)]
    | Conditional (i, t, e) -> append ["if "; etcs i; " then "; etcs t;
                                       " else "; etcs e]
    | Fun (x, def)          -> append ["fun "; x; " -> "; etcs def]
    | Let (x, def, body)    -> append ["let "; x; " = "; etcs def;
                                       " in "; etcs body]
    | Letrec (x, def, body) -> append ["let rec "; x; " = "; etcs def;
                                       " in "; etcs body]
    | Raise                 -> "Raise "
    | Unassigned            -> "Unassigned"
    | App (f, app)          -> append [par (etcs f); " "; par (etcs app)]
  in
  etcs exp ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let exp_to_abstract_string (exp : expr) : string =
  let rec etas (exp : expr) : string =
    match exp with
      | Var x                 -> "Var " ^ x
      | Num n                 -> "Num " ^ string_of_int n
      | Bool b                -> "Bool " ^ string_of_bool b
      | Unop (u, e)           -> build "Unop" [unop_to_str u false; etas e]
      | Binop (b, e1, e2)     -> build "Binop" [binop_to_str b false;
                                                etas e1; etas e2]
      | Conditional (i, t, e) -> build "Conditional" [etas i; etas t; etas e]
      | Fun (x, def)          -> build "Fun" [x; etas def]
      | Let (x, def, body)    -> build "Let" [x; etas def; etas body]
      | Letrec (x, def, body) -> build "Letrec" [x; etas def; etas body]
      | Raise                 -> "Raise "
      | Unassigned            -> "Unassigned"
      | App (f, app)          -> build "App" [etas f; etas app]
  in
  etas exp ;;
