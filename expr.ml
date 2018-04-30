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
let free_vars (exp : expr) : varidset =
  let rec frv (exp : expr) : varidset =
    match exp with
    | Var v -> SS.singleton v
    | Num _ | Bool _ -> SS.empty
    | Unop (_, e) -> frv e
    | Binop (_, e1, e2) -> SS.union (frv e1) (frv e2)
    | Conditional (e1, e2, e3) -> SS.union (frv e1) (SS.union (frv e2) (frv e3))
    | Fun (v, e) -> SS.remove v (frv e)
    | Let (v, e1, e2) -> SS.union (SS.remove v (frv e2)) (frv e1)
    | Letrec (v, e1, e2) -> SS.remove v (SS.union (frv e1) (frv e2))
    | App (e1, e2) -> SS.union (frv e1) (frv e2)
    | Raise | Unassigned -> SS.empty
  in
  frv exp ;;

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
    | Var v -> if v = var_name then repl else exp
    | Num _ | Bool _ -> exp
    | Unop (v, e) -> Unop(v, sub e)
    | Binop (v, e1, e2) -> Binop(v, sub e1, sub e2)
    | Conditional (e1, e2, e3) -> Conditional(sub e1, sub e2, sub e3)
    | Fun (v, e) -> if v = var_name then exp
                    else if not (SS.mem v (free_vars repl))
                      then Fun(v, sub e)
                    else let v' = new_varname () in
                      Fun(v', sub (subst v (Var v') e))
    | Let (v, e1, e2) -> if v = var_name then Let(v, sub e1, e2)
                         else if not (SS.mem v (free_vars repl))
                           then Let(v, sub e1, sub e2)
                         else let v' = new_varname () in
                           Let(v', sub e1, sub (subst v (Var v') e2))
    | Letrec (v, e1, e2) -> if v = var_name then exp
                            else Letrec(v, sub e1, sub e2)
    | Raise | Unassigned -> exp
    | App (e1, e2) -> App(sub e1, sub e2)
  in
  sub exp ;;
(*......................................................................
  String representations of expressions
 *)

(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let exp_to_concrete_string (exp : expr) : string =
  let par (s : string) : string =
    if String.length s < 2 then s
    else "(" ^ s ^ ")"
  in
  let unop_to_string (u : unop) : string =
    match u with
    | Negate -> "~-"
  in
  let binop_to_string (b : binop) : string =
    match b with
    | Plus -> " + "
    | Minus -> " - "
    | Times -> " * "
    | Equals -> " = "
    | LessThan -> " < "
  in
  let rec conv (exp : expr) : string =
    match exp with
    | Var v -> v
    | Num i -> string_of_int i
    | Bool b -> string_of_bool b
    | Unop (u, e) -> unop_to_string u ^  par (conv e)
    | Binop (b, e1, e2) -> par (conv e1) ^ binop_to_string b ^ par (conv e2)
    | Conditional (e1, e2, e3) -> "if " ^ conv e1 ^ " then " ^ conv e2 ^
                                  " else " ^ conv e3
    | Fun (v, e) -> "fun " ^ v ^ " -> " ^ conv e
    | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ conv e1 ^ " in " ^ conv e2
    | Letrec (v, e1, e2) -> "let rec " ^ v ^ " = " ^ conv e1 ^ " in " ^ conv e2
    | Raise -> "Raise "
    | Unassigned -> "Unassigned"
    | App (e1, e2) -> par (conv e1) ^ " " ^ par (conv e2)
  in
  conv exp ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let exp_to_abstract_string (exp : expr) : string =
  let par (s : string) : string =
    "(" ^ s ^ ")" in
  let sep (s : string list) : string =
    List.fold_left (fun a s -> if a <> "" then a ^ ", " ^ s else s) "" s in
  let binop_to_string (b : binop) : string =
    match b with
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Times -> "Times"
    | Equals -> "Equals"
    | LessThan -> "LessThan"
  in
  let rec conv (exp : expr) : string =
    match exp with
      | Var v -> "Var " ^ v
      | Num i -> "Num " ^ string_of_int i
      | Bool b -> "Bool " ^ string_of_bool b
      | Unop (_, e) -> "Unop" ^ par(sep ["Negate"; conv e])
      | Binop (v, e1, e2) -> "Binop" ^ par
                             (sep [binop_to_string v; conv e1; conv e2])
      | Conditional (e1, e2, e3) -> "Conditional" ^ par
                                    (sep [conv e1; conv e2; conv e3])
      | Fun (v, e) -> "Fun" ^ par(sep [v; conv e])
      | Let (v, e1, e2) -> "Let" ^ par(sep [v; conv e1; conv e2])
      | Letrec (v, e1, e2) -> "Letrec" ^ par (sep [v; conv e1; conv e2])
      | Raise -> "Raise "
      | Unassigned -> "Unassigned"
      | App (e1, e2) -> "App" ^ par (sep [conv e1; conv e2])
  in
  conv exp ;;
