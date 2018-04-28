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
  failwith "free_vars not implemented" ;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(*......................................................................
  Substitution

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;

(*......................................................................
  String representations of expressions
 *)

(* add parentheses to a string *)
let parenth (s : string) : string =
  "(" ^ s ^ ")" ;;

(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let exp_to_concrete_string (exp : expr) : string =
  let binop_to_string (b : binop) : string =
    match b with
    | Plus -> " + "
    | Minus -> " - "
    | Times -> " * "
    | Equals -> " = "
    | LessThan -> " < "
  in
  let rec convert (exp : expr) : string =
    match exp with
    | Var v -> v
    | Num i -> string_of_int i
    | Bool b -> string_of_bool b
    | Unop (_, e) -> "~-" ^  parenth (convert e)
    | Binop (b, e1, e2) -> parenth (convert e1) ^ binop_to_string b ^ parenth (convert e2)
    | Conditional (e1, e2, e3) -> "if " ^ convert e1 ^ "then " ^ convert e2 ^
                                  "else" ^ convert e3
    | Fun (v, e) -> "fun " ^ v ^ " -> " ^ convert e
    | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ convert e1 ^ " in " ^ convert e2
    | Letrec (v, e1, e2) -> "let rec " ^ v ^ " = " ^ convert e1 ^ " in " ^
                            convert e2
    | Raise -> "Raise"
    | Unassigned -> "Unassigned"
    | App (e1, e2) -> parenth (convert e1) ^ parenth (convert e2)
  in
  convert exp ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let exp_to_abstract_string (exp : expr) : string =
  let binop_to_string (b : binop) : string =
    match b with
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Times -> "Times"
    | Equals -> "Equals"
    | LessThan -> "LessThan"
  in
  let sep (s : string list) : string =
    List.fold_left (fun a b -> if a <> "" then a ^ ", " ^ b else b) "" s in
  let rec convert (exp : expr) : string =
    match exp with
      | Var v -> "Var " ^ v
      | Num i -> "Num " ^ string_of_int i
      | Bool b -> "Bool " ^ string_of_bool b
      | Unop (_, e) -> "Unop" ^ parenth(sep ["Negate"; convert e])
      | Binop (v, e1, e2) -> "Binop" ^ parenth(sep [binop_to_string v; convert e1;
                             convert e2])
      | Conditional (e1, e2, e3) -> "Conditional" ^ parenth (sep [convert e1; convert e2; convert e3])
      | Fun (v, e) -> "Fun" ^ parenth(sep [v; convert e])
      | Let (v, e1, e2) -> "Let" ^ parenth(sep [v; convert e1; convert e2])
      | Letrec (v, e1, e2) -> "Letrec" ^ parenth (sep [v; convert e1; convert e2])
      | Raise -> "Raise "
      | Unassigned -> "Unassigned"
      | App (e1, e2) -> "App" ^ parenth (sep [convert e1; convert e2])
  in
  convert exp ;;
