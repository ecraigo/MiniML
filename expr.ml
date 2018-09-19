
(** Abstract syntax of MiniML expressions *)

let fmt = Printf.sprintf ;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  | Ref of expr ref                      (* extension: references *)
 and varid = string ;;

(** Sets of varids *)
module SS = Set.Make(struct
			type t = varid
			let compare = String.compare
		      end);;

type varidset = SS.t ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;

(** Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v
  | Num _ | Bool _ | Raise | Unassigned -> SS.empty
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) | App (e1, e2) ->
		         SS.union (free_vars e1) (free_vars e2)
  | Conditional (c, t, f) -> SS.union (free_vars t) (free_vars f)
			     |> SS.union (free_vars c)
  | Fun (v, e) -> SS.diff (free_vars e) (SS.singleton v)
  | Let (v, to_sub, e) ->
     SS.union (free_vars to_sub)
	      (SS.diff (free_vars e) (SS.singleton v))
  | Letrec (v, to_sub, e) ->
     SS.diff (SS.union (free_vars to_sub) (free_vars e))
	     (SS.singleton v)
  | Ref _ -> raise (Invalid_argument "No refs in substitution model")
;;

(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname : unit -> varid =
  let ctr = ref 0 in
  fun () -> (let cur = !ctr in
	     ctr := cur+1;
	     "var" ^ string_of_int cur)
;;

(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =

  match exp with
  | Var v -> if var_name = v then repl else exp
  | Num _ | Bool _ -> exp
  | Unop (op, e) -> Unop (op, subst var_name repl e)
  | Binop (op, e1, e2) -> Binop (op, subst var_name repl e1,
				 subst var_name repl e2)
  | Conditional (c, t, f) -> Conditional (subst var_name repl c,
					  subst var_name repl t,
					  subst var_name repl f)

  (* Avoiding variable capture makes for some pretty ugly code *)
  | Fun (v, e) ->
     if var_name = v then exp
     else
       (if SS.mem v (free_vars repl) then
	       (let fresh = new_varname () in
	        let renamed = subst v (Var fresh) e in
	        Fun (fresh, subst var_name repl renamed))
	      else Fun (v, subst var_name repl e))

  | Let (v, to_sub, e) ->
     if var_name = v
     then Let (v, subst var_name repl to_sub, e)
     else
       (if SS.mem v (free_vars repl)
	then
	  (let fresh = new_varname () in
	   let renamed = subst v (Var fresh) e in
	   Let (fresh, subst var_name repl to_sub,
		subst var_name repl renamed))
	else Let (v, subst var_name repl to_sub,
		  subst var_name repl e))

  | Letrec (v, to_sub, e) ->
     if var_name = v then exp
     else
       (if SS.mem v (free_vars repl)
	then
	  (let fresh = new_varname () in
	   let renamed = subst v (Var fresh) e in
	   Letrec (fresh, subst var_name repl to_sub,
		   subst var_name repl renamed))
	else Letrec (v, subst var_name repl to_sub,
		     subst var_name repl e))


  | Raise | Unassigned -> exp

  (* This can and will result in some nasty cases of non-functions
   * being applied. However, those cases are malformed expressions
   * and not the fault of poor substitution, and so we will deal
   * with them in evaluation. *)
  | App (f, e) -> App (subst var_name repl f, subst var_name repl e)
  | Ref _ -> raise (Invalid_argument "No refs in substitution model")
;;

(** Returns a string representation of the expr *)
let rec exp_to_abstract (exp: expr) : string =
  let aux ident str_list =
    fmt "%s(%s)" ident (String.concat ", " str_list) in
  match exp with
  | Var v -> aux "Var" [v]
  | Num n -> aux "Num" [string_of_int n]
  | Bool b -> aux "Bool" [string_of_bool b]
  | Unop (op, e) -> aux "Unop" [op; exp_to_abstract e]
  | Binop (op, e1, e2) -> aux "Binop"
			      [op; exp_to_abstract e1; exp_to_abstract e2]
  | Conditional (c, t, f) ->
     aux "Conditional" (List.map exp_to_abstract [c; t; f])
  | Fun (v, e) -> aux "Fun" [v; exp_to_abstract e]
  | Let (v, to_sub, e) ->
     aux "Let" [v; exp_to_abstract to_sub; exp_to_abstract e]
  | Letrec (v, to_sub, e) ->
     aux "Letrec" [v; exp_to_abstract to_sub; exp_to_abstract e]
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (f, e) -> aux "App" (List.map exp_to_abstract [f; e])
  | Ref r -> aux "Ref" [exp_to_abstract !r]

let rec exp_to_concrete (exp: expr) : string =
  match exp with
  | Var v -> v
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unop (op, e) -> fmt "%s (%s)" op (exp_to_concrete e)
  | Binop (op, e1, e2) ->
     fmt "(%s) %s (%s)" (exp_to_concrete e1) op (exp_to_concrete e2)
  | Conditional (c, t, f) ->
     fmt "if %s then %s else %s" (exp_to_concrete c) (exp_to_concrete t)
	 (exp_to_concrete f)
  | Fun (v, e) -> fmt "fun %s -> %s" v (exp_to_concrete e)
  | Let (v, to_sub, e) ->
     fmt "let %s = %s in %s" v (exp_to_concrete to_sub)
	 (exp_to_concrete e)
  | Letrec (v, to_sub, e) ->
     fmt "let rec %s = %s in %s" v (exp_to_concrete to_sub)
	 (exp_to_concrete e)
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (f, e) -> fmt "(%s) %s" (exp_to_concrete f) (exp_to_concrete e)
  | Ref r -> fmt "ref (%s)" (exp_to_concrete !r)

let exp_to_string = exp_to_concrete
