
(** Abstract syntax of MiniML expressions *)

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
  | Var x -> SS.singleton x 
  | Num _ -> SS.empty
  | Bool _ -> SS.empty
  | Unop (_, exp1) -> free_vars exp1
      
  | Binop (_, exp1 , exp2) -> SS.union (free_vars exp1) (free_vars exp2)
      


  | Conditional (exp1, exp2, exp3) -> SS.union (free_vars exp1) (SS.union (free_vars exp2) (free_vars exp3))
     

  | Fun (str, exp1) -> SS.remove str (free_vars exp1)
      

  | Let (str, exp1, exp2) -> SS.union (SS.remove str (free_vars exp2)) (free_vars exp1)
      

  | Letrec (str, exp1, exp2) -> SS.union (SS.remove str (free_vars exp1)) (SS.remove str (free_vars exp2))
      
  | Raise  ->  SS.empty
  | Unassigned -> SS.empty

  | App (exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  

   ;;
  
(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)

let init = ref 0;;

let new_varname () : varid =
  let newvar = "newvar" ^ (string_of_int !init) in
  (init := !init + 1 ; newvar) ;;
  
(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =
match exp with
  | Var x -> if var_name = x then repl else Var x
  | Num _ -> exp
  | Bool _ -> exp
  | Unop (str, exp1) -> Unop(str, (subst var_name repl exp1))
      
  | Binop (str, exp1 , exp2) -> 
      Binop(str, (subst var_name repl exp1), (subst var_name repl exp2))


  | Conditional (exp1, exp2, exp3) -> 
      Conditional ((subst var_name repl exp1), (subst var_name repl exp2), 
          (subst var_name repl exp3))


  | Fun (var, exp1) -> 
      if (var = var_name) then exp
    else 
      let freevars = free_vars repl in
      if (SS.mem var freevars) then
      let z = new_varname () in 
        Fun(z, (subst var_name repl (subst var (Var(z)) exp)))
      else
        Fun(var, (subst var_name repl exp1))
      

  | Let (var, exp1, exp2) -> 
      if (var = var_name) then exp
    else 
      let freevars = free_vars repl in
      if (SS.mem var freevars) then
      let z = new_varname () in 
        Let(z, (subst var_name repl exp1) , (subst var_name repl (subst var (Var(z)) exp2)))
      else
        Let(var, (subst var_name repl exp1), (subst var_name repl exp2))


  | Letrec (var, exp1, exp2) -> 
      if (var = var_name) then exp
    else 
      let free = free_vars repl in
      if SS.mem var free then
      let z = new_varname () in 
        Letrec(z, subst var_name repl (subst var (Var(z)) exp1) , subst var_name repl (subst var (Var(z)) exp2))
      else
        Letrec( var, subst var_name repl exp1, subst var_name repl exp2)


  | Raise  ->  Raise
  | Unassigned -> Unassigned

  | App (exp1, exp2) -> App((subst var_name repl exp1), (subst var_name repl exp2))

;;

(** Returns a string representation of the expr *)
let rec exp_to_string (exp: expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num x -> "Num(" ^ (string_of_int x) ^ ")"
  | Bool x -> "Bool(" ^ (string_of_bool x) ^ ")"
  | Unop (str, exp1) -> 
      "Unop(" ^ str ^ " , " ^ (exp_to_string exp1) ^ ")"
  | Binop (str, exp1 , exp2) -> 
      "Binop(" ^ str ^ " , " ^ (exp_to_string exp1) 
        ^ " , " ^ (exp_to_string exp2) ^ ")"

  | Conditional (exp1, exp2, exp3) -> 
      "Conditional(" ^ (exp_to_string exp1) 
        ^ " , " ^ (exp_to_string exp2) ^ " , " ^
          (exp_to_string exp3) ^ ")"

  | Fun (str, exp1) -> 
      "Fun(" ^ str ^ " , " ^ (exp_to_string exp1) ^ ")"

  | Let (str, exp1, exp2) -> 
      "Let(" ^ str ^ " , " ^ (exp_to_string exp1) 
        ^ " , " ^ (exp_to_string exp2) ^ ")"

  | Letrec (str, exp1, exp2) ->
      "Letrec(" ^ str ^ " , " ^ (exp_to_string exp1) 
        ^ " , " ^ (exp_to_string exp2) ^ ")"

  | Raise  ->  "Raise"
  | Unassigned -> "Unassigned"

  | App (exp1, exp2) ->
      "App(" ^ (exp_to_string exp1) ^ " , " ^ (exp_to_string exp2) ^ ")"
;;
  

