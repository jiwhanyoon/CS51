(**********************************************************************
        CS51 Problem Set 6
           Spring 2016
           Refs, Streams, and Music

           Part 1: Refs
 **********************************************************************)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref

(* Write a function has_cycle that returns whether a mutable list has
   a cycle.  You may want a recursive helper function. Don't worry
   about space usage. *)

let has_cycle (lst : 'a mlist) : bool =
  (* checker helper rec function *)
  let rec checker lst1 vl =
    match lst1 with
    | Nil -> false
    | Cons (_ , removed) ->
        if List.memq lst1 vl then true
      else
        checker !removed (lst1::vl)
      in
      checker lst []
  

(* Write a function flatten that flattens a list (removes its cycles
   if it has any) destructively. Again, you may want a recursive
   helper function and you shouldn't worry about space. *)


let flatten (lst : 'a mlist) : unit =
  let rec remover lst1 vl =
    match lst1 with
    | Nil -> ()
    | Cons (_, removed) -> 
      if (has_cycle lst1)
        then removed := Nil
      else
    remover !removed (lst1::vl)
  in
    remover lst []
;;

  

(* Write mlength, which nondestructively finds the number of nodes in
   a mutable list that may have cycles. *)
let mlength (lst : 'a mlist) : int =
  let rec count lst1 vl i = 
    match lst1 with 
    | Nil -> i
    | Cons (_ , t) ->
      if (List.memq lst1 vl) then (i + 1)
    else
      count !t (lst1::vl) (i+1)
    in
      count lst [] 0
  

(* Please give us an honest estimate of how long this part took you to
   complete.  We care about your responses and will use them to help
   guide us in creating future assignments. *)
let minutes_spent : int = 180
