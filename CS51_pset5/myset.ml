(**********************************************************************
 * CS51 Problem Set 5, 2016 - Moogle
 * myset.ml: an interface and simple implementation of a set 
 *           abstract datatype
 **********************************************************************)

open Order

(* Definitions for sets. *)

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set in some
   * unspecified order, using the calling convention of fold_left, that
   * is, if the set s contains s1,...,sn, then
   *      fold f u s
   * returns
   *      (f ... (f (f u s1) s2) ... sn)
  *)
  val fold : ('a -> elt -> 'a) -> 'a -> set -> 'a

  (* functions to convert the types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs the tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end

(* Parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> ordering
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end

(* An example implementation of the COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end

(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Equal -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Equal -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Equal -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Equal -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)
      
  let fold = List.fold_left

  let string_of_elt = C.string_of_t
      
  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for the ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end



(***********************************************************************)
(*       Section 2: Sets as dictionaries                     *)
(***********************************************************************)

(* TODO: Uncomment the skeleton code for the DictSet module below and
   complete the implementation, making sure that it conforms to the
   appropriate signature. 

   Add appropriate tests for the functor and make sure that your 
   implementation passes the tests. Once you have the DictSet functor 
   working, you can use it instead of the ListSet implementation by 
   updating the definition of the Make functor below. *)
  
(******************************************************************)
(* DictSet: a functor that creates a SET by calling the           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(
    struct
      type key = C.t
      type value = unit
      let compare x y = C.compare x y
      let string_of_key x = C.string_of_t x
      let string_of_value () = "()"

      let gen_key  = C.gen 
      let gen_key_gt x = C.gen_gt x 
      let gen_key_lt x  = C.gen_lt x 

      let gen_key_random  = C.gen_random 
      let gen_key_between x y  = C.gen_between x y 
      let gen_value () = ()
      let gen_pair () = (gen_key (), gen_value ())
    end)

  type elt = D.key
  type set = D.dict
  let empty = D.empty

  let is_empty (s:set) : bool = (s = empty)

  let insert (k:elt) (s:set) : set = D.insert s k ()

  let singleton (k:elt): set = D.insert D.empty k ()

  let rec union (x:set) (y:set) : set = 
    match D.choose x with
    | None -> y
    | Some (k, (), xremoved) -> union xremoved (D.insert y k () )


  let member (s:set) (k:elt) : bool = D.member s k

  let remove (k:elt) (s:set) = D.remove s k

  let rec intersect (x:set) (y:set) : set = 
    match D.choose x with
    | None -> empty
    | Some (k, (), xremoved) -> 
        if (member y k) then insert k (intersect xremoved y) 
        else (intersect xremoved y)



  let choose (s:set) : (elt*set) option = 
    match D.choose s with
    | None -> None
    | Some (k, (), sremoved) -> Some (k,sremoved)



  let fold f k s = D.foldl (fun d k _ -> f d k) k s


  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for the DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* add your test functions to run_tests *)

  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts;
    ()

  let test_union () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let elts2 = generate_random_list 100 in
    let s2 = insert_list empty elts2 in
    let uni_f = union s1 s2 in
    List.iter2 
      (fun a b -> assert((member uni_f a) && (member uni_f b))) elts elts2;
    ()

  let test_intersect () = 
    let elt1 = C.gen_random() in
    let elt2 = C.gen_gt elt1 () in
    let elt3 = C.gen_gt elt2 () in
    let s1 = empty in
    let s2 = insert elt1 empty in
    let s2 = insert elt2 s2 in 
    let s3 = insert elt1 empty in
    let s3 = insert elt3 s3 in
    let int12 = intersect s1 s2 in
    let int23 = intersect s2 s3 in
    assert (not (member int12 elt1 || member int12 elt2 || member int12 elt3));
    assert (member int23 elt1);
    assert (not (member int23 elt2 || member int23 elt3));
    () 

  let test_member () =
    let elt1 = C.gen_random () in
    let elt2 = C.gen_gt elt1 () in
    let elt3 = C.gen_gt elt2 () in
    let s1 = insert elt1 empty in
    let s1 = insert elt2 s1 in
    assert (member s1 elt1 && member s1 elt2);
    assert (not (member s1 elt3));
    ()

  let test_choose () = 
    let elt1 = C.gen_random () in
    let s0 = empty in
    let s1 = insert elt1 empty in
    assert (choose s0 = None);
    match choose s1 with
    | Some (k, d) ->
      (assert (k = elt1 && d = empty));
    | _ -> raise (Failure "test_choose failed");
    ()

  let test_fold () =
    let elts = generate_random_list 100 in
    let s1 = fold (fun st elt -> insert elt st) 
              empty (insert_list empty elts) in
    List.iter (fun k -> assert (member s1 k)) elts;
    ()

  let test_is_empty () =
    assert (is_empty (empty));
    ()

  let test_singleton () =
    let ran = C.gen () in
    let sgtn = insert ran empty in
    assert (member sgtn ran);
    () 


  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()


end


(******************************************************************)
(* Run the tests.                                                 *)
(******************************************************************)

(* Create a set of ints using the ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;
  
(***********************************************************************)
(*    Section 3: Update set implementation and try your crawler        *)
(***********************************************************************)

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;

(******************************************************************)
(* Make: a functor that creates a SET by calling the              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use the dictionary implementation of sets 
     when you are finished. *)
  (* ListSet (C)  *)
  DictSet (C)
  
