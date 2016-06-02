(*** CS 51 Problem Set 1 ***)

(* Problem 1 - Fill in types:
 *
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

(* For each of the expressions below, enter a string describing the 
 * type of the expression in the expressions below. The first one is done 
 * for you.
 *)

(* 
let prob0 : ??? = 42 ;;

let prob1a : ??? = let greet y = "Hello " ^ y in greet "World!";;

let prob1b : ??? = [Some 4; Some 2; None; Some 3];;

let prob1c : ??? = ((None, Some 42.0), true);;
 *)

let prob0 =  "int";;

let prob1a = "string";;

let prob1b = "int option list";;

let prob1c = "(('a option * float option) * bool)";;


(* There are several values defined below that do not type check.

  Explain in a comment above each corresponding value
  why the following definitions will
  not type check, and then provide a fixed version of each function
  as an OCaml value (i.e. outside of a comment). *) 

(* this is wrong because  string * int should be contained within parenthesis to
 let the program know this is a tuple that - as a whole - is getting 
 inserted into a list *)


let prob1d : (string * int) list = [("CS", 51); ("CS", 50)];;


  
(* this is wrong because you are comparing the values of different types, an 
int and a float. It must be specified within the given *)

let prob1e : int =
  let compare (x,y) = x < y in
  if compare (4.0, 3.9) then 4 else 2;;


(* this is wrong because second value of the tuples are not strings - they are
 either Option of type none or an int - that must be specified *) 
let prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); ("April", None);
   ("May", None); ("June", Some 1); ("July", None); ("August", None);
   ("September", Some 3); ("October", Some 1); ("November", Some 2); ("December", Some 3)] ;;




(* Problem 2 - Write the following functions *)

(* For each subproblem, you must implement a given function, providing
 * appropriate unit tests in the accompanying file pset1_tests.ml. You
 * are provided a high level description as well as a prototype (type signature)
 * of the function you must implement. 
 * Keep in mind the CS51 style guide 
 * and what you've learned so far about efficiency and elegance. *)

(* `reversed lst` should return true if the integers in lst are in
 * decreasing order. The empty list is considered to be reversed. Consecutive
 * elements can be equal in a reversed list. *)

(* Here is its prototype/signature: *)
(* reversed : int list -> bool *)

(* Implement reversed below, and be sure to write tests for it (see 2b for
 * examples of tests). *)

(* Replace the line below with your own definition of `reversed` *)
let rec reversed (lst: int list) : bool = 
	match lst with
	| [] -> true
	| [a] -> true
	| hd :: md :: tl -> if hd > md then 
		reversed (md::tl) else false;;


(* merge takes two integer lists, each sorted in increasing order,
 and returns a single merged list in sorted order. For example:

merge [1;3;5] [2;4;6];;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;3;5] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

*)

(* The type signature for merge is as follows: *)
(* merge : int list -> int list -> int list *)

(* Replace the line below with your own definition of `merge` *)


let rec merge (x:int list) (y:int list) =
   match x, y with
   |[],_ -> x
   |_,[] -> y
   |x::xtl, y::ytl -> if x < y then 
   x::y::(merge xtl ytl) 
else y::x::(merge xtl ytl);; 


(* unzip should be a function which, given a list of pairs, returns a
 * pair of lists, the first of which contains each first element of
 * each pair, and the second of which contains each second element.
 * The returned lists should have the elements in the order in which
 * they appeared in the input. So, for instance:

unzip [(1,2);(3,4);(5,6)];;
- : int list * int list = ([1;3;5],[2;4;6])

*)


(* The type signature for unzip is as follows: *)
(* unzip : (int * int) list -> int list * int list) *)

(* Replace the line below with your own definition of `unzip` *)
let rec unzip (lst: (int * int) list) : int list * int list = 
	match lst with
	| [] -> ([],[])
	| (hd_x, hd_y) :: tl -> 
		match unzip tl with
		| (x,y) -> (hd_x :: x , hd_y :: y);;


(*>* Problem 2d *>*)

(* `variance lst` returns None if lst has fewer than 2 floats, and
 * Some of the variance of the floats in lst otherwise.  Recall that
 * the variance of a sequence of numbers is 1/(n-1) * sum (x_i-m)^2,
 * where a^2 means a squared, and m is the arithmetic mean of the list
 * (sum of list / length of list). For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0];;
- : int option = Some 2.5
variance [1.0];;
- : int option = None

 * Remember to use the floating point version of the arithmetic
 * operators when operating on floats (+. *., etc). The "float"
 * function can cast an int to a float. *)

(* variance : float list -> float option *)

(* Replace the line below with your own definition of `variance` *)
let rec count (x: float list) : float =
		match x with 
		| [] -> 0.
		| hd :: tl -> count tl +. 1. ;;

let rec sum (y : float list) : float =
			match y with
			| [] -> 0.
			| hd::tl -> sum tl +. 1. ;; 

let rec hi (lst: float list) (x: float) : float =
	match lst with
	| [] -> 0.
	| hd::tl -> ((hd -. x) ** 2.) +. hi tl x;;

let variance (lst: float list) : float option = 
	let mean = sum lst /. count lst in
	if count lst < 2.0 then None else
		let lstsum = hi lst mean in
		Some (lstsum /. (count lst -. 1.));;

(*>* Problem 2e *>*)

(* few_divisors n m should return true if n has fewer than m divisors,
 * (including 1 and n) and false otherwise. Note that this is *not* the
 * same as n having fewer divisors than m:

few_divisors 17 3;;
- : bool = true
few_divisors 4 3;;
- : bool = false
few_divisors 4 4;;
- : bool = true

 * Do not worry about negative integers at all. We will not test
 * your code using negative values for n and m, and do not
 * consider negative integers for divisors (e.g. don't worry about
 * -2 being a divisor of 4) *)

(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)

(* Replace the line below with your own definition of `few_divisors` *)
let rec divisors (x:int) (y:int) : int =
	match (x,y) with
	| (_ ,0) -> 0
	| (x,y) -> if x mod y = 0 then 1 + divisors x (y-1)
	else
	 divisors x (y-1);; 

let few_divisors (num: int) (bound:int) : bool = 
	divisors num num < bound ;;

(*>* Problem 2f *>*)

(* `concat_list sep lst` returns one big string with all the string
 * elements of lst concatenated together, but separated by the string
 * sep. Here are some example tests:

concat_list ", " ["Greg"; "Anna"; "David"];;
- : string = "Greg, Anna, David"
concat_list "..." ["Moo"; "Baaa"; "Quack"];;
- : string = "Moo...Baaa...Quack"
concat_list ", " [];;
- : string = ""
concat_list ", " ["Moo"];;
- : string = "Moo"

*)

(* The type signature for concat_list is: *)
(* concat_list : string -> string list -> string *)

(* Replace the line below with your own definition of `concat_list` *)
let rec concat_list (x:string) (y: string list) : string =
	match y with
	| [] -> ""
	| hd :: [] -> hd
	| hd::tl -> hd^x^concat_list x tl;; 

(*>* Problem 2g *>*)

(* One way to compress a list of characters is to use run-length encoding.
 * The basic idea is that whenever we have repeated characters in a list
 * such as ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] we can
 * (sometimes) represent the same information more compactly as a list
 * of pairs like [(5,'a');(3,'b');(1,'c');(4,'d')].  Here, the numbers
 * represent how many times the character is repeated.  For example,
 * the first character in the string is 'a' and it is repeated 5 times,
 * followed by 3 occurrences of the character 'b', followed by one 'c',
 * and finally 4 copies of 'd'.
 *
 * Write a function to_run_length that converts a list of characters into
 * the run-length encoding, and then write a function from_run_length
 * that converts back. Writing both functions will make it easier to
 * test that you've gotten them right. *)

(* The type signatures for to_run_length and from_run_length are: *)
(* to_run_length : char list -> (int * char) list *)
(* from_run_length : (int * char) list -> char list *)

(* Replace the line below with your own definition of `to_run_length` *)
let to_run_length (lst : char list) : (int*char) list =
	match lst with
	| [] -> [] 
	| (s::tl) -> 
  		let rec compress (lst:char list) (n: int) (s:char) : (int*char) list =
    		match lst with
    		| [] -> [(n,s)]
    		| (hd::tl) ->
    			if (hd=s) then compress tl (n+1) s
    		else 
    		(n,s) :: compress tl 1 hd 
    	in 
    	compress lst 0 s;;


(* Replace the line below with your own definition of `from_run_length` *)
let from_run_length (lst: (int*char) list) : char list = 
  let rec uncompress  (lst: (int*char) list) : char list = 
    match lst with
      |[] ->[]
      |(0,c)::tl -> uncompress tl
      |(n,c)::tl ->  
			if (n < 0) then uncompress tl
			else c::(uncompress ((n-1,c)::tl) )
 		 in
  		uncompress lst;;

(*>* Problem 3 *>*)

(* Challenge!

 * permutations lst should return a list containing every
 * permutation of lst. For example, one correct answer to
 * permutations [1; 2; 3] is
 * [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]].

 * It doesn't matter what order the permutations appear in the returned list.
 * Note that if the input list is of length n then the answer should be of
 * length n!.

 * Hint:
 * One way to do this is to write an auxiliary function,
 * interleave : int -> int list -> int list list,
 * that yields all interleavings of its first argument into its second:
 * interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ].
 * You may also find occasion for the library functions
 * List.map and List.concat. *)

(* The type signature for permuations is: *)
(* permutations : int list -> int list list *)

(* Replace the line below with your own definition of `permutations` *)
let permutations = failwith "implement";;
