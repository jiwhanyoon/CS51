(* CS51 Spring 2016
 * PS 0 *)

(* 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by hitting Ctrl+c and then Ctrl+e in Emacs, or by
 * typing "ocamlbuild ps0.byte" in the terminal. *)

(* 1.a. Replace FIRST and LAST with your first and last name *)
let name : (string * string) = ("Kevin", "Yoon");;


(* 1.b. Replace "Other ..." in class_year with your current year that is of
 * type 'year' *)
type year = Freshman | Sophomore | Junior | Senior | Other of string;;

let class_year : year = Freshman;;

(* 1.c Replace "Other ..." in took_cs_50 with whether or not you took CS50,
 * with a response of *)

type cs50 = Took | DidNotTake | Other of string;;

let took_cs_50 : cs50 = Took;;

(* 1.d. Replace the .... with what you're excited about in this course *)
let exciting : string = "I'm excited about becoming a better programmer overall!!";;

(* ***
   2. You shouldn't change anything below this line, but you should
   read to the bottom of the file and try to figure out what is going on.
  **** *)

let print = Printf.printf;;

let print_survey () =
  let (first, last) = name in
  let string_year =
    (match class_year with
       | Freshman -> "2019"
       | Sophomore -> "2018"
       | Junior -> "2017"
       | Senior -> "2016"
       | Other s -> "Other: " ^ s
    ) in 
  let string_cs50 =
    (match took_cs_50 with
      | Took -> "I took CS50"
      | DidNotTake -> "I did not take CS50"
      | Other s -> "Other: " ^ s) in
    (print "----------------------------------------\n";
     print "Name: %s %s\n\n" first last;
     print "Year: %s\n\n" string_year;
     print "50?: %s \n\n" string_cs50;
     print "%s\n\n" exciting;
     print "----------------------------------------\n\n";);;

print_survey ();;

(* type "ocamlbuild ps0.byte" to compile the file.
  Then type ./ps0.byte to run the program and print the output.
  Make sure all the values look right.  If they do, submit and
  you're done! *)
