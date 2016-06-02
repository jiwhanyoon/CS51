open Expression ;;
open Ast ;;
open ExpressionLibrary ;;

let test () =
  assert (contains_var (parse "x+3"));
  assert (not (contains_var (parse "2")));

  (*  Additional tests go here... *)

;;

test();;
print_endline "All tests passed.";;
