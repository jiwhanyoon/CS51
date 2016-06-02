
open Ps3 ;;
  
(* Sample negate tests: more exhaustive testing for all other functions
 * is required. (An example of such testing is test_equals below).
 * We only allow the positive representation of 0 *)


(* extra lines between tests for easier reading *)


(*test negate *)
let _ = assert (negate {neg = false; coeffs = []}
                    = {neg = false; coeffs = []})
let _ = assert (negate {neg = true; coeffs = [1; 2]}
                    = {neg = false; coeffs = [1; 2]})
let _ = assert (negate {neg = true; coeffs = })
let _ = assert (negate {neg = false; coeffs = [1]} 
                    = {neg = true; coeffs = [1]});;
let _ = assert (negate {neg = false; coeffs = [3;5]} 
                    = {neg = true; coeffs = [3;5]});;




(*test equal *)
let rec test_equal (min : int) (max : int) : unit =
  if min > max then ()
  else
    let _ = assert(equal (fromInt min) (fromInt max) = (min = max)) in
    test_equal (min + 1) max 
  ;;

let () = test_equal (max_int) min_int;;





(*test less *)
let rec test_less (min: int) (max: int) : unit =
  if min > max then ()
  else
    let _ = assert ( less (fromInt min) (fromInt max) = (min < max)) in
    let _ = assert ( less (fromInt max) (fromInt min) = (max < min) in
      test_less (min +1) (max - 1) 
    ;;

let () = test_less ~-99999999 9999999;;
let () = test_less 99999999 ~-9999999;;
let () = test_less ~-2000 200000;;
let () = test_less 0 99999;;





(*test greater *)
let rec test_greater (min:int) (max:int) : unit = 
  if min > max then ()
  else
    let _ = assert (greater (fromInt min) (fromInt max) = (min > max)) in
      test_greater (min + 1) max 
    ;;

let () = test_greater ~-99999999 9999999;;
let () = test_greater 99999999 ~-9999999;;
let () = test_greater ~-2000 200000;;
let () = test_greater 0 99999;;




(*test fromInt *)
let _ = assert(fromInt 0 = {neg=false; coeffs=[]});;
let _ = assert(fromInt ~-0 = {neg=false; coeffs=[]});;
let _ = assert(fromInt 1 = {neg=false; coeffs=[1]});;
let _ = assert(fromInt 1000 = {neg=false; coeffs=[1;0]});;
let _ = assert(fromInt ~-1000 = {neg=true; coeffs=[1;0]});;
let _ = assert(fromInt 51 = {neg=false; coeffs=[51]});;
let _ = assert(fromInt ~-51 = {neg=true; coeffs=[51]});;
let _ = assert(fromInt 111222 = {neg=false; coeffs=[111;222]});;
let _ = assert(fromInt ~-111222 = {neg=true; coeffs=[111;222]});;




(*test toInt *)
let rec test_toInt (min : int) (max : int) : unit =
  if min > max then ()
  else if min = 0
       then assert(toInt (fromInt min) = None)
       else 
         let _ = assert(toInt (fromInt min) = Some min) in
  test_toInt (min + 1) max 
;;

let () = test_toInt ~-99999999 9999999;;
let () = test_toInt ~-2000 200000;;
let () = test_toInt 0 99999;;




(*test plus *)
let rec test_plus (min : int) (max : int) : unit =
  if min > max then ()
  else
    let y = 
      match toInt (plus (fromInt min) (fromInt max)) with
      | None -> 0
      | Some x -> x 
    in
    let _ = assert( y = min + max ) 
  in
    test_plus (min + 1) (max - 1)
;;

let () = test_plus ~-99999999 9999999;;
let () = test_plus ~-2000 200000;;
let () = test_plus 0 99999;;




(*test times - similar to test_plus *)
let rec test_times (min : int) (max : int) : unit =
  if min > max then ()
  else
    let y = 
      match toInt (times (fromInt min) (fromInt max)) with
      | None -> 0
      | Some x -> x 
    in
    let _ = assert( y = min * max ) 
  in
    test_times (min + 1) (max - 1)
;;     

let () = test_times ~-99999999 9999999;;
let () = test_times ~-2000 200000;;
let () = test_times 0 99999;;

