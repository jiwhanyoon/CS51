open Mapfold ;;
  
let test () =

assert ((negate_all []) = []);
assert ((negate_all [1; -2; 5]) = [-1; 2; -5]);
assert ((negate_all [-2;-3;0;1;2;3]) = [2;3;0;-1;-2;-3]) ;

assert ((sum [1;2;3;4;5]) = 15);
assert ((sum []) = 0);
assert ((sum [5;6;7]) = 18);
assert ((sum [-1;-5;3]) =  -3);

assert ((sum_rows [[3;0];[3;4]]) = [3;7]);
assert ((sum_rows [ [0;0] ; [1;2]] = [0;3]));
assert ((sum_rows [[-5;4] ; [5;9]] = [-1;14]));
assert ((sum_rows [[4;3;2;1];[10;25]]) = [10;35]);

assert ((filter_odd [1;2;3;4;5;6]) = [1;3;5]);
assert ((filter_odd [0;-7;5;-3]) = [-7; 5; -3]);
assert ((filter_odd [1;6;-5;-3]) = [1;-5;-3]);

assert ((num_occurs 4 [1;3;4;5;4]) = 2);
assert ((num_occurs 0 [1;3;4;5;4]) = 0);
assert ((num_occurs 4 [4;4;4;4;4]) = 5);
assert ((num_occurs (-2) [0;1;3;-2;-2;5,-2]) = 2);

assert ((super_sum [[1;2;3];[];[5]]) = 11);
assert ((super_sum [[1;2;3];[0];[6;7]; [6]]) = 25);
assert ((super_sum [[3;2;1];[];[6]]) = 12);
assert ((super_sum [[3;6];[4];[6;3;2];[3;3;3];[1;4]]) = 38);


assert((filter_range [1;3;4;5;2] (1,3) = [1;3;2]));
assert((filter_range [1;3;4;5;2] (2,3) = [3;2]));
assert((filter_range [1;3;4;5;2] (3,3) = [3]));
assert((filter_range [] (1,3) = []));
assert((filter_range [0;0;0;0] (1,3) = []));
assert((filter_range [1;10;15;13;8] (13,15) = [15;13]));

assert ((floats_of_ints [1;2;3;4;5;6]) = [1.;2.;3.;4.;5.;6.]);
assert ((floats_of_ints [2;1;2;1;8;5]) = [2.;1.;2.;1.;8.;5.]);
assert ((floats_of_ints [2;6;0]) = [2.;7.;0.]);
assert ((floats_of_ints [2;-6;5]) = [2.;-6.;0.]);

assert (log10s [1.0; 10.0; -10.0] = [Some 0.; Some 1.; None]);
assert (log10s [1.0; 10.0; 0.] = [Some 0.; Some 1.; None]);
assert (log10s [-2.; -5.] = [None;None]);

assert (deoptionalize [Some 3; None; Some 5; Some 7] = [3;5;7]);
assert (deoptionalize [Some "hello"; None; Some 5.0; Some "c"] = ["hello";5.0;"c"]);
assert (deoptionalize [None; None] = []);
assert (deoptionalize [Some "World"; None; Some "Bye"] = ["World";"Bye"]);

assert (some_sum [Some 2; Some 8; None; Some 10] = 20);
assert (some_sum [Some 1; None; None; None; Some 2] = 3);
assert (some_sum [Some 2; None] = 2);
assert (some_sum [None] = 0);
assert (some_sum [Some (-1); None; None; None; Some (-2)] = 3);

assert (mult_odds [1;3;0;2;-5] = -15);
assert (mult_odds [6;2;1;2;5] = 5);
assert (mult_odds [6;2;4] = 0);
assert (mult_odds [0;0] = 0);
assert (mult_odds [] = 0);

assert (concat [[1;2;3];[1;2;3]] = [1; 2; 3; 1; 2; 3]);
assert (concat [["bye"];["hello"]] = ["bye"; "hello"]);
assert (concat [[0;0;0];[0;0]] = [0;0;0;0;0]);
assert (concat [[5;2;1];[2]] = [5;2;1;2]);

let students = [("K",2010);("J",2010);("M",2013)];
assert (filter_by_year students 2010 = ["K";"J"]);
assert (filter_by_year students 2013 = ["M"]);
assert (filter_by_year students 2000 = []);
;;

test();;
print_endline "All tests passed.";;
