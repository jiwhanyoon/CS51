open Ps1 ;;

let () = assert ((reversed [1;2;3;4;5;6;7;8]) = false);;
let () = assert ((reversed [8;9;7;6;5;4;3;2;1]) = false);;
let () = assert ((reversed [3;2;1]) = true);;


let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [1;1;1;1] [1;2;3]) = [1;1;1;1;1;2;3]);;
let () = assert ((merge [1;3] [2;4]) = [1;2;3;4]);;
let () = assert ((merge [-1;2;8;100] [1;9;1001]) = [-1;1;2;8;9;100;1001]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [2] []) = [2]);;
let () = assert ((merge [] [-2]) = [-2]);;
let () = assert ((merge [5] [-1]) = [-1;5]);;


let () = assert (unzip [(1,2);(3,4);(5,6)] = ([1;3;5],[2;4;6]));;
let () = assert (unzip [(1,3);(2,4);(5,6)] = ([1;2;5],[3;4;6]));;
let () = assert (unzip [(9,10)] = ([9],[10]));;

let () = assert (variance [1.0;2.0;3.0;4.0;5.0] = Some 2.5);;
let () = assert (variance [1.0;2.0] = Some 0.5);;
let () = assert (variance [1.0] = None);;


let () = assert (few_divisors 19 4 = true);;
let () = assert (few_divisors 5 3 = false);;
let () = assert (few_divisors 5 5 = true);;

let () = assert(concat_list ", " ["Hi"; "Jay"; "Jae"] = "Hi, Jay, Jae");;
let () = assert(concat_list "..." ["I"; "am"; "mad"] = "I...am...mad");;
let () = assert(concat_list ", " [] = "");;
let () = assert(concat_list ", " ["Moo"] = "Moo");;

let () = assert(to_run_length ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] =
                     [(5,'a');(3,'b');(1,'c');4,'d');;
let () = assert(to_run_length ['c';'c';'b'] = [(2,'c');(1,'b')];;
let () = assert(from_run_length [(4,'a');(2,'b');(1,'c');(2,'d')] = 
                  ['a';'a';'a';'a';'b';'b';'c';'d';'d']);;
let () = assert(from_run_length [(2,'c');(2,'d')] = ['c';'c';'d';'d']);;











