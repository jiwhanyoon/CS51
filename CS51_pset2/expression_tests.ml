open Expression ;;
open Ast ;;
open ExpressionLibrary ;;

let test () =
	assert (contains_var (parse "x^4") = true);
	assert (not (contains_var (parse "2"))= true);
	assert (contains_var (parse "4+3") = false);
	assert (not(contains_var(parse "4+3")) = true);
	assert (contains_var (parse "ln(3)") = false);
	assert (contains_var (parse "sin(3)") = false);
	assert (contains_var (parse "cos(x+4)") = true);

	assert (evaluate (parse "x^4 + 3") 2.0 = 19.0);
	assert (evaluate (parse "x") 1.0 = 1.0) ;
	assert (evaluate (parse "12 + x*2") 3.0 = 18.0);
	assert (evaluate (parse "cos(12)") 3.0 = 0.843853);
	assert (evaluate (parse "cos(x)") 3.0 = -0.9899924);
	assert (evaluate (parse "12 + x*2") 3.0 = 18.0);
	assert (evaluate (parse "x * 2") 3.0 = 6.0);
	
	assert (evaluate (derivative (parse "x")) 3. = 1.);
	assert (evaluate (derivative (parse "x ^ 2")) 3. = 18.);
	assert (evaluate (derivative (parse "ln x")) (-2.) = -0.5);
	assert (evaluate (derivative (parse "x ^ 2")) 2. = 4.);
	assert (evaluate (derivative (parse "sin x")) 2. = -0.416146836547142407);
	assert (evaluate (derivative (parse "cos x")) 2. = -0.909297426825681709);
	assert (evaluate (derivative (parse "3*x")) 2. = 3.0);
	assert (evaluate (derivative (parse "x + 3*x")) 2. = 4.0);
	
	assert (find_zero (parse "x") 1.0 0.1 50 = Some 0.);
	assert ((find_zero (parse "sin x") 3.0 0.05 15) = None);;
	assert ((find_zero (parse " 4+x") 3.0 0.05 15) = None);;
	assert ((find_zero (parse "x") 1.0 0.2 50) = Some 0.);;
	assert ((find_zero (parse " 4+x") 1.0 0.01 10) = None);;

;;

test();;
print_endline "All tests passed.";;
