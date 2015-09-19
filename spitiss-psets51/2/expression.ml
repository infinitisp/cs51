
open Core.Std ;;
open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionLibrary.ml
 * 3. Remember to test using the function "assert".
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
    match e with
    |Num _ -> false
    |Var -> true
    |Binop (_,x,y) -> contains_var x || contains_var y
    |Unop (_,x) -> contains_var x

assert (contains_var (parse "x^4"))
assert (contains_var (parse "4+3") = false)



(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Don't
 *            worry about handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  |Num y -> y
  |Var -> x
  |Binop (b,y,z) -> (match b with
		    |Add -> evaluate y x +. evaluate z x
		    |Sub -> evaluate y x -. evaluate z x
		    |Mul -> evaluate y x *. evaluate z x
		    |Div -> evaluate y x /. evaluate z x
		    |Pow -> evaluate y x ** evaluate z x)
  |Unop (b,y) -> (match b with
		 |Sin -> sin (evaluate y x)
		 |Cos -> cos (evaluate y x)
		 |Ln -> log (evaluate y x)
		 |Neg -> -1. *. (evaluate y x))

assert ( evaluate (parse "x^4 + 3") 2.0 = 19.0)
 

(*>* Problem 2.3 *>*)

(* See writeup for instructions. We have pictures! *)
let rec derivative (e:expression) : expression =
    match e with
    | Num _ -> Num 0.
    | Var -> Num 1.
    | Unop (u,e1) ->
        (match u with
        | Sin -> Binop(Mul,Unop(Cos,e1),derivative e1)
        | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
        | Ln -> Binop(Div,derivative e1, e1)
        | Neg -> Unop(Neg,derivative e1))
    | Binop (b,e1,e2) ->
        match b with
        | Add -> Binop(Add,derivative e1,derivative e2)
        | Sub -> Binop(Sub,derivative e1,derivative e2)
        | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                        Binop(Mul,derivative e1,e2))
        | Div -> Binop(Div, Binop(Sub,
			    Binop(Mul,derivative e1, e2),
			    Binop(Mul,e1,derivative e2)),
		      Binop(Pow,e2,Num 2.))
        | Pow -> Binop(Mul,e,
		       Binop(Add, Binop(Mul,derivative e2, Unop(Ln,e1)),
			     Binop(Div,Binop(Mul, derivative e1, e2),e1)))
	   
(*I really don't want to write out assert statements for each possible path ??
 *that's just not conducive to learning (in fact, this problem was enough data
 * entry that involved no creativity; sorry -- I did use your 
 *checkexp function to check several cases though. Thanks. *)

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
    print_string ("Checking expression: " ^ strs ^ "\n");
    let parsed = parse strs in (
        print_string "contains variable : ";
        print_string (string_of_bool (contains_var parsed));
        print_endline " ";
        print_string "Result of evaluation: ";
        print_float (evaluate parsed xval);
        print_endline " ";
        print_string "Result of derivative: ";
        print_endline " ";
        print_string (to_string_smart (derivative parsed));
        print_endline " "
    )
;;


(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  if lim = 0 then None else
    if Float.abs (evaluate e g) < epsilon then Some g else
      find_zero 
	e (g -. (evaluate e g) /. (evaluate (derivative e) g)) epsilon (lim - 1)
;;

find_zero (parse ("x^2")) 0.1 0.0025 5;;
find_zero (parse ("x^2")) 0.5 0.0001 5;;
find_zero (parse ("x^2")) 0.5 0.001 5;;
find_zero (parse ("x^3")) 5. 0.001 100;;

(*>* Problem 2.5 *>*)

(* Challenge problem:
 * Just leave it unimplemented if you don't want to do it.
 * See writeup for instructions. *)

(* simplifies any Mul or Div ops *)

let rec find_zero_exact (e:expression) : expression option =
    failwith "Not implemented"
;;


(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 : int = 120;;
