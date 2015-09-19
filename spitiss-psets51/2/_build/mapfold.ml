
open Core.Std

(****************************************************)
(******       1.1: Sparking your INTerest      ******)
(****************************************************)

(* Solve each problem in this part using List.map, List.fold_right, or
 * List.filter.
 *
 * See the Ocaml Core Library documentation on lists:
 * https://ocaml.janestreet.com/ocaml-core/109.60.00/doc/core/#Std.List
 *
 * You MAY NOT change the definition of these
 * functions to make them recursive. *)

(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
    List.map ~f:(fun x -> -x) nums

(* Unit test example. *)
assert ((negate_all [1; -2; 0]) = [-1; 2; 0]) ;;


(*>* Problem 1.1.b *>*)

(*  sum : Returns the sum of the elements in the list. *)
let sum (nums:int list) : int =
    List.fold_right ~f:(fun x y -> x + y) ~init:0 nums

assert(sum [1;2;3] = 6)
assert(sum [] = 0)

(*>* Problem 1.1.c *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)
let sum_rows (rows:int list list) : int list =
    List.map ~f:sum rows

assert (sum_rows [[1;2]; [3;4]] = [3; 7])
assert (sum_rows [] = [])
assert (sum_rows [[0]] = [0])

(*>* Problem 1.1.d *>*)

(*  filter_odd : Retains only the odd numbers from the given list.
 *     Example : filter_odd [1;4;5;-3] = [1;5;-3]. *)
let filter_odd (nums:int list) : int list =
    List.filter ~f:(fun x -> if x mod 2 = 0 then false else true) nums

assert (filter_odd [0;1;2;3;4] = [1;3])
assert (filter_odd [] = [])


(*>* Problem 1.1.e *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)
let num_occurs (n:int) (nums:int list) : int =
    List.fold_right ~init: 0 ~f:(fun x y ->
				 if x = n then 1 + y
				 else y) nums

assert (num_occurs 4 [1;3;4;5;4] = 2)
assert (num_occurs 4 [0;2;3;5] = 0)


(*>* Problem 1.1.f *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)
let super_sum (nlists:int list list) : int =
    List.fold_right ~init:0 ~f:(fun x y -> sum x + y) nlists

assert (super_sum [[1;2;3];[];[5]] = 11)

(*>* Problem 1.1.g *>*)

(*  filter_range : Returns a list of numbers in the input list within a
 *                 given range (inclusive), in the same order they appeared
 *                 in the input list.
 *       Example : filter_range [1;3;4;5;2] (1,3) = [1;3;2] *)
let filter_range (nums:int list) (range:int * int) : int list =
  let (min,max) = range in
  List.filter ~f:(fun x -> if (x >= min) && (x <= max) then
			     true else false) nums

assert (filter_range [1;3;4;5;2] (1,3) = [1;3;2])



(****************************************************)
(**********       1.2 Fun with Types       **********)
(****************************************************)


(*>* Problem 1.2.a *>*)

(*  floats_of_ints : Converts an int list into a list of floats *)
let floats_of_ints (nums:int list) : float list =
    List.map ~f:(Float.of_int) nums

assert (floats_of_ints [1;2] = [1.;2.]) 


(*>* Problem 1.2.b *>*)

(*   log10s : Applies the log10 function to all members of a list of floats.
 *            The mathematical function log10 is not defined for
 *            numbers n <= 0, so undefined results should be None.
 *  Example : log10s [1.0; 10.0; -10.0] = [Some 0.; Some 1.; None] *)
let log10s (lst: float list) : float option list =
    List.map ~f:(fun x -> if x > 0. then Some (log10 x) else
			    None) lst

assert (log10s [1.0; 10.0; -10.0] = [Some 0.; Some 1.; None])


(*>* Problem 1.2.c *>*)

(*  deoptionalize : Extracts values from a list of options.
 *        Example : deoptionalize [Some 3; None; Some 5; Some 10] = [3;5;10] *)

(* This can likely be done in 1 step, but this is how it went down *)
(* also note the None pattern match in the List.map function never occurs *)
let deoptionalize (lst:'a option list) : 'a list =
  let y = List.filter ~f:(function None -> false | Some _ -> true) lst in
  List.map ~f:(function None -> 0 | Some x -> x) y

assert (deoptionalize [Some 3; None; Some 5; Some 10] = [3;5;10])


(*>* Problem 1.2.d *>*)

(*  some_sum : Sums all of the numbers in a list of int options;
 *             ignores None values *)
let some_sum (nums:int option list) : int =
    sum (deoptionalize nums)

assert (some_sum [Some 3; None; Some 5; Some 10] = 18) 


(*>* Problem 1.2.e *>*)

(*  mult_odds : Product of all of the odd members of a list.
 *    Example : mult_odds [1;3;0;2;-5] = -15 *)
let mult_odds (nums:int list) : int =
    List.fold_right ~init:1 ~f:(fun x y -> x * y) (filter_odd nums)

assert(mult_odds [1;3;0;2;-5] = -15)

(*>* Problem 1.2.f *>*)

(*  concat : Concatenates a list of lists. See the Ocaml library ref *)
let concat (lists:'a list list) : 'a list =
    List.fold_right ~init:[] ~f:(@) lists

assert (concat [] = [])
assert (concat [[];[]] = [])
assert (concat [[1];[1;2]] = [1;1;2])


(*>* Problem 1.2.g *>*)

(* the student's name and year *)
type name = string
type year = int
type student = name * year

(*  filter_by_year : returns the names of the students in a given year
 *         Example : let students = [("Joe",2010);("Bob",2010);("Tom",2013)];;
 *                   filter_by_year students 2010 => ["Joe";"Bob"] *)
let filter_by_year (slist:student list) (yr:year) : name list =
  let x = List.filter ~f:(fun (_,y) -> if y = yr then true else false) slist in
  List.map ~f:(fun (x,_) -> x) x
 
assert (filter_by_year ([("Joe",2010);("Bob",2010);("Tom",2013)]) 2010 = 
	  ["Joe";"Bob"])

(*>* Problem 1.3 *>*)

(* Please give us an honest estimate of how long this Part of the problem
 * set took you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)

(* I think this was a good length. Not very challenging, but good practice *)
let minutes_spent_on_part_1 : int = 50;;

