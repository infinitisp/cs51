(*** CS 51 Problem Set 1 ***)
(*** January 31, 2014 ***)
(*** SILVIU PITIS ***)

(* Open up the library we'll be using in this course *)
open Core.Std

(* Problem 1 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

(*>* Problem 1a *>*)

let prob1a : string = 
  let greet (y:string) = "Hello " ^ y in 
  greet "World!";;

(*>* Problem 1b *>*)

let prob1b : int option list = 
  [Some 4; Some 2; None; Some 3];;

(*>* Problem 1c *>*)

let prob1c : ('a option * float option) * bool = 
  ((None, Some 42.0), true);;

(* Explain in a comment why the following will not type check,
   and provide a fix *)

(*>* Problem 1d *>*)

(* Original wouldn't compile because the the order of operations made the
 * type string * (int list)), instead of as below *)

let prob1d : (string * int) list = 
  [("CS", 51); ("CS", 50)];;

(*>* Problem 1e *>*)

(* Original wouldn't compile because there was an imposter float (3.9) that
 * screwed everything up - i changed everything to floats rather than changing
 * the 3.9 to an int *)

let prob1e : float =
  let compare (x,y) = x < y in
  if compare (4., 3.9) then 4. else 2.;;

(*>* Problem 1f *>*)

(* Orig wouldn't compile b/c list elements were not string * string, but int 
 * options, and also, the options that weren't None needed Some prefix *)

let prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); ("April", None);
   ("May", None); ("June", Some 1); ("July", None); ("August", None);
   ("September", Some 3); ("October", Some 1); ("November", Some 2); 
   ("December", Some 3)] ;;


(* Problem 2 - Write the following functions *)
(* For each subproblem, you must implement a given function and corresponding
 * unit tests (i.e. assert expressions). You are provided a high level
 * description as well as a prototype of the function you must implement. *)

(*>* Problem 2a *>*)

(* `reversed lst` should return true if the integers in lst are in
 * decreasing order. The empty list is considered to be reversed. Consecutive
 * elements can be equal in a reversed list. *)

(* Here is its prototype/signature: *)
(* reversed : int list -> bool *)

(* Implement reversed below, and be sure to write tests for it (see 2b for
 * examples of tests). *)

let rec reversed (l : int list) : bool =
  match l with
  |[] -> true
  |[_] -> true
  |a::b::l' -> 
    if a > b then
      reversed (b::l')
    else false

assert (reversed [])
assert (reversed [1])
(* what's syntax for negate? ! doesn't work *)
(* for that matter... is there a reference? Google seems useless for this
 * language, the assigned book is not a reference, and I'm just guessing
 * syntax everywhere (e.g. for explicitly typed optional args later on) *)
assert (reversed [1;2] = false)
assert (reversed [2;1])

(*>* Problem 2b *>*)

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

let rec merge (a: int list) (b: int list) : int list = 
  match a with
  | [] -> b
  | a_hd :: a' ->
     match b with
     | [] -> a
     | b_hd :: b' ->
	(* take lowest first element out & attach it to merged remaining *)
	if a_hd < b_hd then
	  a_hd :: merge a' b
	else
	  b_hd :: merge a b'

assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7])
assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7])
assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3])
assert ((merge [1;2] [1;2]) = [1;1;2;2])
assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001])
assert ((merge [] []) = [])
assert ((merge [1] []) = [1])
assert ((merge [] [-1]) = [-1])
assert ((merge [1] [-1]) = [-1;1])


(*>* Problem 2c *>*)
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

let rec unzip (a : (int * int) list) : int list * int list =
  match a with
  |[] -> ([],[])
  |(x,y)::a' -> let (x',y') = unzip (a') in
		(x::x',y::y')

assert (unzip [] = ([],[]))
assert (unzip [(1,2)] = ([1],[2]))
assert (unzip [(1,2);(3,4)] = ([1;3],[2;4]))

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

(* returns number of elements in list as a float *)
let rec numf (xs : 'a list) : float =
  match xs with
  | [] -> 0.
  | _ :: xs' -> 1. +. numf xs'

(* returns sum of elements in float list as a float *)
let rec sumf (xs : float list) : float =
  match xs with
  | [] -> 0.
  | x :: xs' -> x +. sumf xs'

(* returns mean of float list as a float *)
let meanf xs = sumf xs /. numf xs

(* returns float list w/ corresponding squared diffs of a float list *)
(* unsure how else to fix m below without including it in the funtion call *)
let rec squared_diffs (xs : float list) (m : float)  : float list =
  match xs with
  | [] -> []
  | x :: xs' -> (x -. m) ** 2. :: squared_diffs xs' m
					    
let variance (xs : float list) : float option =
  match xs with
  |[] -> None
  |[_] -> None
  |_ -> Some ( sumf (squared_diffs xs (meanf xs)) /.  (numf xs -. 1.))

assert (variance [1.0; 2.0; 3.0; 4.0; 5.0] = Some 2.5)
assert (variance [1.0] = None)
assert (variance [1.0; 1.0] = Some 0.)
assert (variance [] = None)

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
 *)

(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)

(* returns list of ints from 1 to n *)
let rec list_to_n (n : int) : int list =
  if n < 1 then []
  else list_to_n (n-1) @ [n]

(* returns number of divisors of n in a given integer list *)
(* like for squared_diffs, I think this is bad code, but don't know language *)
let rec divs_of_n_in_list (n : int) (lst : int list): int list =
  match lst with
  |[] -> []
  |x :: rest -> if n mod x = 0 then x :: divs_of_n_in_list n rest
		else divs_of_n_in_list n rest

let few_divisors (n : int) (m : int) : bool =
  if Float.to_int (numf (divs_of_n_in_list n (list_to_n n))) < m then
    true
  else
    false

(* again, how to negate? *)
assert (few_divisors 17 3)
assert (few_divisors 4 3 = false)
assert (few_divisors 4 4)
assert (few_divisors 1 1 = false)



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

(* why is it get super easy all of a sudden? *)

let rec concat_list (sep : string) (lst : string list) : string =
  match lst with
  |[] -> ""
  |[x] -> x
  |x :: xs -> x ^ sep ^ concat_list sep xs

assert (concat_list ", " ["Greg"; "Anna"; "David"] = "Greg, Anna, David")
assert (concat_list "..." ["Moo"; "Baaa"; "Quack"] = "Moo...Baaa...Quack")
assert (concat_list ", " [] =  "")
assert (concat_list ", " ["Moo"] = "Moo")


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

let add_char_to_run_length (c : char) (rl : (int*char) list) : (int*char) list =
  match rl with
  |[] -> [(1,c)]
  |(x,y) :: rest -> if c = y then (x + 1, y) :: rest
		    else (1,c) :: (x,y) ::  rest

let rec to_run_length (cs : char list) : (int * char) list =
  match cs with
  |[] -> []
  |a :: b -> add_char_to_run_length a (to_run_length b)

let rec from_run_length (rl : (int * char) list) : char list =
  match rl with
  |[] -> []
  |(x,y) :: rest -> if x = 1 then
		      y :: from_run_length rest
		    else
		      y :: from_run_length ((x-1, y) :: rest)

assert (from_run_length [(5,'a');(3,'b');(1,'c');(4,'d')] = 
 ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'])
assert (to_run_length (from_run_length [(5,'a');(3,'b');(1,'c');(4,'d')]) =
 [(5,'a');(3,'b');(1,'c');(4,'d')])
assert (from_run_length (to_run_length ['a';'c';'c']) = ['a';'c';'c'])

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


(* to combine one element into a single permutation of a list *)
let rec comb_with_list ?(pre: 'a list= []) (n:'a) (lst: 'a list) 
	: 'a list list  =
  match lst with
  |[] -> [pre @ [n]]
  |x::rest -> 
    (pre @ (n :: lst)) :: comb_with_list n rest ~pre:(pre @ [x])

(*to combine one element into each of a list of permutations *)
let comb_with_lists n lsts = List.concat (List.map ~f:(comb_with_list n) lsts)

let rec permute (lst : 'a list) : 'a list list =
  match lst with
  |[] -> []
  |[x] -> [[x]]
  |hd::tl -> comb_with_lists hd (permute tl)

assert (permute ['a'] = [['a']])
assert (permute [1;2] = [[1;2];[2;1]])
(* this assert stuff is a bit silly.. lists are order sensitive... *)
assert (permute [1;2;3] = [[1;2;3];[2;1;3];[2;3;1];[1;3;2];[3;1;2];[3;2;1]])
