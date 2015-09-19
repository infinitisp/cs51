(* NAMES:
 *
 * Partner 1's name: Silviu Pitis
 * Partner 1's code.seas account: spitis i think... 
 * are we supposed to use this for something other than getting the pset?
 *)

open Core.Std

(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)

(* I think this doesn't use any extra space? my first attempt... 
 * sadly I don't think I see the easier way that uses more space,
 * unless you mean building up a new list of pointers as you go,
 * which is a lot harder in my opinion... *)

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

let has_cycle (lst : 'a mlist) : bool =

  (* helper that checks list up to e for whether e points to list *)
  let rec checksofar lst' e : bool =
    match lst' with
    |Nil -> false
    |Cons (x, r) -> (match e with
		     |Nil -> false (* should never happen, but putting an
				    * assert stmt here before false gave
				    * warning (why?) / but compiled *)
		     |Cons (x', r') -> 
		       (match !r' with (* if e's ref -> lst then cyclic *)
			|Nil -> false (* if e points to nil quit early *)
			|Cons (x'', r'') ->
			  if (x = x'') && (phys_equal (!r) (!r'')) then true
			  else (*check if e = lst before recurring *)
			    (* note that MUST do it in this order to catch
			     * elements that refer to themselves *)
			    if (x = x') && (phys_equal (!r) (!r')) then
			      checksofar lst (!r')
			    else checksofar (!r) e))
  in

  checksofar lst lst

assert (has_cycle list2)
assert (not(has_cycle list1))
;;

(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  let rec checksofar lst' e : unit =
    match lst' with
    |Nil -> ()
    |Cons (x, r) -> (match e with
		     |Nil -> ()
		     |Cons (x', r') -> 
		       (match !r' with
			|Nil -> ()
			|Cons (x'', r'') ->
			  if (x = x'') && (phys_equal (!r) (!r'')) then
			    (* only difference here is changing behavior
			     * when cyclic: *)
			    (r' := Nil)
			  else 
			    if (x = x') && (phys_equal (!r) (!r')) then
			      checksofar lst (!r')
			    else checksofar (!r) e))
  in

  checksofar lst lst

let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b);;

flatten list1;
assert (list1 = Cons(1, ref list1b));
assert (list1a = Cons(2, ref Nil));;

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2;;

assert (has_cycle(list2));
flatten list2;
assert (list2 = Cons(1, ref (Cons (2, ref Nil))));
assert (not(has_cycle list2));;


(*>* Problem 1.3 *>*)
(* Write mlength, which finds the number of nodes in a mutable list. *)
let mlength (lst : 'a mlist) : int =
  (* copy pasting code is fun! *)
  let y = ref 1 in
  let rec checksofar lst' e : unit =
    match lst' with
    |Nil -> ()
    |Cons (x, r) -> (match e with
		     |Nil -> ()
		     |Cons (x', r') -> 
		       (match !r' with
			|Nil -> ()
			|Cons (x'', r'') ->
			  if (x = x'') && (phys_equal (!r) (!r'')) then
			    (* do not mutate this time, just return *)
			    ()
			  else 
			    if (x = x') && (phys_equal (!r) (!r')) then
			      (* every time we use a new second input,
			       * that means our length increases *)
			      let _  = y := !y + 1 in checksofar lst (!r')
			    else checksofar (!r) e))
  in

  match lst with
  |Nil -> 0
  |Cons (_, _) -> checksofar lst lst; !y

let list1a = Cons(2, ref Nil);;
assert (mlength list1a = 1);;
let list1b = Cons(2, ref list1a);;
assert (mlength list1b = 2);;

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2;;
assert (mlength list2 = 2);;

  
(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 60
(* This section was exemplary compared to the previous 2 or 3 data-enty psets. 
 * Make more like this. I actually learn from this *)
