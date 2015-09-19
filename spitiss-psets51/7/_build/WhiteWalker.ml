open Core.Std
open Helpers
open WorldObject
open WorldObjectI
open Movable

(* ### Part 2 Movement ### *)
let walker_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_destroyed_objects = 100

(** A White Walker will roam the world until it has destroyed a satisfactory
    number of towns *)
class white_walker p kl wall: movable_t =
object (self)
  inherit movable p walker_inverse_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  val mutable objects_vanquished = 0

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  initializer
    self#register_handler World.action_event self#do_action

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)

  method private do_action = 
    fun () ->
    (
      if objects_vanquished < max_destroyed_objects then (
      let xs = World.get self#get_pos in
      let rec do_neighbors lst =
	match lst with
	|[] -> ()
	|x :: ys ->
	  (
	    match x#smells_like_gold with
	    |None -> do_neighbors ys
	    |Some _ -> 
	    (
	      x#die;
	      objects_vanquished <- objects_vanquished + 1
	    );
	    do_neighbors ys
	  )
      in
      do_neighbors xs
      ) else if
	self#get_pos = wall#get_pos then self#die
    )
    

  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "white_walker"

  method! draw = self#draw_circle
			(Graphics.rgb 0x89 0xCF 0xF0)
			Graphics.black
			(string_of_int objects_vanquished)

  method! draw_z_axis = 4


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method! next_direction = 
    if objects_vanquished < max_destroyed_objects then
    (if Random.float 1. < (2. /. Float.of_int World.size) then
      World.direction_from_to self#get_pos kl#get_pos
    else
      Some (Direction.random World.rand))
    else
      World.direction_from_to self#get_pos wall#get_pos

  (* ### TODO: Part 6 Custom Events ### *)

end
