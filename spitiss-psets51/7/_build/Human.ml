open Core.Std
open Event51
open Helpers
open WorldObject
open WorldObjectI
open Movable
open Ageable
open CarbonBased

(* ### Part 2 Movement ### *)
let human_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
let max_gold_types = 5

(* ### Part 4 Aging ### *)
let human_lifetime = 1000

(* ### Part 5 Smart Humans ### *)
let max_sensing_range = 5

(** Humans travel the world searching for towns to trade for gold.
    They are able to sense towns within close range, and they will return
    to King's Landing once they have traded with enough towns. *)


class type human_t =
	  object
	    inherit Ageable.ageable_t
	    val mutable gold_obj_list : int list
	    method private next_direction_default : Direction.direction option
	  end

class human p home : human_t =
object(self)
  inherit carbon_based p human_inverse_speed 
		  (World.rand human_lifetime) human_lifetime

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  val mutable gold_obj_list : int list = []
  val sensing_range = World.rand max_sensing_range
  val gold_types = World.rand max_gold_types + 1
  val mutable danger_exists = None


  (* ### TODO: Part 5 Smart Humans ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  initializer
    self#register_handler World.action_event self#do_action;
    self#register_handler home#get_danger_event self#do_danger

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)

  method private do_action =
    fun () ->
    (
    let xs = World.get self#get_pos in
    let rec do_neighbors lst = 
      match lst with
      |[] -> ()
      |x :: ys ->
	self#deposit_gold x;
	self#extract_gold x;
        do_neighbors ys
    in
    do_neighbors xs;
    (match danger_exists with
     |None -> ()
     |Some drag -> if self#get_pos = drag#get_pos then
		     (drag#receive_damage; self#die)
    )
    )

  method private do_danger drag =
		 danger_exists <- Some drag;
		 self#register_handler drag#get_die_event self#drag_died

  method private drag_died () =
    danger_exists <- None
		 

  method private deposit_gold x =
    gold_obj_list <- x#receive_gold gold_obj_list 

  method private extract_gold x = 
    match x#forfeit_gold with
    |None -> ()
    |Some g -> gold_obj_list <- g :: gold_obj_list

  (* ### TODO: Part 5 Smart Humans ### *)

  method private next_direction_default = None

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "human"

  method! private draw_picture = self#draw_circle 
			(Graphics.rgb 0xC9 0xC0 0xBB) Graphics.black
			(string_of_int (List.length gold_obj_list))

  method! draw_z_axis = 2

  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method! next_direction = 
    match danger_exists with 
    |Some drag -> World.direction_from_to self#get_pos drag#get_pos
    |None -> (
    if List.length (List.dedup gold_obj_list) >= gold_types then
      World.direction_from_to self#get_pos home#get_pos else
    match self#magnet_gold with
    |None -> self#next_direction_default
    |Some x -> World.direction_from_to self#get_pos x#get_pos
    )

  (* ### TODO: Part 5 Smart Humans ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (**** Human Methods ****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans ### *)
  method private magnet_gold : world_object_i option = 
    let lst = World.objects_within_range self#get_pos sensing_range in
    let towns = List.filter ~f:(fun x -> x#get_name = "town") lst in
    let mag_towns = List.filter 
		   ~f:(fun x -> match x#smells_like_gold with
				|None -> false
				|Some y -> 
				  if not (List.mem gold_obj_list y) then
				    true else false) towns
    in
    if List.is_empty mag_towns then None
    else (
    let sorted_mag_towns = List.sort ~cmp:(fun x y ->
					   World.cdistance self#get_pos 
							   x#get_pos 
							   y#get_pos)
				     mag_towns
    in
    Some (List.hd_exn sorted_mag_towns)
    )     

end
