open Core.Std
open Helpers
open WorldObject
open WorldObjectI
open Movable
open Event51

(* ### Part 3 Actions ### *)
let gold_theft_amount = 1000

(* ### Part 4 Aging ### *)
let dragon_starting_life = 20

(* ### Part 2 Movement ### *)
let dragon_inverse_speed = Some 10

class dragon p kl dn: movable_t =
object (self)
  inherit movable p dragon_inverse_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  val mutable gold_stolen : int = 0
  val mutable life = dragon_starting_life

  (* ### TODO: Part 6 Events ### *)

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
      if self#get_pos = dn#get_pos then
	(gold_stolen <- 0; if kl#get_gold < (gold_theft_amount /2) then
			     self#die
	);
      if self#get_pos = kl#get_pos then
	gold_stolen <- gold_stolen + 
			 (kl#forfeit_treasury gold_theft_amount 
					      (self :> world_object_i))
    )


  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "dragon"

  method! draw = self#draw_circle Graphics.red Graphics.black
				  (string_of_int gold_stolen)

  method! draw_z_axis = 3


  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  method! die =
    if (dn#get_name = "dany") then dn#set_dragon_false;
    Event51.fire_event self#get_die_event () ;
    World.remove_must_exist self#get_pos (self :> world_object_i)

  method! receive_damage = life <- life - 1; if life <= 0 then self#die

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method! next_direction = if gold_stolen = 0 then 
			     World.direction_from_to self#get_pos kl#get_pos
			   else
			     World.direction_from_to self#get_pos dn#get_pos

  (* ### TODO: Part 6 Custom Events ### *)


end
