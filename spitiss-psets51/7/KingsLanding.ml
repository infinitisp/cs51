open Core.Std
open Event51
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let starting_gold = 500
let cost_of_human = 10
let spawn_probability = 20
let gold_probability = 50
let max_gold_deposit = 3

(** King's Landing will spawn humans and serve as a deposit point for the gold
    that humans gather. It is possible to steal gold from King's Landing;
    however the city will signal that it is in danger and its loyal humans
    will become angry. *)
class kings_landing p : 
object
  inherit world_object_i
  method forfeit_treasury : int -> world_object_i -> int
  method get_gold_event : unit Event51.event
  method get_gold : int
  method set_dragon_false : unit
end =
object (self)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  val mutable gold_amount : int = starting_gold
  val gold_event : unit Event51.event = Event51.new_event ()

  (* ### TODO: Part 6 Custom Events ### *)

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
      (if World.rand gold_probability = 1 then
	gold_amount <- gold_amount + 1);
      (if (World.rand spawn_probability = 1) && (gold_amount > cost_of_human)
       then 
	 (gold_amount <- gold_amount - cost_of_human; self#generate_human)
      )
    ) 

  (* ### TODO: Part 4 Aging ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)

  method private generate_human =
    if World.rand 2 = 1 then
    World.spawn 1 p (fun x -> ignore (new Baratheon.baratheon x (self :> world_object_i)))
    else
    World.spawn 1 p (fun x -> ignore (new Lannister.lannister x (self :> world_object_i)))
      

  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "kings_landing"

  method! draw = self#draw_circle
			(Graphics.rgb 0xFF 0xD7 0x00) Graphics.black
			(string_of_int gold_amount)

  (* ### TODO: Part 3 Actions ### *)
  
  method! receive_gold ps =
    gold_amount <- gold_amount + (List.length ps);
    Event51.fire_event gold_event ();
    []
    

  (* ### TODO: Part 6 Custom Events ### *)

  (**********************************)
  (***** King's Landing Methods *****)
  (**********************************)

  (* ### TODO: Part 3 Actions ### *)

  method get_gold_event = gold_event

  method get_gold = gold_amount

  method forfeit_treasury n drag = 
    let old_gold = gold_amount in
    gold_amount <- max 0 (gold_amount - n);
    self#danger drag;
    min old_gold n 

  (* ### TODO: Part 6 Custom Events ### *)

  method set_dragon_false = ()

end
