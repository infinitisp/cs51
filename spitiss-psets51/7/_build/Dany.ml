open Core.Std
open Event51
open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let spawn_dragon_gold = 500

(** Dany will spawn a dragon when King's Lnading has collected a certain
    amount of gold. *)
class dany p kl :
  object
    inherit world_object_i
    method set_dragon_false : unit
  end =
object (self)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 6 Custom Events ### *)

  val mutable dragon : bool = false

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 6 Custom Events ### *)

  initializer
    self#register_handler kl#get_gold_event self#do_dragon

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)

  method private do_dragon =
    fun () ->
    (
      if (kl#get_gold >= spawn_dragon_gold) && 
	 (not dragon) then
      (World.spawn 1 p (fun x -> 
		       ignore (new Dragon.dragon 
				   self#get_pos kl self));
		       Printf.printf "dragons! ";
		       dragon <- true;
		       flush_all () )
    )



  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "dany"

  method! draw = self#draw_circle 
			Graphics.black
			(Graphics.rgb 0x80 0x00 0x80)
			"D"

  (* ### TODO: Part 6 Custom Events *)

  method set_dragon_false = dragon <- false

end
