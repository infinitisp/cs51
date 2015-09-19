open Core.Std
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let town_limit = 200

(** The Wall will spawn a white walker when there are enough towns
    in the world. *)
class wall p kl: world_object_i =
object (self)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO Part 6 Custom Events ### *)
  initializer
    self#register_handler World.action_event self#do_action

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO Part 6 Custom Events ### *)

  method private do_action = 
    fun () -> 
    (
    let n_smells = World.fold (fun (x:world_object_i) y -> (
				       match x#smells_like_gold with
				       |None -> y
				       |Some _ -> 1 + y))
			      0
    in
    let ww_alive : bool = World.fold 
			     (fun x y ->
				       match x#get_name with
				       |"white_walker" -> true
				       |_ -> (y || false))
			      false 
    in
    if (n_smells > town_limit) && (not ww_alive) then
      (World.spawn 1 p (fun x -> ignore (new WhiteWalker.white_walker 
					     self#get_pos kl self));
       Printf.printf "white walkers! ";
       flush_all () )
    )

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO Part 1 Basic ### *)

  method! get_name = "wall"

  method! draw = self#draw_circle 
			(Graphics.rgb 70 100 130)
			Graphics.white
			"W"

end

