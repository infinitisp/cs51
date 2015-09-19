open Core.Std
open WorldObject
open WorldObjectI
open Human

(* Baratheons should travel in a random direction. *)
class baratheon p city : human_t =
object (self)
  inherit human p city as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Humans *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)

  method! get_name = "baratheon"
  method! private next_direction_default = Some (Direction.random World.rand)

  (***********************)
  (***** Human Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)



end


