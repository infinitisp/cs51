open Core.Std
open WorldObject
open WorldObjectI
open Human

class lannister p city : human_t =
object (self)
  inherit human p city

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Humans *)

  val mutable dir : Direction.direction option = 
    Some (Direction.random World.rand)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)

  method! get_name = "lannister"

  method! private draw_picture = self#draw_circle 
			Graphics.yellow Graphics.black
			(string_of_int (List.length gold_obj_list))
 
  method! private next_direction_default = 
    if World.can_move (
	   Direction.move_point self#get_pos dir) then
      dir else
      (dir <- Some (Direction.random World.rand); dir)

  (***********************)
  (***** Human Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)



end


