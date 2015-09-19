open Core.Std
open Event51
open Helpers
open WorldObject
open WorldObjectI

(** Class type for objects which constantly try to move in a calculated next
    direction. *)
class type movable_t =
object
  inherit world_object_i

  (** The next direction for which this object should move. *)
  method next_direction : Direction.direction option
end

class movable p (inv_speed:int option) : movable_t =
object (self)
  inherit world_object p

  val mutable i : int = 0

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 2 Movement ### *)

  initializer
  let i = ref 0 in
    match inv_speed with
    |None -> ()
    |Some s ->
      self#register_handler World.move_event (self#do_move s i)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 2 Movement ### *)
  method private do_move s i=
    fun () -> 
    i := !i + 1;
    if !i >= s then (
       i := 0;
       self#move self#next_direction)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)
  method next_direction = Some (Direction.random World.rand)

end
