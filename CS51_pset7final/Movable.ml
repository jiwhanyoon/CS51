open Helpers
open WorldObject
open WorldObjectI
open WEvent

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
  inherit world_object p as super

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 2 Movement ### *)
  initializer
    match inv_speed with
    | None -> ()
    | Some s -> self#register_handler 
        (WEvent.buffer s World.move_event) 
         (fun _ -> self#do_move)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 2 Movement ### *)
  (*world_object#move used because movable p inherits
    world_object p so it's part of self *)

  (*syntax of self#move takes a direction as its input*)
  method private do_move = self#move self#next_direction

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)
  (* random number generator for a random direction *)
  method next_direction = Some (Direction.random World.rand)

end
