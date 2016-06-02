open WorldObject
open WorldObjectI
open Bee

(** Bouncy bees will travel in a straight line in a random direction until an
    obstacle or edge of the world is reached, at which point a new random
    direction will be chosen. *)
class bee_bouncy p hive : bee_t =
object (self)
  inherit bee p hive as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Bees *)
  method get_name = "bee_bouncy"

  val mutable randomdir = Some(Direction.random World.rand)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Bees *)

  method private next_direction_default = 
    if World.can_move (Direction.move_point self#get_pos randomdir)
    then randomdir
    else let randomdir1 = Some(Direction.random World.rand) 
    in 
    randomdir <- randomdir1 ;
    randomdir1


  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees *)

end
