open Dust

(** Carbon based objects eventually die, and leave dust behind when they do. *)
class carbon_based p inv_speed starting_lifetime max_lifetime : Ageable.ageable_t =
object (self)
  inherit Ageable.ageable p inv_speed starting_lifetime max_lifetime as super

  (***********************)
  (***** Initializer *****)
  (***********************)

    initializer
    self#register_handler self#get_die_event
         (fun _ -> self#do_action)

  method private do_action = 
    World.spawn 1 self#get_pos 
    (fun p -> ignore (new dust p self#get_name))
  (* ### TODO: Part 4 Aging *)

  method draw_picture = self#draw
  method reset_life = self#reset_life

end
