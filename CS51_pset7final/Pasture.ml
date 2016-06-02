open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let smelly_object_limit = 150

(** A pasture will spawn a cow when there are enough objects in the world that
    smell like pollen. *)
class pasture p (hive) : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

    initializer
    self#register_handler 
        (World.action_event)
          self#do_action


  (* ### TODO Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  method private do_action () = 
    if (smelly_object_limit < (World.fold
      (fun obj i -> match obj#smells_like_pollen with
      | Some _ -> i + 1
      | None -> i) 0
      )) && 
       (World.fold (fun obj i -> (obj#get_name <> "cow") && i) true)
    then
    (ignore(new Cow.cow self#get_pos hive (self :> world_object_i)); 
              print_string "mooooooooo "; flush_all ();)

  (* ### TODO Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO Part 1 Basic ### *)

  method get_name = "pasture"

  method draw = self#draw_circle (Graphics.rgb 70 100 130) 
    Graphics.white "P"

  method draw_z_axis = 1


end

