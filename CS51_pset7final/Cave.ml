open WorldObject
open WorldObjectI
open Hive
open Bear

(* ### Part 6 Custom Events ### *)
let spawn_bear_pollen = 500

(** A cave will spawn a bear when the hive has collected a certain amount of
    honey. *)
class cave p hive: world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable honey_count = 0


  (* ### TODO: Part 6 Custom Events ### *)


  (***********************)
  (***** Initializer *****)
  (***********************)

   initializer
    self#register_handler 
        (hive#get_pollen_event)
         (fun _ -> self#do_action)

    method private do_action  =
      if (hive#get_pollen > spawn_bear_pollen) && 
               (World.fold (fun obj i -> (obj#get_name <> "bear") && i) true)
         then ignore (ignore (new Bear.bear (self#get_pos) hive (self:> world_object_i); );
                       print_string "omg bears! ";
                       flush_all();)



  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "cave"

  method draw = self#draw_circle Graphics.black
    Graphics.white "C"

  method draw_z_axis = 1


  (* ### TODO: Part 6 Custom Events *)

  method receive_pollen lst = 
    (honey_count <- honey_count + (List.length lst)); [];

end
