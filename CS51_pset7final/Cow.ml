open WorldObject
open WorldObjectI
open Movable
open Hive

(* ### Part 2 Movement ### *)
let cow_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_consumed_objects = 20

(** Cows will graze across the field until it has consumed a satisfactory number
    of flowers *)
class cow p hive home: movable_t =
object (self)
  inherit movable p cow_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### Part 3 Actions ### *)
  val mutable consumed_objects = 0


  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler 
        (World.action_event)
         (fun _ -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### Part 3 Actions ### *)
  method private do_action =
    if consumed_objects < max_consumed_objects then
      let neighbors = World.get self#get_pos in
      List.iter begin fun o -> match o#smells_like_pollen with
        | Some _ -> print_string "*nom* " ; flush_all () ;
          o#die ;
          consumed_objects <- consumed_objects + 1
        | None -> ()
      end neighbors
    else self#die
   
  (* ### TODO: Part 6 Custom Events ### *)


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "cow"

  method draw = self#draw_circle (Graphics.rgb 180 140 255) 
    Graphics.black (string_of_int consumed_objects)

  method draw_z_axis = 4


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method next_direction = 
  if consumed_objects >= max_consumed_objects then
  World.direction_from_to self#get_pos home#get_pos
  else if World.rand (World.size/2) = 0 
  then World.direction_from_to self#get_pos hive#get_pos
  else Some (Direction.random World.rand)


  (* ### TODO: Part 6 Custom Events ### *)

end
