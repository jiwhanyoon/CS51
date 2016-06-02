open WorldObject
open WorldObjectI
open Movable
open Ageable
open CarbonBased

(* ### Part 2 Movement ### *)
let bee_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
let max_pollen_types = 5

(* ### Part 4 Aging ### *)
let bee_lifetime = 1000

(* ### Part 5 Smart Bees ### *)
let max_sensing_range = 5


(* new bee type *)
class type bee_t = 
object
  inherit Ageable.ageable_t

  method private next_direction_default : Direction.direction option 
end


(** Bees travel the world searching for honey.  They are able to sense flowers
    within close range, and they will return to the hive once they have
    pollenated enough species of flowers. *)
class bee p (home: world_object_i) : bee_t =
object (self)
  inherit carbon_based p (bee_inverse_speed) 
  (World.rand bee_lifetime) bee_lifetime as super
(*   inherit ageable p (bee_inverse_speed) (World.rand bee_lifetime) bee_lifetime
 *)
   (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### Part 3 Actions ### *)
  val mutable pollen = []

  (* ### TODO: Part 5 Smart Bees ### *)
    val sensing_range = World.rand max_sensing_range 
    val pollen_types = World.rand max_pollen_types + 1

  (* ### TODO: Part 6 Custom Events ### *)
  val mutable sting = None

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler 
        (World.action_event)
         (fun _ -> self#do_action);

    self#register_handler
        (home#get_danger_event)
          (fun _ -> self#danger_action)
          
  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  method private do_action : unit =
    let default () = 
      let neighbors = World.get self#get_pos in
      List.iter self#deposit_pollen neighbors ;
      List.iter self#extract_pollen neighbors;
    in
    match sting with
    | Some obj -> (if obj#get_pos = self#get_pos
                   then obj#receive_sting; self#die)
    | None -> default ();

  (* ### TODO: Part 6 Custom Events ### *)

  method private danger_action: unit =
    let obj = List.hd (World.get self#get_pos)
    in
    ignore(sting <- Some obj);
     self#register_handler obj#get_die_event self#remove_sting;

  method private remove_sting () = 
    ignore (sting <- None)

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### Part 3 Actions ### *)
  method private deposit_pollen (o:world_object_i) : unit =
    let pollen' = o#receive_pollen pollen in
    pollen <- pollen'

  method private extract_pollen (o:world_object_i) : unit =
    match o#forfeit_pollen with
    | None -> ()
    | Some i -> pollen <- i::pollen



  (* ### TODO: Part 5 Smart Bees ### *)

    method private magnet_flower : world_object_i option =
    (* create a list just for flowers from the world objects *)
    let flower_list = 
      (* filter allows us to collect the ones that satisfy the bool function
          = collect whatever's true only *)
      (List.filter 
        (* of the flowers that smell like pollen *)
        (fun flower -> match flower#smells_like_pollen with 
          (* if there is an element that smells like pollen, 
              make sure it has not been stored in "pollen" through
            extract_pollen *)
          |Some fl -> not(List.mem fl pollen)
          (* if there is no flower that smells like pollen *)
          |None -> false) 
        (*apply the function to the list of objs within the range*)
        (World.objects_within_range (self#get_pos) (sensing_range))) 
    in
    if 
      (List.length flower_list > 0) 
    then
      Some ( List.hd ( List.sort (fun x y -> compare (Direction.distance self#get_pos x#get_pos) 
                        (Direction.distance self#get_pos y#get_pos)) flower_list) )
    else 
      None

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "bee"

(*   method draw = self#draw_circle Graphics.yellow
    Graphics.black (string_of_int (List.length pollen)) *)

  method draw_z_axis = 2

  (* ### TODO: Part 4 Aging ### *)

  method draw_picture = self#draw_circle Graphics.yellow
    Graphics.black (string_of_int (List.length pollen))
  method reset_life = self#reset_life

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  (* random direction *)
(*   method next_direction = 
  Some (Direction.random World.rand)
 *)

  (* ### TODO: Part 5 Smart Bees ### *)

    method next_direction = 
      (* more than pollen_types unique pollen identifiers, 
        then go back to the hive *)
      if List.length pollen >= pollen_types 
      then 
        World.direction_from_to (self#get_pos) (home#get_pos)
      else 
        match self#magnet_flower with
          (*if the bee can sense a magnet flower nearby, go towards it *)
          |Some fl -> World.direction_from_to (self#get_pos) (fl#get_pos)
          (* go in the next_default_direction *)
          |None -> self#next_direction_default

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees ### *)

    method private next_direction_default = None


end
