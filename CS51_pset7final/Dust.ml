(* ### Part 4 Aging ### *)
let dust_lifetime = 50

(** Dust is what remains when carbon-based objects die. *)
class dust p (name:string) : Ageable.ageable_t =
object (self)
  inherit Ageable.ageable p None dust_lifetime dust_lifetime as super

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)
    method draw_picture : unit = self#draw_circle (Graphics.rgb 150 150 150)
    Graphics.black (String.sub name 0 2)
    method reset_life : unit = self#reset_life

end
