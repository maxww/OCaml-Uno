(** 
   Representation of static card data.

   This module represents the cards in the game. Apart from getter functions,
   this module includes functions that cause the cards to change.
*)

(** The abstract type of values representing cards. *)
type t

(** The type of card colors. *)
type color = 
  | Red 
  | Green 
  | Yellow 
  | Blue 
  | Wild 

(** The type of numbered cards. *)
type value = 
  | Zero 
  | One 
  | Two 
  | Three 
  | Four 
  | Five 
  | Six 
  | Seven 
  | Eight 
  | Nine 
  | WildColor 
  | DrawFourColor
  | NoneV 

(** The type of action cards. *)
type action = 
  | Skip 
  | Reverse 
  | DrawTwo 
  | DrawFour
  | WildCard 
  | SwapHands 
  | NoneA

(** [get_color card] is the color of card [card]. *)
val get_color : t -> color

(** [get_value card] is the value of card [card]. *)
val get_value : t -> value

(** [get_action card] is the action of card [card]. *)
val get_action : t -> action

(** [make_card color value action] is the card with color [color], value 
    [value] and action [action]. *)
val make_card: color -> value -> action -> t

(** [color_to_string color] is the string representation of color [color]. *)
val color_to_string: color -> string

(** [value_to_string value] is the string representation of value [value]. *)
val value_to_string: value -> string

(** [action_to_string action] is the string representation of action 
    [action]. *)
val action_to_string: action -> string

(** [card_to_string card] is the string representation of card [card]. *)
val card_to_string: t -> string

(** [card_score card] converts the card [card] only a numerical score. *)
val card_score: t -> int
