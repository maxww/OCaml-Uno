(** 
   Representation of static deck data.

   This module represents the deck in the game. This module includes functions 
   that cause the deck to change.
*)

(** Raised when an unknown color is encountered. *)
exception UnknownColor

(** Raised when an unknown value is encountered. *)
exception UnknownValue

(** Raised when an unknown action is encountered. *)
exception UnknownAction

(** Raised when the number of cards to deal is greater 
    than the length of the deck. *)
exception LargerThanDeck

(** [parse_list j] is a list of cards extracted from [j].
    Requires: j is a valid json file. *)
val parse_list: Yojson.Basic.t -> Card.t list

(** [shuffle lst] is the shuffled card list from [lst].*)
val shuffle: Card.t list -> Card.t list

(** [deal deck n_card] is the double (hand, deck). Returns the hand of the 
    first [n_cards] in deck and the deck without the cards dealt.  
    Raises: [LargerThanDeck] if [n_card > deck]. *)
val deal: Card.t list -> int -> (Card.t list * Card.t list)

(** [string_to_color string] is the color of string [string].
    Raises: UnknownColor if not a valid color. *)
val string_to_color: string -> Card.color

(** [string_to_value string] is the value of string [string].
    Raises: UnknownValue if not a valid value. *)
val string_to_value: string -> Card.value

(** [string_to_action string] is the action of string [string].
    Raises: UnknownAction if not a valid action. *)
val string_to_action: string -> Card.action

(** [draw deck n_card hand] is the double (hand, deck). Returns the hand 
    [hand] with [n_cards] cards added from the deck [deck], along with the 
    deck [deck] without the cards drawn. *)
val draw: Card.t list -> int -> Card.t list -> Card.t list * Card.t list