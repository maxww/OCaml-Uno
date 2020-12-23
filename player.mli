(** 
   Representation of static player data.

   This module represents the players in the game. Apart from getter functions,
   this module includes functions that cause the player or list of players
   to change.
*)

(** The abstraction type of players. Players are represented as records,
    and they have a name, a hand and a unique position. *)
type t

(** [get_hand player] is the hand of player [player]. *)
val get_hand: t -> Card.t list

(** [get_pos player] is the position of player [player]. *)
val get_pos: t -> int

(** [get_name player] is the name of player [player]. *)
val get_name: t -> string

(** [make_player name pos hand] is the player with name [name], position [pos]
    and hand [hand]. *)
val make_player: string -> int -> Card.t list -> t

(** [set_pos player index] is the player [player] with position changed to
    [index]. *)
val set_pos: t -> int -> t

(** [hand_to_string hand] is the string representation of hand [hand]. *)
val hand_to_string: Card.t list -> string

(** [sort_players players] sorts the player list [players] in ascending order
    based on position. *)
val sort_players: t list -> t list 

(** [has_card player card] is true if player [player] has card [card] in their
    hand.*)
val has_card: t -> Card.t -> bool

(** [cards_equal c1 c2] is true if cards [c1] and [c2] are equal, false 
    otherwise. *)
val cards_equal: Card.t -> Card.t -> bool

(** [compare_players p1 p2] returns 0 if the positions of [p1] and [p2] are
    equal, -1 if the position of [p1] < [p2], and 1 otherwise. *)
val compare_players: t -> t -> int

(** [players_equal player1 player2] is true if players [player1] and [player2] 
    are equal, false otherwise. *)
val players_equal: t -> t -> bool

(** [get_pos_from_name name players] returns the position of the player with
    name [name] in [players]. 
    None if none of the players in [players] have name [name]. *)
val get_pos_from_name: string -> t list -> int option

(** [remove_card player card] is player [player] without card [card] in their 
    hand. *)
val remove_card: t -> Card.t -> t

(** [replace player player_list] replaces the player in [player_list] with 
    the same position as [player] with [player]. 
    Returns the new [player_list]. *)
val replace: t -> t list -> t list

(** [get_valuation t] Computes the total points score of card points in 
    Player [player]'s hand. *)
val get_valuation : t -> int
