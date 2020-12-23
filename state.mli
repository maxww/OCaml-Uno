(** 
   Representation of state data.

   This module is the representation of state in the game. The state represents
   the state of the game in the middle of play. This module includes getter
   functions as well as functions that change the state of the game.   
*)


(** [is_valid c1 c2] returns true if card [c1] can be validly placed on card 
    [c2] based on the following criteria.
    1. the two cards have the same color
    2. The two cards have the same numeric value or action
    3. The card is a wild colored card.
*)
val is_valid: Card.t -> Card.t -> bool

(** The abstraction type of state. States have a player list [players], 
    a current player [current], a deck [deck] , a top card [top_card],
    and discard pile [discard_pile]. *)
type state 

(** Constructer for a new game state with valid player list [players], 
    current player [current], current deck [deck] , top card [top_card],
    and discard pile [discard_pile].  *)
val update_state: Player.t list -> Player.t -> Card.t list -> Card.t ->
  Card.t list -> state 

(** [get_players state] returns current game's players in the order in which
    they play. *)
val get_players: state -> Player.t list

(**[get_curr state] returns current player. *)
val get_curr: state -> Player.t

(** [get_deck state] returns the deck of cards that have not been picked up. *)
val get_deck: state -> Card.t list

(** [get_top state] returns the current top facing card in the discard pile of
    the game's state. *)
val get_top: state -> Card.t

(** [get_discard state] returns the entire list of discarded cards in a 
    game. *)
val get_discard: state -> Card.t list

(** [next_player state turns] advances the game state [state]'s current player 
    [turns] places in the player order. *)
val next_player: state -> int -> Player.t

(** [endgame state] is true if a player has won the game, false otherwise. *)
val endgame: state -> bool

(** [get_ranks state] is the player list in order from first to last place. *)
val get_ranks: state -> Player.t list

(** [read_podium state] prints out the players in order of first to last 
    place. *)
val read_podium: state -> unit
