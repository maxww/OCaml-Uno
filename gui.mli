(** 
   Representation of the Graphical User Interface.

   This module represents the screens drawn in the game. This module includes 
   functions that draw a Graphical User Interface on the screen. 
*)

(** The type representing whether the cards should be masked or unmasked. *)
type masked = 
  | Masked
  | Unmasked

(** [draw_state state masked] draws the state [state] onto the screen. 
    [masked] indicates whether the cards in the player's hand should be 
    masked. *)
val draw_state: State.state -> masked -> unit

(** [is_click x y (x, y, l, h)] returns true if the coordinates given by (x, y)
    are located in the box bounded by (x, y, l, h).  *)
val is_click: int -> int -> int * int * int * int -> bool

(** [clicked_discard (x, y)] is true if the coordinates [(x, y)] are inside 
    the discard pile. *)
val clicked_discard: int * int -> bool

(** [clicked_hand  (x, y)] is true if the coordinates [(x, y)] are inside 
    the hand. *)
val clicked_hand: int * int -> bool

(** [clicked_draw  (x, y)] is true if the coordinates [(x, y)] are inside 
    the draw pile. *)
val clicked_draw: int * int -> bool

(** [clicked_two_players  (x, y)] is true if the coordinates [(x, y)] are 
    inside the two players icon. *)
val clicked_two_players: int * int -> bool

(** [clicked_two_players  (x, y)] is true if the coordinates [(x, y)] are 
    inside the three players icon. *)
val clicked_three_players: int * int -> bool

(** [clicked_two_players (x, y)] is true if the coordinates [(x, y)] are 
    inside the four players icon. *)
val clicked_four_players: int * int -> bool

(** [get_card_gui (x, y) state] is the card option returning Some card if the
    coordinates (x, y) in the state [state] correspond to a card in the hand. 
    Otherwise returns None. *)
val get_card_gui: int * int -> State.state -> Card.t option

(** [draw_selected_state card state] draws the state [state] onto the screen 
    with a highlighted card [card]. *)
val draw_selected_state: Card.t -> State.state -> unit

(** [text_display text font x y] draws string [text] with font [font] on 
    the screen at position [(x, y)].  *)
val text_display: string -> string -> int -> int -> unit

(** [draw_instructions_screen ()] draws the instructions screen. *)
val draw_instructions_screen: unit -> unit

(** [draw_welcome_screen ()] draws the welcome screen. *)
val draw_welcome_screen: unit -> unit

(** [draw_player_screen ()] draws the player selection screen. *)
val draw_player_screen: unit -> unit

(** [draw_player_name_screen num_of_players] draws the player name choosing
    screen for [num_of_players] players. *)
val draw_player_name_screen: int -> unit

(** [draw_invalid_place ()] draws the invalid card placement message. *)
val draw_invalid_place: unit -> unit

(** [clicked_blue (x, y)] returns true if the user clicked on the blue area
    of the circle, otherwise false. *)
val clicked_blue: float * float -> bool

(** [clicked_green (x, y)] returns true if the user clicked on the green area
    of the circle, otherwise false. *)
val clicked_green: float * float -> bool

(** [clicked_yellow (x, y)] returns true if the user clicked on the yellow 
    area of the circle, otherwise false. *)
val clicked_yellow: float * float -> bool

(** [clicked_red (x, y)] returns true if the user clicked on the red area
    of the circle, otherwise false. *)
val clicked_red: float * float -> bool

(** [draw_color_selection_screen ()] draws the color selection screen when 
    someone plays a wildcard. *)
val draw_color_selection_screen: unit -> unit

(** [draw_invalid_player_name ()] draws the invalid player name message. *)
val draw_invalid_player_name: unit -> unit

(** [draw_cards_notice ()] draws the notice on game customization with json. *)
val draw_cards_notice: unit -> unit

(** [draw_swap_hands ()] draws the swap hands screen. *)
val draw_swap_hands: unit -> unit

(** [draw_winner_screen first second third fourth] draws the winner screen. *)
val draw_winner_screen: string -> string -> string option -> string option 
  -> unit

(** [draw_play_again ()] draws the screen prompting the player to play again
    or quit. *)
val draw_play_again: unit -> unit
