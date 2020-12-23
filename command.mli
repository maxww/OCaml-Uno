(**
   Representation of commands.

   This module represents the commands and their parsing in the game. This 
   module includes functions that parse player commands.
*)

(** The type [card phrase] represents the card phrase that is generated
    with a player command. Each element in the list is either a color and 
    number/action pair or a single special card. For example,
    - If the player command is:
      "play red 1", the card phrase is ["red"; "1"]
    - If the player command is:
      "play wildcard", the card phrase is ["wildcard"]
*)
type card_phrase = string list

(** the type [command] represents the two possible actions that a player
    can make: Play and Draw. *)
type command = 
  | Play of card_phrase
  | Draw

(** [Empty] is raised when an empty command is made. *)
exception Empty

(** [Malformed] is raised when an invalid command is made.  *)
exception Malformed

(** [parse_command str] parses a string into a [command].
    Raises: [Empty] if the [str] is the empty string or only has whitespace. 
    Raises: [Malformed] if the [str] does not produce a valid command. *)
val parse_command: string -> command

(** [phrase_to_card phrase] converts card_phrase [phrase] into a card.
    Raises: [Malformed] if the [phrase] is not a valid phrase. *)
val phrase_to_card: string list -> Card.t