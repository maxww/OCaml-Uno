open Card
open Command
open Player

(** [is_numerical card] is true if the card [card] is a numerical card, and
    false otherwise. *)
let is_numerical card =
  match get_value card with
  | NoneV -> false
  | _ -> true

(** [is_color_special] is true if the card [card] is a numerical card, and
    false otherwise. *)
let is_color_special card =
  if get_color card <> Wild && get_action card <> NoneA then true 
  else false

(** [is_wild card] is true if the card [card] is a wildcard, and false
    otherwise. *)
let is_wild card =
  match get_color card with
  | Wild -> true
  | _ -> false

let is_valid c1 c2 =
  if is_numerical c1 && is_numerical c2 then 
    get_value c2 = get_value c1 || get_color c2 = get_color c1
  else if is_numerical c1 && is_color_special c2 then 
    get_color c1 = get_color c2
  else if is_color_special c1 && is_color_special c2 then 
    get_color c1 = get_color c2 || get_action c1 = get_action c2
  else if is_color_special c1 && is_numerical c2 then 
    get_color c1 = get_color c2 
  else get_color c1 = Wild || get_color c2 = Wild

type state = {
  players : Player.t list;
  current : Player.t; 
  deck : Card.t list;
  top_card : Card.t;
  discard_pile : Card.t list;
}

let update_state players current deck top_card discard_pile = {
  players = players;
  current = current;
  deck = deck;
  top_card = top_card;
  discard_pile = discard_pile;
}

let get_players state = state.players

let get_curr state = state.current

let get_deck state = state.deck

let get_top state = state.top_card

let get_discard state = state.discard_pile

let rec player_by_pos num players = 
  match players with 
  | [] -> failwith "This never happens"
  | h :: t -> 
    if get_pos h = num then h
    else player_by_pos num t

let next_player state turns = 
  let next = get_pos (get_curr state) + turns in
  if next < List.length (get_players state)
  then player_by_pos next (get_players state)
  else player_by_pos (next mod (List.length (get_players state))) 
      (get_players state)

(** [score_compare p1 p2] compares the total scores of players [p1] and [p2].
    Returns 1 if p1 the valuation of the hand is less than that of p2.
    Returns -1 if p2's score is smaller than p1. 
    Returns 0 otherwise. *)
let score_compare p1 p2 = 
  if (get_valuation p1) < (get_valuation p2) then 1
  else if (get_valuation p2) > (get_valuation p1) then -1 
  else 0

let endgame state = List.length 
    (List.filter (fun x -> List.length (get_hand x) = 0 ) state.players) >= 1

(** [places] is the list of string constants for podium readout. *)
let places = ["2nd:" ;"3rd:" ; "4th:" ]

let rec runners_up  acc pos players = 
  match players with
  | [] -> acc
  | h :: t -> runners_up (acc ^ ((List.hd pos) ^ get_name h )^ "\n") 
                (List.tl pos ) t

let get_ranks state = List.sort (score_compare) (get_players state)

let read_podium state = 
  let ranks = List.rev (get_ranks state) in
  print_endline ("The winner is: " ^ (get_name (List.hd ranks))^ "\n");

  print_endline ("and the the runners up: ");
  print_endline (runners_up "" places (List.tl ranks))