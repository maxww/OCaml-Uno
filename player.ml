open Card
open Deck

type t = {
  name : string;
  player_pos : int;
  hand : Card.t list 
}

let make_player name player_pos hand = {
  name = name;
  player_pos = player_pos;
  hand = hand;
}

let get_pos player = player.player_pos

let get_hand player = player.hand

let get_name player = player.name

let set_pos player index = make_player (get_name player) index 
    (get_hand player)

let hand_to_string hand = 
  let rec helper hand acc = 
    match hand with
    | [] -> acc
    | [h] -> acc ^ card_to_string h 
    | h :: t ->
      let card = String.trim (card_to_string h) in
      helper t (acc ^ card ^ " | ")
  in helper hand ""

let compare_players p1 p2 =
  if (get_pos p1) = (get_pos p2) then 0
  else if (get_pos p1) < (get_pos p2) then -1
  else 1

let sort_players players = 
  List.sort compare_players players

let cards_equal c1 c2 =
  if(get_color c1 = get_color c2) &&
    (get_value c1 = get_value c2) && 
    (get_action c1 = get_action c2)
  then true
  else false

(** [member hand card] is true if card [card] is in hand [hand]. Otherwise, 
    returns false.  *)
let rec member hand card = 
  match hand with
  | [] -> false
  | h :: t -> 
    if cards_equal h card then true
    else member t card

let has_card player card =
  if member (get_hand player) card then true
  else false

let remove_card player card =
  let hand = List.filter (fun x -> not(cards_equal x card)) 
      (get_hand player) in
  make_player (get_name player) (get_pos player) hand

let players_equal player1 player2 =
  player1.name = player2.name && player1.player_pos = player2.player_pos 
  && player1.hand = player2.hand

let rec get_pos_from_name name player_list =
  match player_list with
  | [] -> None
  | h :: t ->
    if (get_name h = name) then Some (get_pos h)
    else get_pos_from_name name t

let replace player player_list = 
  let rec replace_helper player player_list acc =
    match player_list with
    | [] -> acc
    | h :: t ->
      if (compare_players player h = 0) then replace_helper player t 
          (player :: acc) 
      else replace_helper player t (h :: acc)
  in
  replace_helper player player_list []

let get_valuation player =
  let rec card_total acc hand = 
    match hand with
    | [] -> acc
    | h :: t -> card_total (card_score h + acc) t 
  in card_total 0 player.hand

