open Player
open Card
open Deck
open Gui
open State
open Command

(** [wait_for_event ()] waits for a key to be pressed or for the mouse
    to be clicked. If so, returns unit, otherwise, keeps on waiting.  *)
let rec wait_for_event () =
  let event = Graphics.wait_next_event [Graphics.Button_down; 
                                        Graphics.Key_pressed] in
  if event.Graphics.keypressed then ()
  else if event.Graphics.button then ()
  else wait_for_event ()

(** [delete s font x y offset] is a helper function for the delete
    functionality in textbox. *)
let delete s font x y offset = 
  Graphics.set_color Graphics.black;
  Graphics.fill_rect (x - offset) (y - 10) 20 50;
  let len = String.length s in
  String.sub s 0 (len - 1) 

(** [typing c font x y] is a helper function for the typing functionality
    in textbox. *)
let typing c font x y =
  Graphics.moveto x y;
  Graphics.set_font font;
  Graphics.draw_char c

(** [textbox s font x y offset] emulates a textbox, allowing the user to type
    on the GUI screen in font [font] at position [(x, y)]. *)
let rec textbox s font x y offset =
  Graphics.set_color Graphics.white;
  let event = Graphics.wait_next_event [Graphics.Key_pressed] in
  if event.Graphics.keypressed then
    let c = event.Graphics.key in
    match c with
    | '\r' -> s
    | '\b' -> begin
        try(
          let string = delete s font x y offset  in
          textbox string font (x - offset) y offset )
        with
        | _ -> textbox s font x y offset
      end
    | _ ->
      typing c font x y;
      textbox (s ^ (Char.escaped c)) font (x + offset) y offset
  else
    textbox s font x y offset

(** [check_name_in_list player_lst name] returns true if a player of name
    [name] is in [player_lst], false otherwise. *)
let rec check_name_in_list player_lst name =
  match player_lst with
  | [] -> false
  | h :: t ->
    let n = get_name h in
    if n = name then true
    else check_name_in_list t name

(** [delete_row x y] deletes a row of text at position (x, y).  *)
let delete_row x y =
  Graphics.set_color Graphics.black;
  Graphics.moveto x y;
  Graphics.fill_rect x y 600 30  

(** [type_name player_lst font count] checks whether the name the player
    inputs has already been used. If so, returns [name]. Otherwise, deletes. *)
let rec type_name player_lst font count =
  let name = textbox "" font 400 (405  - 100 * count) 15 in
  if not (check_name_in_list player_lst name) then name
  else 
    ( delete_row 400 (405 - 100 * count);
      type_name player_lst font count)

(** [init_players shuffled_deck num] initializes [num] players and the deck
    after giving them each 7 cards from the deck [shuffled_deck]. *)
let init_players shuffled_deck num = 
  Gui.draw_player_name_screen num;
  let rec helper shuffled_deck num acc count = 
    if num =  0 then (acc, shuffled_deck)
    else
      (
        let font = "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1" in
        let name = type_name acc font count in
        try (let name = String.sub name 0 10 in
             let hand = deal shuffled_deck 7 in
             let player = make_player name (num - 1) (fst hand) in
             helper (snd hand) (num - 1) (player :: acc) (count + 1))
        with 
        | _ -> let hand = deal shuffled_deck 7 in
          let player = make_player name (num - 1) (fst hand) in
          helper (snd hand) (num - 1) (player :: acc) (count + 1)
      )
  in
  helper shuffled_deck num [] 0    

(** [choose_players string] checks that the number of players is <= 4 and
    returns the integer equivalent. *)
let rec choose_players string = 
  match int_of_string_opt string with
  | None -> print_endline "Please input a valid number of players."; 
    let s = read_line () in
    choose_players s
  | num when ((Option.get num) > 4) -> print_endline "Too many players. 
  Please try again."; 
    let s = read_line () in
    choose_players s
  | _ -> Option.get (int_of_string_opt string)

(** [print_players lst] prints out to the console the names of the 
    players in [lst]. *)
let print_players lst =
  print_endline("\n These are the current players:");
  let rec print lst =
    match lst with
    | [] -> ""
    | h :: t -> "- " ^ get_name h ^ "\n" ^ print t 
  in print_endline (print lst)

(** [overdraw_helper state] reshuffles the discard pile into the deck if there
    cards are drawn when there are not enough cards remaining in the deck. *)
let overdraw_helper state =
  let shuffled = shuffle (get_discard state) in
  let top_card = get_top state in
  let players = get_players state in
  let current = get_curr state in
  update_state players current shuffled top_card [top_card] 

(** [string_to_unit s] returns unit. *)
let string_to_unit s = ()

(** [parse_color str wild_type] parses the color chosen by the player 
    after playing a wild card. Returns a card of the color specified
    if color is valid. Otherwise, prompts the player to choose again.  *)
let rec parse_color str wild_type = 
  let s = String.trim str in
  try make_card (string_to_color s) wild_type NoneA
  with UnknownColor -> 
    print_endline "Invalid color. Please choose again.";
    let read = read_line () in
    parse_color read wild_type

(** [number_helper players state top_card prev_discard] is the state after
    a numbered card is played. *)
let number_helper players state top_card prev_discard =
  let next = next_player state 1 in
  update_state players (next) (get_deck state) top_card 
    (top_card :: prev_discard)

(** [skip_helper players state top_card prev_discard] is the state after a
    skip card is played.*)
let skip_helper players state top_card prev_discard =
  let next = next_player state 2 in 
  update_state players 
    (next) (get_deck state) top_card (top_card :: prev_discard)

(** [draw_no_error players state top_card cards_drawn prev_discard] is the 
    state after a player draws cards. *)
let draw_no_error players state top_card cards_drawn prev_discard = 
  let next = next_player state 2 in
  let gets_cards = next_player state 1 in
  let draw = draw (get_deck state) cards_drawn 
      (get_hand gets_cards) in
  let new_hand = fst draw in
  let new_deck = snd draw in
  let player = make_player (get_name gets_cards) 
      (get_pos gets_cards) (new_hand) in
  let players = replace player players in
  update_state players next new_deck top_card (top_card :: prev_discard)

(** [draw_helper players state top_card cards_drawn prev_discard] is the state
    after the player draws cards. Handles overdraws. *)
let draw_helper players state top_card cards_drawn prev_discard =
  try( draw_no_error players state top_card cards_drawn prev_discard )
  with
  | LargerThanDeck -> 
    let overdraw_state = overdraw_helper state in
    draw_no_error players overdraw_state top_card cards_drawn prev_discard

(** [rev player_lst] is the reversed player list [player_lst] by position. *)
let rev player_lst =
  let sorted_lst = sort_players player_lst in
  let reverse_lst = List.rev (sort_players player_lst) in
  let rec swap sorted_lst reverse_lst acc =
    match sorted_lst, reverse_lst with
    | [], [] -> acc
    | (h1 :: t1), (h2 :: t2) ->
      let player = (set_pos h1 (get_pos h2)) in
      swap t1 t2 (player :: acc)
    | _ -> failwith "never happens"
  in
  swap sorted_lst reverse_lst []

(** [player_from_list player players] is the player with the same name as
    player [player] in player list [players]. *)
let rec player_from_list player players =
  match players with
  | [] -> failwith "never happens"
  | h :: t ->
    if (get_name player = get_name h) then h
    else player_from_list player t

(** [reverse_helper players state top_card prev_discard] is the state after
    a reverse card is played. *)
let reverse_helper players state top_card prev_discard =
  let reversed = rev players in
  let current = player_from_list (get_curr state) reversed in
  let state = update_state reversed current (get_deck state) 
      top_card prev_discard in
  let next = next_player state 1 in
  update_state reversed next (get_deck state) top_card 
    (top_card :: prev_discard)

(** [color selection ()] draws a color selection circle on the screen and
    waits for a color to be clicked on. *)
let rec color_selection () = 
  let event = Graphics.wait_next_event [Graphics.Button_down] in
  let mouse_x = float_of_int event.Graphics.mouse_x in
  let mouse_y = float_of_int event.Graphics.mouse_y in
  if event.button && Gui.clicked_blue (mouse_x, mouse_y) then "blue"
  else if event.button && Gui.clicked_green (mouse_x, mouse_y) then "green"
  else if event.button && Gui.clicked_yellow (mouse_x, mouse_y) then "yellow"
  else if event.button && Gui.clicked_red (mouse_x, mouse_y) then "red"
  else color_selection ()

(** [wild_helper players state next play_card prev_discard wild] is the state
    after a wild card is played. *)
let wild_helper players state next play_card prev_discard wild = 
  Gui.draw_color_selection_screen ();
  let color = color_selection () in
  let new_color = parse_color color wild in
  let next = next_player state next in
  update_state players (next) (get_deck state) 
    new_color (play_card :: prev_discard)

(** [drawcard_helper state players curr_player hand] is the state after the
    player chooses to draw a card from the deck. *)
let drawcard_helper state players curr_player hand =
  let hand_deck = draw (get_deck state) 1 hand in
  let new_hand = fst hand_deck in 
  let new_player = make_player (get_name curr_player) 
      (get_pos curr_player) new_hand in 
  let new_players = replace new_player players in 
  let new_deck = snd hand_deck in 
  let top_card = get_top state in
  let next = next_player state 1 in 
  update_state new_players next new_deck top_card 
    (get_discard state)

(** [loops state card] is the command generated when the player interacts with
    the GUI. *)
let rec loops state card = 
  let event = Graphics.wait_next_event [Graphics.Button_down] in
  if event.Graphics.button && 
     clicked_hand (event.Graphics.mouse_x, event.Graphics.mouse_y) then
    let card = Gui.get_card_gui 
        (event.Graphics.mouse_x, event.Graphics.mouse_y) state in
    if not (Option.is_none card) then
      (draw_selected_state (Option.get card) state;
       loops state card)
    else 
      (draw_state state Unmasked;
       loops state None)
  else if event.Graphics.button && 
          clicked_draw (event.Graphics.mouse_x, event.Graphics.mouse_y)
  then "draw"
  else if event.Graphics.button && 
          clicked_discard (event.Graphics.mouse_x, event.Graphics.mouse_y)
          && not (Option.is_none card)
  then "play " ^ card_to_string (Option.get card)
  else (draw_state state Unmasked;
        loops state None)

(** [players_to_assoc players acc] is the association list of players in the
    form (name, hand). *)
let rec players_to_assoc players acc =
  match players with
  | [] -> acc
  | h :: t ->
    let player_name = get_name h in
    let hand = get_hand h in 
    players_to_assoc t ((player_name, hand) :: acc)

(** [swap_name_in_list player_assoc font] draws a screen prompting the player
    to choose another player to swap hands with. *)
let rec swap_name_in_list player_assoc font =
  draw_swap_hands ();
  let name = textbox "" font 400 200 15 in
  if List.mem_assoc name player_assoc then name
  else
    (draw_invalid_player_name ();
     wait_for_event ();
     swap_name_in_list player_assoc font)

let rec swap player players acc =
  match players with
  | [] -> acc
  | h :: t ->
    if get_name player = get_name h 
    then swap player t (player :: acc) 
    else swap player t (h :: acc)

(** [swap_hands_helper state players curr_player hand] is the state after
    a swap hands card is played. *)
let swap_hands_helper state players curr_player hand =
  let dict = players_to_assoc players [] in
  let curr_name = get_name curr_player in
  let dict = List.remove_assoc curr_name dict in
  let font = "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1" in
  let swap_name = swap_name_in_list dict font in 
  let swap_hand = List.assoc swap_name dict in
  let swap_pos = get_pos_from_name swap_name players in
  let current_player = make_player curr_name (get_pos curr_player)
      swap_hand in 
  let player_lst = swap current_player players [] in
  let swap_player = make_player swap_name (Option.get swap_pos) hand in
  swap swap_player player_lst []

(** [rank_players state] draws the winner/ranking screen when the game is 
    done. *)
let rank_players state =
  let ranks = List.rev (get_ranks state) in
  match ranks with
  | [f; s] -> 
    let first = get_name f in
    let second = get_name s in
    draw_winner_screen first second None None 
  | [f; s; t] -> 
    let first = get_name f in 
    let second = get_name s in
    let third = get_name t in
    draw_winner_screen first second (Some third) None
  | [f; s; t; ft] ->
    let first = get_name f in 
    let second = get_name s in
    let third = Some (get_name t) in
    let fourth = Some (get_name ft) in
    draw_winner_screen first second third fourth
  | _ -> ()

(** [instructions_helper ()] draws the instructions screen and waits for 
    either a mouse click or a key press before continuing. *)
let instructions_helper () =
  draw_instructions_screen ();
  wait_for_event ()

(** [player_selection ()] draws the player selection screen and waits for
    a number of players button to be pressed. Returns the number of players,
    as a string. *)
let rec player_selection () =
  let event = Graphics.wait_next_event [Graphics.Button_down] in
  if event.Graphics.button then
    let x = event.Graphics.mouse_x in
    let y = event.Graphics.mouse_y in
    if clicked_two_players (x, y) then "2"
    else if clicked_three_players (x, y) then "3"
    else if clicked_four_players (x, y) then "4"
    else player_selection ()
  else player_selection ()

(** [deck_builder json] is the initial deck built from the json [json]. *)
let deck_builder json = 
  try (parse_list json)
  with
  | UnknownColor -> 
    print_endline "Invalid color. Check your json file and try again."; 
    exit 0
  | UnknownValue -> 
    print_endline "Invalid value. Check your json file and try again."; 
    exit 0
  | UnknownAction -> 
    print_endline "Invalid action. Check your json file and try again."; 
    exit 0
  | _ -> 
    print_endline "Deck made successfully."; 
    parse_list json

(** [swap_hands_state state new_players new_top prev_discard] is the state
    during a swap hands play. *)
let swap_hands_state state new_players new_top prev_discard =
  let curr_player = get_curr state in
  let next = next_player state 1 in
  let hand = get_hand curr_player in
  let players = swap_hands_helper state new_players curr_player hand in
  update_state players next (get_deck state) new_top prev_discard

(** [end_of_game state] displays the screen for end of the game. *)
let end_of_game state =
  read_podium state;
  rank_players state;
  wait_for_event ();
  draw_play_again ()

(** [helper state] handles the turn by turn actions of the game. *)
let rec helper state =
  if endgame state then 
    (end_of_game state;
     play_again ())
  else
    let players = get_players state in 
    let curr_player = get_curr state in
    let hand = get_hand curr_player in 
    draw_state state Masked;
    wait_for_event ();
    draw_state state Unmasked; 
    let com_phrase = loops state None in
    match parse_command (com_phrase) with
    | exception Malformed -> helper state
    | exception Empty -> helper state
    | Draw -> draw_logic state players curr_player hand
    | Play card -> begin
        try( check_cards state players card )
        with 
        | Malformed -> helper state
      end

(** [draw_logic state players curr_player hand] handles the logic if the 
    player chooses to draw a card from the deck. *)
and draw_logic state players curr_player hand = 
  try (
    let new_state = drawcard_helper state players curr_player hand in
    helper new_state
  )
  with 
    LargerThanDeck -> 
    let reshuffled_state = overdraw_helper state in
    let players = get_players reshuffled_state in
    let current = get_curr reshuffled_state in
    let hand = get_hand current in
    let new_state = drawcard_helper reshuffled_state players 
        current hand in
    helper new_state

(** [check_cards state players card] checks whether the card [card] is in the
    player's hand and whether it is a valid play. If so, continues, otherwise,
    prompts player to choose again.  *)
and check_cards state players card =
  let top_card = get_top state in
  let play_card = phrase_to_card card in
  let prev_discard = get_discard state in
  if not (has_card (get_curr state) play_card) then
    (print_endline "This card is not in your hand. Please try again."; 
     helper state)
  else
    (if is_valid play_card top_card then
       let new_top = play_card in
       let new_player = remove_card (get_curr state) play_card in
       let new_players = replace new_player players in
       let state = update_state new_players new_player (get_deck state) 
           new_top prev_discard in
       card_logic play_card new_players new_top prev_discard state
     else
       (draw_invalid_place ();
        wait_for_event ();
        helper state))

(** [card_logic play_card new_players new_top prev_discard state] handles
    the logic for the different card types. *)
and card_logic play_card new_players new_top prev_discard state = 
  match get_action play_card with
  | NoneA -> helper (number_helper new_players state new_top prev_discard)
  | Skip -> helper (skip_helper new_players state new_top prev_discard)
  | DrawTwo -> 
    helper (draw_helper new_players state new_top 2 prev_discard)
  | Reverse ->
    if List.length new_players = 2 then
      helper (skip_helper new_players state new_top prev_discard)
    else helper (reverse_helper new_players state new_top prev_discard) 
  | WildCard -> 
    helper 
      (wild_helper new_players state 1 play_card prev_discard WildColor)
  | DrawFour -> 
    let draw_state = draw_helper new_players state new_top 4 prev_discard in
    let wild_state = wild_helper (get_players draw_state) draw_state 0 
        (get_top draw_state) (get_discard draw_state) DrawFourColor in 
    helper wild_state
  | SwapHands ->
    let state = swap_hands_state state new_players new_top prev_discard in
    helper state

(** [play_again ()] draws the play again screen. If "YES" is clicked, restart
    the game. Otherwise, exits. *)
and play_again () = 
  let event = Graphics.wait_next_event [Graphics.Button_down] in
  let x = event.Graphics.mouse_x in
  let y = event.Graphics.mouse_y in
  if event.Graphics.button && is_click x y (150, 300, 250, 100)
  then play_game "playing_cards.json"
  else if event.Graphics.button && is_click x y (600, 300, 250, 100)
  then ()
  else play_again ()

(** [play_game f] starts the game with json file [f].
    Requires: [f] is a valid json file. *)
and play_game f =
  let j = Yojson.Basic.from_file f in
  let deck = deck_builder j in
  draw_cards_notice ();
  wait_for_event ();
  let shuffled_deck = shuffle deck in
  draw_player_screen ();
  let num_players = choose_players (player_selection ()) in
  let make = init_players shuffled_deck num_players in
  let ordered_players = sort_players (fst make) in
  let top_card = deal (snd make) 1 in
  let initial_state = update_state (ordered_players) 
      (List.hd ordered_players) (snd top_card) (List.hd (fst top_card)) 
      (fst top_card) in
  print_players ordered_players;
  instructions_helper ();
  draw_state initial_state Masked;
  helper initial_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  Graphics.open_graph " 1000x750";
  draw_welcome_screen ();
  wait_for_event ();
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Uno!\n");
  play_game "playing_cards.json"

(* Execute the game engine. *)
let () = main ()