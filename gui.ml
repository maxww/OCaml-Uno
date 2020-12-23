open Card
open Player
open State
open Graphics
open Camlimages
open Images

(** Source: CamlImages library.
    https://github.com/jrk/camlimages *)
let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
    let w = bitmap.Index8.width
    and h = bitmap.Index8.height
    and colormap = bitmap.Index8.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) 
        colormap in
    if bitmap.Index8.transparent <> -1 then
      cmap.(bitmap.Index8.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
    let w = bitmap.Index16.width
    and h = bitmap.Index16.height
    and colormap = bitmap.Index16.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
    if bitmap.Index16.transparent <> -1 then
      cmap.(bitmap.Index16.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
    let w = bitmap.Rgb24.width
    and h = bitmap.Rgb24.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
            rgb r g b))
  | Rgba32 _ -> failwith "RGBA not supported"
  | Cmyk32 _ -> failwith "CMYK not supported"

(** Source: CamlImages library.
    https://github.com/jrk/camlimages *)
let of_image img = make_image (array_of_image img)

(** [load_image link] is the image of type [Graphics.image] from the link 
    [link]. *)
let load_image link = of_image (Jpeg.load link [])

(** [get_link card folder] is the link to the image of a particular [card] 
    in folder [folder]. *)
let get_link card folder =
  match get_value card with
  | NoneV -> 
    let color = color_to_string (get_color card) in
    let action = action_to_string (get_action card) in
    "images/" ^ folder ^ "/" ^ color ^ "_" ^ action ^ ".jpeg"
  | _ -> 
    let color = color_to_string (get_color card) in
    let value = value_to_string (get_value card) in
    "images/" ^ folder ^ "/" ^ color ^ "_" ^ value ^ ".jpeg"

(** [hand_to_image hand] is the list of card images in the list of cards 
    [hand]. *)
let hand_to_image hand =
  let link_hand = List.map (fun x -> get_link x "cards") hand in
  List.map (fun x -> load_image x) link_hand

(** The type [player_position] represents the position of the players on the 
    GUI screen. *)
type player_position = 
  | Top 
  | Bottom 
  | Left 
  | Right 

(** [pos_to_str pos] is the string that represents each type of 
    player_position. *)
let pos_to_str pos =
  match pos with
  | Top -> "top"
  | Bottom -> "bottom"
  | Left -> "left"
  | Right -> "right"

(** [masked_cards hand pos] is the masked version of the cards in hand [hand]
    that can be displayed on the screen. The position [pos] determines the
    image that is fetched. *)
let masked_cards hand pos = 
  let place = pos_to_str pos in
  let link_hand = 
    List.map (fun x -> ("images/cards/masked_" ^ place ^ ".jpeg")) hand in
  List.map (fun x -> load_image x) link_hand

(** [draw_hand hand x y offset] draws the hand [hand] on the screen at position
    ([x, y]). The offset [offset] denotes the separation of the cards. *)
let rec draw_hand hand x y offset =
  match hand with
  | [] -> ()
  | h :: t -> 
    draw_image h x y;
    draw_hand t (x + offset) y offset

(** [draw_hand_side hand x y offset] draws the hand [hand] vertically on the 
    screen at position ([x, y]). The offset [offset] denotes the separation 
    of the cards. *)
let rec draw_hand_side hand x y offset =
  match hand with
  | [] -> ()
  | h :: t ->
    draw_image h x y;
    draw_hand_side t x (y + offset) offset

(** [center_x n_cards x_total] is the x-coordinate where the hand with 
    [n_cards] cards would be centered on a screen of length [x_total]. *)
let center_x n_cards x_total =
  let center = x_total / 2 in
  match n_cards with
  | k when k mod 2 = 1 -> 
    center - (n_cards/2) * 77 - 38
  | _ ->
    center - (n_cards / 2) * 77

(** [text_name name y] draws the name "name" on the screen at y-coordinate
    [y]. *)
let text_name name y =
  let len = String.length name in
  let x_coord = 500 - (len / 2) * 25 in 
  moveto x_coord y;
  set_color white;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  draw_string name

(** [text_name_vert name x] draws the name "name" vertically on the screen 
    at x-coordinate [x]. *)
let text_name_vert name x =
  moveto x 350;
  set_color white;
  set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
  draw_string name

(** [two_players] draws the state [state] with two total players on the 
    screen. *)
let two_players state =
  let next = next_player state 1 in
  let next_hand = get_hand next in
  let mask = masked_cards next_hand Top in
  let n_cards = List.length mask in
  let offset = (77 * 7) / n_cards in
  text_name (Player.get_name next) 580;
  if n_cards >= 7 then
    draw_hand mask (center_x 7 1000) 640 offset
  else 
    draw_hand mask (center_x n_cards 1000) 640 77

(** [three_players_top state] draws the top player of state [state] with 
    three total players on the screen. *)
let three_players_top state =
  let next2 = next_player state 2 in
  let next_hand2 = get_hand next2 in
  let mask_top = masked_cards next_hand2 Top in
  let n_cards2 = List.length mask_top in
  let offset2 = (77 * 7) / n_cards2 in
  text_name (get_name next2) 580;
  if n_cards2 >= 7 then
    draw_hand mask_top (center_x 7 1000) 640 offset2
  else 
    draw_hand mask_top (center_x n_cards2 1000) 640 77

(** [three_players state] draws the state [state] with three total 
    players on the screen. *)
let three_players state =
  let next = next_player state 1 in
  let next_hand = get_hand next in
  let mask_right = masked_cards next_hand Right in
  let n_cards = List.length mask_right in
  let offset = (77 * 7) / n_cards in

  let name = get_name next in
  let len = (String.length name) * 25 in
  text_name_vert (get_name next) (880 - len); 

  if n_cards >= 7 then
    draw_hand_side mask_right 890 (center_x 7 750) offset
  else
    draw_hand_side mask_right 890 (center_x n_cards 750) 77;

  three_players_top state

(** [four_players state] draws the state [state] with four total players on 
    the screen. *)
let four_players state =
  three_players state;

  let next3 = next_player state 3 in
  let next_hand3 = get_hand next3 in
  let mask_left = masked_cards next_hand3 Left in
  let n_cards3 = List.length mask_left in
  let offset3 = (77 * 7) / n_cards3 in
  text_name_vert (get_name next3) 120;
  if n_cards3 >= 7 then
    draw_hand_side mask_left 0 (center_x 7 750) offset3
  else
    draw_hand_side mask_left 0 (center_x n_cards3 750) 77

(** [n_players_helper n_players state] is the helper function that matches
    the number of players [n_players] to the appropriate helper function to
    draw the state on the screen. *)
let n_players_helper n_players state = 
  match n_players with
  | 2 -> two_players state
  | 3 -> three_players state
  | 4 -> four_players state
  | _ -> ()

type masked = 
  | Masked
  | Unmasked

(** [masked_helper state players current_hand] draws the masked version of 
    the current player's hand [current_hand] on the screen. *)
let masked_helper state players current_hand =
  let masks = masked_cards current_hand Bottom in
  let n_cards = List.length current_hand in
  let offset = (77 * 7) / n_cards in
  if n_cards >= 7 then 
    (draw_hand masks (center_x 7 1000) 0 offset;
     n_players_helper (List.length players) state)
  else 
    (draw_hand masks (center_x n_cards 1000) 0 77;
     n_players_helper (List.length players) state)

(** [masked_helper state players current_hand] draws the unmasked version of 
    the current player's hand [current_hand] on the screen. *)
let unmasked_helper state players current_hand = 
  let n_cards = List.length current_hand in
  let offset = (77 * 7) / n_cards in
  if n_cards >= 7 then
    (draw_hand current_hand (center_x 7 1000) 0 offset;
     n_players_helper (List.length players) state)
  else
    (draw_hand current_hand (center_x n_cards 1000) 0 77;
     n_players_helper (List.length players) state)

let draw_state state masked =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;

  let players = get_players state in
  let current = get_curr state in
  let current_hand = hand_to_image (get_hand current) in
  let top_card = state |> get_top in
  let tc = load_image (get_link top_card "cards") in
  text_name (get_name current) 120;
  draw_image tc 510 320;
  let deck = load_image "images/cards/masked_bottom.jpeg" in
  draw_image deck 413 320;
  match masked with
  | Masked ->
    masked_helper state players current_hand
  | Unmasked ->
    unmasked_helper state players current_hand

let is_click click_x click_y (x, y, l, h) =
  click_x >= x && click_x < x + l && click_y >= y && click_y < y + h

(** [window_list n_cards c offset acc counter] makes a list of tuples which
    bound the rectangle (x, y, l, h) depending on the number of cards. *)
let rec window_list n_cards c offset acc counter =
  match n_cards with
  | 0 -> acc
  | _ ->
    let float_cards = float_of_int c in
    if c <= 7 then
      let initial_x = int_of_float (500.0 -. float_cards /. 2.0 *. 77.0) + 1 in
      let x_new = initial_x + offset * counter in
      window_list (n_cards - 1) c offset ((x_new, 0, 77, 110) :: acc) 
        (counter + 1)
    else 
      let x_new = 231 + offset * counter in
      window_list (n_cards - 1) c offset ((x_new, 0, 77, 110) :: acc) 
        (counter + 1)

(** [match_cards (x, y) window_list hand count] checks whether a card in hand
    [hand] has beeen clicked. If so, returns that card as an option. *)
let rec match_cards (x, y) window_list hand count = 
  match window_list with
  | [] -> None
  | h :: t ->
    try (
      if is_click x y h 
      then Some (List.nth hand count)
      else match_cards (x, y) t hand (count + 1)  
    )
    with 
    | _ -> None

let clicked_discard (x, y) =
  is_click x y (510, 320, 77, 110)

let clicked_hand (x, y) =
  is_click x y (230, 0, 539, 110)

let clicked_draw (x, y) =
  is_click x y (413, 320, 77, 110)

let clicked_two_players (x, y) =
  is_click x y (65, 250, 200, 200)

let clicked_three_players (x, y) =
  is_click x y (365, 250, 200, 200)

let clicked_four_players (x, y) =
  is_click x y (665, 250, 260, 200)

let get_card_gui (x, y) state = 
  let current = get_curr state in
  let hand = get_hand current in
  let n_cards = List.length hand in
  if n_cards >= 7 then
    let window = (77 * 7) / n_cards in
    let window_lst = List.rev (window_list n_cards n_cards window [] 0) in
    match_cards (x, y) window_lst hand 0
  else
    let window_lst = List.rev (window_list n_cards n_cards 77 [] 0) in
    match_cards (x, y) window_lst hand 0

(** [hand_to_selected_image hand card acc] makes a list of images with the
    selected versions of the images in hand [hand]. *)
let rec hand_to_selected_image hand card acc = 
  match hand with
  | [] -> acc
  | h :: t ->
    if cards_equal h card then
      let link = get_link card "selected_cards" in 
      let img = load_image link in
      hand_to_selected_image t card (img :: acc)
    else 
      let link = get_link h "cards" in
      let img = load_image link in
      hand_to_selected_image t card (img :: acc)

let draw_selected_state card state =
  set_color black;
  fill_rect 0 0 1000 750;

  let players = get_players state in
  let current = get_curr state in
  let current_hand = List.rev (hand_to_selected_image (get_hand current) 
                                 card []) in
  let top_card = state |> get_top in
  let tc = load_image (get_link top_card "cards") in
  text_name (Player.get_name current) 120;
  draw_image tc 510 320;
  let deck = load_image "images/cards/masked_bottom.jpeg" in
  draw_image deck 413 320;
  unmasked_helper state players current_hand

(** [center_text text size] returns the x-coordinate required to center text
    [text] of size [size] on the screen. *)
let center_text text size =
  let len = String.length text in
  500 - ( len / 2 * size / 2)

let text_display text font x y = 
  moveto x y;
  set_font font;
  draw_string text

let draw_instructions_screen () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;

  let title_font = "-*-fixed-bold-r-semicondensed--50-*-*-*-*-*-iso8859-1" in
  let reg_font = "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1" in

  set_color white;
  let title = "Instructions" in
  text_display title title_font (center_text title 50) 600;

  let text = "There are two commands - play and draw" in
  text_display text reg_font (center_text text 30) 500;

  let text = "You can choose to play any card in your hand by" in
  text_display text reg_font (center_text text 30) 450;

  set_color red;
  let text = "clicking a card" in
  text_display text reg_font (center_text text 30) 400;

  set_color white;
  let text = "You can choose to draw instead by" in
  text_display text reg_font (center_text text 30) 350;

  set_color red;
  let text = "clicking the discard pile" in
  text_display text reg_font (center_text text 30) 300;

  set_color white;
  let text = "[Press Enter to continue]" in
  text_display text reg_font (center_text text 30) 250

let draw_invalid_place () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;
  let title_font = "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1" in
  set_color red;
  let title = "THIS CARD CANNOT BE PLACED HERE" in
  text_display title title_font (center_text title 50) 500;

  let title = "TRY AGAIN" in
  text_display title title_font (center_text title 50) 400;

  set_color white;
  let title = "[PRESS ANY KEY TO CONTINUE]" in
  text_display title title_font (center_text title 50) 300

let draw_welcome_screen () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;

  let title_font = "-*-fixed-bold-r-normal--50-*-*-*-*-*-iso8859-1" in

  set_color red;
  let title = "WELCOME" in
  text_display title title_font (center_text title 60) 600;

  set_color green;
  let title = "TO" in
  text_display title title_font (center_text title 60) 500;

  set_color blue;
  let title = "OCAML" in
  text_display title title_font (center_text title 60) 400;

  set_color yellow;
  let title = "UNO" in
  text_display title title_font (center_text title 60) 300

let draw_player_screen () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;

  let title_font = "-*-fixed-bold-r-normal--50-*-*-*-*-*-iso8859-1" in
  let reg_font = "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1" in
  set_color red;
  let title = "NUMBER OF PLAYERS?" in
  text_display title title_font (center_text title 60) 600;
  let two_players = load_image "images/players/two_players.jpeg" in
  draw_image two_players 65 250;
  text_display "2 Players" reg_font 100 200;
  let three_players = load_image "images/players/three_players.jpeg" in
  draw_image three_players 365 250;
  text_display "3 Players" reg_font 400 200;
  let four_players = load_image "images/players/four_players.jpeg" in
  draw_image four_players 665 250;
  text_display "4 Players" reg_font 730 200

let draw_player_name_screen num_of_players =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;
  let title_font = "-*-fixed-bold-r-normal--50-*-*-*-*-*-iso8859-1" in
  set_color red;
  let title = "PLAYER NAMES?" in
  text_display title title_font (center_text title 60) 600;
  match num_of_players with
  | 2 ->  text_display "Player 1" title_font 100 400;
    text_display "Player 2" title_font 100 300;
  | 3 ->  text_display "Player 1" title_font 100 400;
    text_display "Player 2" title_font 100 300;
    text_display "Player 3" title_font 100 200;
  | 4 ->  text_display "Player 1" title_font 100 400;
    text_display "Player 2" title_font 100 300;
    text_display "Player 3" title_font 100 200;
    text_display "Player 4" title_font 100 100;
  | _ -> ()

(** [square x] returns the square of float [x]. *)
let square x = x *. x

(** [distance x1 y1 x2 y2] returns the distance from point [(x1, y1)] to
    point [(x2, y2)]. *)
let distance x1 y1 x2 y2 =
  let x_diff = square (x2 -. x1) in
  let y_diff = square (y2 -. y1) in
  sqrt (x_diff +. y_diff)

(** The type [quarter] represents the type of quarter of the circle that
    is being clicked. *)
type quarter = 
  | First
  | Second
  | Third
  | Fourth
  | Outside

(** [is_click_quarter click_x click_y (x, y, r)] returns the quarter of type
    [quarter] that [(click_x, click_y)] lands on for circle with radius [r] and 
    center [(x, y)].  *)
let is_click_quarter click_x click_y (x, y, r) = 
  if distance click_x click_y x y <= r
  && click_y >= y && click_x >= x
  then First
  else if distance click_x click_y x y <= r
       && click_y <= y && click_x >= x
  then Second
  else if distance click_x click_y x y <= r
       && click_y <= y && click_x <= x
  then Third
  else if distance click_x click_y x y <= r
       && click_y >= y && click_x <= x
  then Fourth
  else Outside

let clicked_blue (x, y) =
  is_click_quarter x y (500.0, 375.0, 110.0) = First

let clicked_green (x, y) =
  is_click_quarter x y (500.0, 375.0, 110.0) = Second

let clicked_yellow (x, y) =
  is_click_quarter x y (500.0, 375.0, 110.0) = Third

let clicked_red (x, y) =
  is_click_quarter x y (500.0, 375.0, 110.0) = Fourth

let draw_color_selection_screen () = 
  set_color blue;
  fill_arc 500 375 110 110 0 90;
  set_color red;
  fill_arc 500 375 110 110 90 180;
  set_color yellow;
  fill_arc 500 375 110 110 180 270;
  set_color green;
  fill_arc 500 375 110 110 270 360

let draw_cards_notice () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;

  let title_font = "-*-fixed-bold-r-normal--50-*-*-*-*-*-iso8859-1" in
  let reg_font = "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1" in

  set_color red;
  let title = "NOTICE" in
  text_display title title_font (center_text title 60) 600;

  set_color white;
  let text = "You can choose to customize your Uno Experience!" in
  text_display text reg_font (center_text text 30) 500;

  let text = "Just modify the playing_cards.json file!" in
  text_display text reg_font (center_text text 30) 450;

  let text = "[PRESS ANY KEY TO CONTINUE]" in
  text_display text reg_font (center_text text 30) 300


(** [third_and_fourth third fourth title_font] is a helper function to draw 
    the winner screen. [third] and [fourth] are string options representing the
    names of the third and fourth players, if they exist.*)
let third_and_fourth third fourth title_font =
  match third, fourth with
  | None, None -> ()
  | Some s, None ->
    let title = "#3 - " ^ s in
    text_display title title_font (center_text title 60) 300
  | Some s, Some s2 ->
    let title = "#3 - " ^ s in
    text_display title title_font (center_text title 60) 300;
    let title = "#4 - " ^ s2 in
    text_display title title_font (center_text title 60) 200
  | _ -> ()

let draw_invalid_player_name () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;
  let title_font = "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1" in
  set_color red;
  let title = "THIS PLAYER NAME DOES NOT EXIST" in
  text_display title title_font (center_text title 50) 500;

  let title = "TRY AGAIN" in
  text_display title title_font (center_text title 50) 400;

  set_color white;
  let title = "[PRESS ANY KEY TO CONTINUE]" in
  text_display title title_font (center_text title 50) 300

let draw_swap_hands () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;
  let title_font = "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1" in
  set_color red;
  let title = "SWAP HANDS" in
  text_display title title_font (center_text title 50) 600;

  let title = "TYPE THE PLAYER'S NAME" in
  text_display title title_font (center_text title 50) 500;

  let title = "YOU WOULD LIKE TO SWITCH HANDS WITH" in
  text_display title title_font (center_text title 50) 400


let draw_winner_screen first second third fourth =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750; 

  let title_font = "-*-fixed-bold-r-normal--50-*-*-*-*-*-iso8859-1" in

  set_color white;
  let title = "RESULTS" in
  text_display title title_font (center_text title 60) 600;

  let title = "#1 - " ^ first in
  text_display title title_font (center_text title 60) 500;

  let title = "#2 - " ^ second in
  text_display title title_font (center_text title 60) 400;

  third_and_fourth third fourth title_font


(** [draw_button x y l h] draws a rectangular button on the screen with 
    lower left corner [(x, y)], length [l] and height [h]. *)
let draw_button x y l h =
  moveto x y;
  set_color white;
  draw_rect x y l h

let draw_play_again () =
  clear_graph ();
  set_color black;
  fill_rect 0 0 1000 750;

  let title_font = "-*-fixed-bold-r-normal--50-*-*-*-*-*-iso8859-1" in

  set_color white;
  let title = "PLAY AGAIN" in
  text_display title title_font (center_text title 60) 600;

  draw_button 150 300 250 100;
  text_display "YES" title_font 220 320;

  draw_button 600 300 250 100;
  text_display "NO" title_font 690 320





