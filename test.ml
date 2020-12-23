(*Test Plan~ 

  Our testing approached used was Glass Box Testing Approach. We
  took into account the implementation as well as the expected results of the 
  functions we tested. We tested by making sure all our internal functions were 
  working. The card actions and gameplay functions like deal and draw were 
  tested through gameplay.
  Tested Manually:
  - GUI
  - Main
  - Deck
    Tested with OUnit:
  - State
  - Card
  - Command
  - Player

    Our testing approach shows the correctness of our system because we covered 
    all of the functions we are using either by testing manually or by using
    OUnit, and ensured that they all evaluated as expected.
*)


open OUnit2
open Player
open Card
open Deck
open Command
open State

let j = Yojson.Basic.from_file "playing_cards.json"
let cards = parse_list j

let blue9 = make_card Blue Nine NoneA
let red8 = make_card Red Eight NoneA
let green5 = make_card Green Five NoneA
let yellow1 = make_card Yellow One NoneA
let wildcard = make_card Wild NoneV WildCard
let blueskip = make_card Blue NoneV Skip
let red9 = make_card Red Nine NoneA
let blue5 = make_card Blue Five NoneA
let red6 = make_card Red Six NoneA
let green9 = make_card Green Nine NoneA
let yellow3 = make_card Yellow Three NoneA
let draw4 = make_card Wild NoneV DrawFour
let green_drawtwo = make_card Green NoneV DrawTwo
let red_reverse = make_card Red Nine Reverse

let hand1 = [blue9; red8; green5; yellow1; wildcard; blueskip; red6]
let hand2 = [blue5; red8; green5; yellow1; wildcard; blueskip; red9]
let hand3 = [yellow3; red9; blueskip; green_drawtwo; red6; draw4; green5]
let hand4 = [red8; red_reverse; wildcard; blue5; yellow3; blueskip; red6]
let hand5 = [red8; green5; yellow1; wildcard; blueskip; red6]
let hand6 = [blue9; red8; yellow1; wildcard; blueskip; red6]
let hand7 = [yellow3;red9;green_drawtwo;red6;draw4;green5]

let get_card_test 
    (name : string) 
    funct 
    (card : Card.t) 
    expected_output : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (funct card))

let make_card_test 
    (name : string) 
    (color : color) 
    (value : value) 
    (action : action)  
    (expected_output : Card.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (make_card color value action))

let card_tests = 
  [
    get_card_test "get color" get_color blue5 Blue;
    get_card_test "get value" get_value blue5 Five;
    get_card_test "get action" get_action blueskip Skip;
    get_card_test "get color of blue5" get_color blue5 Blue;
    get_card_test "get color of green9" get_color green9 Green;
    get_card_test "get value of blue5" get_value blue5 Five;
    get_card_test "get value of green9" get_value green9 Nine;
    get_card_test "get action of blueskip" get_action blueskip Skip;
    get_card_test "get action of draw4" get_action draw4 DrawFour;
    make_card_test "make blue9" Blue Nine NoneA blue9;
    make_card_test "make a wild card" Wild NoneV WildCard wildcard;
  ]

let alex = make_player "alex" 1 hand1
let alex2 = make_player "alex" 2 hand5
let alex3 = make_player "alex" 1 hand2
let norma = make_player "norma" 4 hand3
let penny = make_player "penny" 3 hand4
let jack = make_player "jack" 2 hand1

let compare_player_lst lst1 lst2 =
  let unique1 = List.sort_uniq (Player.compare_players) lst1 in
  let unique2 = List.sort_uniq (Player.compare_players) lst2 in
  let rec helper lst1 lst2 = 
    match lst1, lst2 with
    | [], [] -> true
    | (h1 :: t1), (h2 :: t2) ->
      if not (Player.players_equal h1 h2) then false
      else helper t1 t2
    | _ -> failwith "Not same size"
  in helper unique1 unique2

let get_test 
    (name : string) 
    funct 
    (player: Player.t) 
    expected_output : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (funct player))

let make_player_test 
    (name : string)
    (name : string)
    (player_pos: int) 
    (hand : Card.t list) 
    (expected_output : Player.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (make_player name player_pos hand))

let set_pos_test 
    (name : string)
    (player : Player.t)
    (index : int) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_pos (set_pos player index)))

let compare_players_test
    (name : string)
    (player1 : Player.t)
    (player2 : Player.t) 
    (expected_output : int): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (compare_players player1 player2))

let sort_players_test 
    (name : string)
    (player_list : Player.t list) 
    (expected_output : Player.t list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (sort_players player_list))

let cards_equal_test 
    (name : string)
    (c1 : Card.t) 
    (c2 : Card.t)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (cards_equal c1 c2))

let has_card_test 
    (name : string)
    (player : Player.t)
    (card : Card.t)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (has_card player card)) 

let get_pos_from_name 
    (namet : string)
    (name : string)
    (players : Player.t list)
    (expected_output : int option) : test = 
  namet >:: (fun _ -> 
      assert_equal expected_output (get_pos_from_name name players)) 

let remove_card_test
    (name : string)
    (player: Player.t)
    (card : Card.t )
    (expected_output : Card.t list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_hand (remove_card player card)))

let replace_test 
    (name : string)
    (player: Player.t)
    (player_list : Player.t list)
    (expected_output : Player.t list) : test =                    
  name >:: (fun _ -> 
      assert_equal expected_output (replace player player_list)
        ~cmp: compare_player_lst)

let player_tests = [
  get_test "get position test" get_pos alex 1;
  get_test "get hand test" get_hand alex hand1;
  get_test "get name test" get_name alex "alex";
  make_player_test "make player alex" "alex" 1 hand1 alex;
  set_pos_test "change alex from 1st to 2nd position" alex 2 2;
  set_pos_test "change norma from 4th to 3rd position" norma 3 3;
  set_pos_test "change penny from 3rd to 1st position" penny 1 1;
  compare_players_test "compare alex and norma" alex norma (-1);
  compare_players_test "compare alex2 and alex" alex2 alex 1;
  compare_players_test "compare alex and alex" alex alex 0;
  sort_players_test "sort alex alex2 and norma" [alex2; alex; norma] 
    [alex; alex2; norma];
  sort_players_test "sort alex alex2 and norma" [alex2;alex;norma] 
    [alex;alex2;norma];
  sort_players_test "sort alex penny and jack" [jack;alex;penny]
    [alex;jack;penny];
  sort_players_test "sort alex norma penny and jack" [norma;penny;jack;alex]
    [alex;jack;penny;norma];
  cards_equal_test "red9 and yellow3" red9 yellow3 false;
  cards_equal_test "two equal cards" blue9 blue9 true;
  cards_equal_test "blue5 and blue9" blue9 blue5 false;
  cards_equal_test "two equal cards" blue9 blue9 true;
  cards_equal_test "two unequal cards" blue9 yellow1 false;
  has_card_test "card is in hand" alex blue9 true;
  has_card_test "card is not in hand" alex blue5 false;
  has_card_test "jack has green5" jack green5 true;
  has_card_test "jack does not have draw4" jack draw4 false;
  get_pos_from_name "norma's position in player list" "norma" 
    [alex; alex2; norma] (Some 4);
  get_pos_from_name "name not in player list" "norma" [alex; alex2] None;
  remove_card_test "remove blue9 from alex" alex blue9 hand5;
  remove_card_test "remove green5 from jack" jack green5 hand6;
  remove_card_test "remove blueskip from norma" norma blueskip hand7;
  replace_test "replace alex2 with alex" alex3 [alex; norma] [alex3; norma];
]

let parse_command_test
    (name : string)
    (str : string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse_command str))

let phrase_to_card_test
    (name : string)
    (phrase : string list) 
    (expected_output : Card.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (phrase_to_card phrase)
        ~cmp: cards_equal)

let command_tests = 
  [
    parse_command_test "draw returns draw" "draw" Draw;
    parse_command_test "plays red one" "play red 1" (Play ["red"; "1"]);
    parse_command_test "spaces don't matter" "   play  red    3" 
      (Play ["red"; "3"]);
    phrase_to_card_test "numbered card" ["red"; "9"] red9;
    phrase_to_card_test "wildcard" ["wildcard"] wildcard;
    phrase_to_card_test "drawfour" ["drawfour"] 
      (make_card Wild NoneV DrawFour);
    phrase_to_card_test "color action card" ["green"; "drawtwo"] green_drawtwo
  ]

let state1 = update_state [alex; penny; jack; norma] alex hand3 blue9 hand1
let state2 = update_state [alex;penny;jack;norma] jack hand3 blue9 hand1
let state3 = update_state [alex;penny;jack;norma] penny hand3 blue9 hand1

let update_test
    (name : string)
    (players : Player.t list) 
    (current : Player.t)
    (deck : Card.t list)
    (top_card : Card.t)
    (discard_pile : Card.t list)
    (expected_output : state) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (update_state players current deck 
                                      top_card discard_pile))

let get_state_test 
    (name : string) 
    funct 
    (state : state) 
    expected_output : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (funct state))

let valid_test
    (name : string)
    (c1 : Card.t) 
    (c2 : Card.t)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_valid c1 c2))

let next_player_test 
    (name : string)
    (state: state) 
    (turns: int)
    (expected_output : Player.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (next_player state turns))

let state_tests = 
  [
    update_test "make state1" [alex;penny;jack;norma] alex hand3 blue9 hand1 
      state1;
    get_state_test "get players test" get_players state1 
      [alex;penny;jack;norma];
    get_state_test "get current player state1" get_curr state1 alex;
    get_state_test "get current player state2" get_curr state2 jack;
    get_state_test "get current player state3" get_curr state3 penny;
    get_state_test "get deck test" get_deck state1 hand3;
    get_state_test "get top card test" get_top state1 blue9;
    get_state_test "get discard pile" get_discard state1 hand1;
    next_player_test "jack becomes next player" state1 1 jack;
    next_player_test "alex skips jack's turn" state1 2 penny;
    valid_test "Same color is valid" 
      (make_card Red One NoneA) (make_card Red Two NoneA) true;
    valid_test "Different color, different value is not valid" 
      (make_card Red One NoneA) (make_card Blue Two NoneA) false;
    valid_test "Different color, same value is valid" 
      (make_card Red Nine NoneA) (make_card Yellow Nine NoneA) true;
    valid_test "Same color numerical and special card is valid"
      (make_card Blue Eight NoneA) (make_card Blue NoneV Skip) true;
    valid_test "Different color numerical and special card is not valid"
      (make_card Blue Eight NoneA) (make_card Green NoneV Reverse) false;
    valid_test "Same color special and special card is valid"
      (make_card Blue NoneV Skip) (make_card Blue NoneV DrawTwo) true;
    valid_test "Same action, different color special card is valid"
      (make_card Blue NoneV Skip) (make_card Green NoneV Skip) true;
    valid_test "Different action, different color special card is not valid"
      (make_card Blue NoneV Skip) (make_card Green NoneV Reverse) false;
    valid_test "Wild card, numeric card is valid"
      (make_card Wild NoneV DrawFour) (make_card Green Seven NoneA) true;
    valid_test "Numeric card, wild card is valid"
      (make_card Yellow Six NoneA) (make_card Wild NoneV WildCard) true;
    valid_test "Wild card, wild card is valid"
      (make_card Wild NoneV SwapHands) (make_card Wild NoneV WildCard) true;
    valid_test "Wild card, wild card is valid"
      (make_card Wild NoneV DrawFour) (make_card Wild NoneV SwapHands) true;
    valid_test "Special card, wild card is valid"
      (make_card Yellow NoneV DrawTwo) (make_card Wild NoneV WildCard) true;
    valid_test "Wild card, special card is valid"
      (make_card Wild NoneV DrawFour) (make_card Red NoneV DrawTwo) true;
  ]

let suite =
  "test suite for OCaml-Uno"  >::: List.flatten [
    state_tests;
    command_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite