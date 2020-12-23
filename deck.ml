open Card
open Yojson.Basic.Util

exception UnknownColor
exception UnknownValue
exception UnknownAction

exception LargerThanDeck

let string_to_color str =
  match str with
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | "yellow" -> Yellow
  | "wild" -> Wild
  | _ -> raise(UnknownColor)

let string_to_value str = 
  match str with
  | "0" -> Zero
  | "1" -> One
  | "2" -> Two
  | "3" -> Three
  | "4" -> Four
  | "5" -> Five
  | "6" -> Six
  | "7" -> Seven
  | "8" -> Eight
  | "9" -> Nine
  | "none" -> NoneV
  | _ -> raise(UnknownValue)

let string_to_action str = 
  match str with
  | "skip" -> Skip
  | "reverse" -> Reverse
  | "drawtwo" -> DrawTwo
  | "drawfour" -> DrawFour
  | "swaphands" -> SwapHands
  | "wildcard" -> WildCard
  | "none" -> NoneA
  | _ -> raise(UnknownAction)

let parse_list j =
  let json_lst = j |> to_list in
  let lst = [] in
  let rec parse lst json_lst =
    match json_lst with
    | [] -> lst
    | h :: t -> 
      let color = h |> member "color" |> to_string |> string_to_color in
      let value = h |> member "value" |> to_string |> string_to_value in
      let action = h |> member "action" |> to_string |> string_to_action in
      parse ((make_card color value action) :: lst) t
  in
  parse lst json_lst 

let shuffle lst =
  Random.self_init ();
  let new_lst = [] in
  let rec helper lst new_lst =
    match lst with
    | [] -> new_lst
    | h :: t ->
      let n = Random.int (List.length lst) in
      let ele = List.nth lst n in
      let rm_ele = List.filter (fun x -> x <> ele) lst in
      helper rm_ele (ele :: new_lst)
  in
  helper lst new_lst

let deal deck n_card =
  let hand = [] in
  let rec helper deck n_card hand =
    match deck with
    | [] -> raise LargerThanDeck
    | h :: t -> begin
        if n_card = 0 then (hand, deck)
        else helper t (n_card - 1) (h :: hand)
      end
  in
  helper deck n_card hand

let draw 
    (deck : Card.t list) 
    (n_card : int ) 
    (hand : Card.t list) : Card.t list * Card.t list = 
  let dealt = deal deck n_card in
  (hand @ fst dealt, snd dealt)
