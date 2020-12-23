open Yojson.Basic.Util

type color = 
  | Red 
  | Green 
  | Yellow 
  | Blue 
  | Wild 

type value = 
  | Zero 
  | One 
  | Two 
  | Three 
  | Four 
  | Five 
  | Six 
  | Seven 
  | Eight 
  | Nine 
  | WildColor
  | DrawFourColor 
  | NoneV 

type action = 
  | Skip 
  | Reverse 
  | DrawTwo 
  | DrawFour 
  | WildCard 
  | SwapHands 
  | NoneA

type t = {
  color : color;
  value : value;
  action : action;
}

let get_color (card : t) : color = 
  card.color

let get_value (card : t) : value = 
  card.value

let get_action (card : t): action = 
  card.action

let make_card color value action = {
  color = color;
  value = value;
  action = action;
}

let color_to_string color =
  match color with
  | Red -> "red"
  | Blue -> "blue"
  | Green -> "green"
  | Yellow -> "yellow"
  | Wild -> "wild"

let value_to_string value = 
  match value with
  | Zero -> "0"
  | One -> "1"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | WildColor -> "wildcolor"
  | DrawFourColor -> "dfcolor"
  | NoneV -> ""

let action_to_string action = 
  match action with
  | Skip -> "skip"
  | Reverse -> "reverse"
  | DrawTwo -> "drawtwo"
  | DrawFour -> "drawfour"
  | SwapHands -> "swaphands"
  | WildCard -> "wildcard"
  | NoneA -> ""

let card_to_string card = 
  color_to_string (get_color card) ^ " " ^ value_to_string (get_value card) 
  ^ " " ^ action_to_string (get_action card)

let card_score card = 
  match get_action card with
  | Skip -> 4
  | Reverse -> 4
  | DrawTwo -> 6
  | DrawFour -> 0
  | SwapHands -> 0
  | WildCard -> 0
  | NoneA -> (card |> get_value |> value_to_string |> int_of_string)    