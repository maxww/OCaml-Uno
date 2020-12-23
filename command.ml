open Card
open Deck

type card_phrase = string list

type command = 
  | Play of card_phrase
  | Draw

exception Empty
exception Malformed

let parse_command str = 
  let lst = String.split_on_char ' ' str in
  let filtered_lst = List.filter (fun x -> x <> "") lst in
  match filtered_lst with
  | [] -> raise Empty
  | ["play"] -> raise Malformed
  | ["draw"] -> Draw
  | "play" :: t -> Play t
  | "draw" :: t -> raise Malformed
  | _ -> raise Malformed

let phrase_to_card phrase =
  match phrase with
  | [] -> raise Empty
  | [h] ->  
    if h = "drawfour" then make_card Wild NoneV DrawFour
    else if h = "wildcard" then make_card Wild NoneV WildCard
    else if h = "swaphands" then make_card Wild NoneV SwapHands
    else raise Malformed 
  | [h; t] -> begin
      let color = try string_to_color h with UnknownColor -> 
        raise Malformed in
      if int_of_string_opt t <> None then 
        let value = try string_to_value t with UnknownValue -> 
          raise Malformed in
        make_card color value NoneA
      else 
        let action = try string_to_action t with UnknownAction -> 
          raise Malformed in
        make_card color NoneV action
    end
  | _ -> raise Malformed 