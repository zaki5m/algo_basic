open Printf
open Effect
open Effect.Shallow

module type STATE = sig

  type player =  A | B

  type color = White | Black

  val get : player -> (int*color) 
  val run : (unit -> unit) -> unit
  val hand : player -> (int*color) list
  val  print_color : color -> string
end

module State : STATE = struct

  type player =  A | B

  type color = White | Black

  type _ Effect.t += Get : player -> (int*color) Effect.t

  type _ Effect.t += Hand : player -> (int*color) list Effect.t

  let get player = perform (Get player)

  let hand player = perform (Hand player)

  let print_color color = match color with
    White -> "White"
    | Black -> "Black"

  let shuffle lst =
    let compare _ _ = (Random.int 3) - 1 in
    List.sort compare lst
  
  let card_take lst n =
    let rec loop n lst acc = match n, lst with 
      0, _ -> acc , lst
      | _, [] -> acc , []
      | n, x::xs -> loop (n-1) xs (x::acc)
    in
    loop n lst []
  
  let card_sort lst card = 
    let rec loop lst = match lst with 
      [] -> [card]
      | (x,y)::xs -> if x < fst card then 
                        (x,y)::(loop xs) 
                     else if x > fst card then
                        card::lst
                     else
                      if snd card = Black then
                        card::lst
                      else
                        (x,y)::card::xs
    in
    loop lst

  let card_sort_first lst = 
    let rec loop lst = match lst with 
      [] -> []
      | x::xs -> card_sort (loop xs) x
    in
    loop lst 
  
  let card_insert a_hand b_hand card player = 
    if player = A then
      card_sort a_hand card, b_hand
    else
      a_hand, card_sort b_hand card

  let deck = [(0,White);(1,White);(2,White);(3,White);(4,White);(5,White);(6,White);(7,White);(8,White);(9,White);(10,White);(11,White);
              (0,Black);(1,Black);(2,Black);(3,Black);(4,Black);(5,Black);(6,Black);(7,Black);(8,Black);(9,Black);(10,Black);(11,Black)]

  let run f  =
    let rec loop : type a r. (int*color) list -> (int*color) list -> (int*color) list ->(a, r) continuation -> a -> r =
      fun player_a_hand player_b_hand shuffled_deck k x ->
        continue_with k x
        { retc = (fun result -> result);
          exnc = (fun e -> raise e);
          effc = (fun (type b) (eff: b Effect.t) ->
            match eff with
            | Get player -> Some (fun (k: (b,r) continuation) ->
                    let card::shuffled_deck = shuffled_deck in
                    let player_a_hand , player_b_hand = card_insert player_a_hand player_b_hand card player in
                    loop player_a_hand player_b_hand shuffled_deck k card)
            | Hand player-> Some (fun (k: (b,r) continuation) ->
                    if player = A then
                      loop player_a_hand player_b_hand shuffled_deck k player_a_hand
                    else
                      loop player_a_hand player_b_hand shuffled_deck k player_b_hand)
            | _ -> None)
        }
    in
    let shuffled_deck = shuffle deck in
    let player_a_hand, shuffled_deck = card_take shuffled_deck 5 in
    let player_b_hand, shuffled_deck = card_take shuffled_deck 5 in
    let player_a_hand = card_sort_first player_a_hand in
    let player_b_hand = card_sort_first player_b_hand in
    loop player_a_hand player_b_hand shuffled_deck (fiber f) ()
end
