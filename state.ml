open Effect
open Effect.Shallow

module type STATE = sig

  type player =  A | B

  type color = White | Black

  type card_state = Open | Close

  val get : player -> (card_state * (int*color)) 
  val run : (unit -> unit) -> unit
  val hand : player -> (card_state * (int*color)) list
  val change : player -> (int*color)-> (card_state * (int*color))
  val print_card : card_state * (int*color) -> string
  val color_of_string : string -> color
  val print_player : player -> string
  val another_player : player -> player
  val print_color : color -> string
end

module CardState : STATE = struct

  type player =  A | B

  type color = White | Black

  type card_state = Open | Close

  type _ Effect.t += Get : player -> (card_state * (int*color))  Effect.t

  type _ Effect.t += Hand : player -> (card_state * (int*color)) list Effect.t

  type _ Effect.t += Change : player * (int*color) -> (card_state * (int*color)) Effect.t

  let get player = perform (Get player)

  let hand player = perform (Hand player)

  let change player card = perform (Change (player,card))

  let print_color color = match color with
    White -> "White"
    | Black -> "Black"

  let print_card_state card_state = match card_state with
    Open -> "###"
    | Close -> ":::"

  let color_of_string str = match str with
    "White" -> White
    | "Black" -> Black
    | _ -> failwith "Invalid color"

  let print_card (card_state, (num,color)) = print_card_state card_state ^ " (" ^ string_of_int num ^ "," ^ print_color color ^ ")"

  let print_player player = match player with
    A -> "A:"
    | B -> "B:"

  let another_player player = match player with
    A -> B
    | B -> A

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
  
  let card_sort lst card state = 
    let rec loop lst = match lst with 
      [] -> [(state,card)]
      | (cs,(x,y))::xs -> if x < fst card then 
                        (cs,(x,y))::(loop xs) 
                     else if x > fst card then
                        (state, card)::lst
                     else
                      if snd card = Black then
                        (state, card)::lst
                      else
                        (cs, (x,y))::(state, card)::xs
    in
    loop lst

  let card_sort_first lst = 
    let rec loop lst = match lst with 
      [] -> []
      | x::xs -> card_sort (loop xs) x Close
    in
    loop lst 
  
  let card_insert a_hand b_hand card state player = 
    if player = A then
      card_sort a_hand card state, b_hand
    else
      a_hand, card_sort b_hand card state

  let card_change a_hand b_hand card player = 
    if player = A then
      List.map (fun (a,b) -> if b = card then (Open,card) else (a,b)) a_hand, b_hand
    else
      a_hand, List.map (fun (a,b) -> if b = card then (Open,card) else (a,b)) a_hand

  let deck = [(0,White);(1,White);(2,White);(3,White);(4,White);(5,White);(6,White);(7,White);(8,White);(9,White);(10,White);(11,White);
              (0,Black);(1,Black);(2,Black);(3,Black);(4,Black);(5,Black);(6,Black);(7,Black);(8,Black);(9,Black);(10,Black);(11,Black)]

  let run f  =
    let rec loop : type a r. (card_state * (int*color)) list -> (card_state * (int*color)) list -> (int*color) list ->(a, r) continuation -> a -> r =
      fun player_a_hand player_b_hand shuffled_deck k x ->
        continue_with k x
        { retc = (fun result -> result);
          exnc = (fun e -> raise e);
          effc = (fun (type b) (eff: b Effect.t) ->
            match eff with
            | Get player -> Some (fun (k: (b,r) continuation) ->
                    let card::shuffled_deck = shuffled_deck in
                    let player_a_hand , player_b_hand = card_insert player_a_hand player_b_hand card Close player in
                    loop player_a_hand player_b_hand shuffled_deck k (Close ,card))
            | Hand player-> Some (fun (k: (b,r) continuation) ->
                    if player = A then
                      loop player_a_hand player_b_hand shuffled_deck k player_a_hand
                    else
                      loop player_a_hand player_b_hand shuffled_deck k player_b_hand)
            | Change (player,card) -> Some (fun (k: (b,r) continuation) ->
                    let player_a_hand , player_b_hand = card_change player_a_hand player_b_hand card player in
                    loop player_a_hand player_b_hand shuffled_deck k (Open ,card))
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

