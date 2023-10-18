open Printf
open Effect
open Effect.Deep

open State

module Card_state = CardState

type response = Success | Failure

type result = 
  | Win
  | Lose
  | Continue

type _ Effect.t += Try : Card_state.player * (int * (int * Card_state.color)) -> (response * response) Effect.t
type _ Effect.t += Wait : unit -> (response * response) Effect.t
type _ Effect.t += Now : Card_state.player -> ((result * result) * (Card_state.card_state * (int * Card_state.color)) list) Effect.t (* 現在のゲームの結果を返す *)

type game_satate = 
  Attack
  | Defend

type user_status =
    Done
  | Paused of response * ((response * response), user_status) continuation
  | After_attack of (result * (Card_state.card_state * (int * Card_state.color)) list) * (((result  * result) * ((Card_state.card_state * (int * Card_state.color)) list)), user_status) continuation

let response_attack_converter response1 response2 = match response1, response2 with
  Success, Success -> Success
  | _ -> Failure

let response_deffend_converter response1 response2 = match response1, response2 with
  Success, Success -> Failure
  | _ -> Success

let result_attack_converter result1 result2 = match result1, result2 with
  Win, Continue -> Win
  | Continue, Win -> Win
  | _ -> Continue

let result_deffend_converter result1 result2 = match result1, result2 with
  Win, Continue -> Lose
  | Continue, Win -> Lose
  | _ -> Continue

let print_hand player hand = 
  print_endline ("Player " ^ Card_state.print_player player ^ " hand: " ^ List.fold_right (fun a b -> Card_state.print_card a ^ b) hand "")



let step f v () = 
  match_with f v 
    { retc = (fun _ -> Done);
      exnc = (fun e -> raise e);
      effc = (fun (type b) (eff: b t) -> 
        match eff with 
          Try (player, (place, (num, color))) -> Some (fun (k: (b,_) continuation) ->
            let player_hand = Card_state.hand player in (* アタックされているプレイヤーのhandを取得 *)
            let (_,card) = List.nth player_hand place in (* placeで指定されたカードを取り出す *)
            let result =  card = (num, color) in (* カードとアタックの数字と色を比較 *)
            if result then 
              let _ = Card_state.change player card in 
              Paused (Success, k)
            else
              Paused (Failure, k))
          | Wait () -> Some (fun (k: (b,_) continuation) -> Paused (Success, k))
          | Now player -> Some (fun (k: (b,_) continuation) -> 
            let hand = Card_state.hand player in
            print_endline ("Player " ^ Card_state.print_player player ^ " hand: " ^ List.fold_right (fun a b -> Card_state.print_card a ^ b) hand "");
            let result = Card_state.is_win hand in
            if result then
              After_attack ((Win, hand), k)
            else
              After_attack ((Continue, hand), k))
          | _ -> failwith "improper synchronization"
      )}

let rec run_both a b =
  match a (), b () with
  | Done, Done -> ()
  | Paused (v1, k1), Paused (v2, k2) ->
      let return_value = (response_attack_converter v1 v2, response_deffend_converter v1 v2) in
      run_both (fun () -> continue k1 return_value) (fun () -> continue k2 return_value)
  | After_attack ((v1,hand1), k1), After_attack ((v2,hand2), k2) ->
      let return_value = (result_attack_converter v1 v2, result_deffend_converter v1 v2) in
      run_both (fun () -> continue k1 (return_value, hand2)) (fun () -> continue k2 (return_value, hand1))
  | _ -> failwith "improper synchronization"

(* プレイヤーが最後に行うアクション *)
let player_last_action action player response = match action, response with
    Attack, Success -> print_endline ("Player " ^ Card_state.print_player player ^ " attack success!! ");
                       let ((result, _), player_hand) = perform (Now (Card_state.another_player player)) in
                        let _ = if result = Win then
                                  printf "Player %s win!!\n" (Card_state.print_player player)
                                else
                                  print_hand player player_hand in 
                        result
    | Attack, Failure -> print_endline ("Player " ^ Card_state.print_player player ^ " attack failure!! ");
                          let ((result, _), player_hand) = perform (Now (Card_state.another_player player)) in
                          print_hand player player_hand;
                          result
    | Defend, Success -> print_endline ("Player " ^ Card_state.print_player player ^ " defend success!! ");
                          let ((_, result), player_hand) = perform (Now (Card_state.another_player player)) in
                          print_hand player player_hand;
                          result
    | Defend, Failure -> print_endline ("Player " ^ Card_state.print_player player ^ " defend failure!! ");
                        let ((_, result), player_hand) = perform (Now (Card_state.another_player player)) in
                        let _ = if result = Lose then
                                  printf "Player %s lose...\n" (Card_state.print_player player)
                                else
                                  print_hand player player_hand in 
                        result

(* ここでのplayerは自分のことを指す *)
(* TODO: 変数名いい感じにする *)
let rec player_action action player = match action with
  Attack -> 
    print_endline ("Player " ^ Card_state.print_player player ^ " attack!! " ^ "Please enter a string:");
    let input = input_line stdin in
    let (i, (j, c)) = Scanf.sscanf input "%d %d %s" (fun i j c -> (i, (j, Card_state.color_of_string c))) in
    printf "place %d attack (%d, %s)\n" i j (Card_state.print_color c);
    let (response,_) = perform (Try (Card_state.another_player player, (i, (j, c)))) in
    let result = player_last_action action player response in 
    if result = Win then
      ()
    else 
      player_action Defend player
  | Defend -> 
    print_endline ("Player " ^ Card_state.print_player player ^ " defend!! ");
    let (_,response) = perform (Wait ()) in
    let result = player_last_action action player response in 
    if result = Lose then
      ()
    else
      player_action Attack player



let _ = Card_state.run (fun () -> run_both (step (player_action Attack) Card_state.A) (step (player_action Defend) Card_state.B))
