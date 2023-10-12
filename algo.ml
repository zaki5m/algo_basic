open Printf
open Effect
open Effect.Deep

open State

module Card_state = CardState

type response = Success | Failure

type _ Effect.t += Try : Card_state.player * (int * (int * Card_state.color)) -> (response * response) Effect.t
type _ Effect.t += Wait : unit -> (response * response) Effect.t

type game_satate = 
  Attack of int * (int * Card_state.color)
  | Defend

type user_status =
    Done
  | Paused of response * ((response * response), user_status) continuation

let response_attack_converter response1 response2 = match response1, response2 with
  Success, Success -> Success
  | _ -> Failure

  let response_deffend_converter response1 response2 = match response1, response2 with
  Success, Success -> Failure
  | _ -> Success


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
          | _ -> failwith "improper synchronization"
      )}

let rec run_both a b =
  match a (), b () with
  | Done, Done -> ()
  | Paused (v1, k1), Paused (v2, k2) ->
      let return_value = (response_attack_converter v1 v2, response_deffend_converter v1 v2) in
      run_both (fun () -> continue k1 return_value) (fun () -> continue k2 return_value)
  | _ -> failwith "improper synchronization"

(* ここでのplayerは相手のことを指す *)
(* TODO: 変数名いい感じにする *)
let rec player_action action player = match action with
  Attack (i, (j, c)) -> 
    printf "Player %d attacks %d\n" i j;
    let (result,_) = perform (Try (player, (i, (j, c)))) in
    if result = Success then
      printf "Player %d hits %d\n" i j
    else
      printf "Player %d misses %d\n" i j;
  | Defend -> 
    printf "Player defends\n";
    let (_,result) = perform (Wait ()) in
    if result = Success then
      printf "defends success\n"
    else
      printf "Player defends miss\n" 

 

let _ = Card_state.run (fun () -> run_both (step (player_action (Attack (2, (3, Card_state.Black)))) Card_state.A) (step (player_action Defend) Card_state.B))