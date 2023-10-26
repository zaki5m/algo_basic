open Printf
open Unix
open Effect
open Effect.Deep

open State
open Server

module Card_state = CardState

module Io_Action = IoAction

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
   ("Player " ^ Card_state.print_player player ^ " hand: " ^ List.fold_right (fun a b -> Card_state.print_card a ^ b) hand "" ^ "\n")


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
            let msg = ("Player " ^ Card_state.print_player player ^ " hand: " ^ List.fold_right (fun a b -> Card_state.print_card a ^ b) hand "") in 
            Io_Action.printbuf player msg;
            let result = Card_state.is_win hand in
            if result then
              After_attack ((Win, hand), k)
            else
              After_attack ((Continue, hand), k))
          | _ -> None
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
    Attack, Success -> let msg =  ("Player " ^ Card_state.print_player player ^ " attack success!!\n") in 
                       Io_Action.printbuf player msg;
                       let ((result, _), player_hand) = perform (Now (Card_state.another_player player)) in
                        let _ = if result = Win then
                                  let msg = sprintf "Player %s win!!\n" (Card_state.print_player player) in
                                  Io_Action.printbuf player msg
                                else
                                  let msg = print_hand player player_hand in
                                  Io_Action.printbuf player msg
                        in
                        result
    | Attack, Failure ->  let msg = ("Player " ^ Card_state.print_player player ^ " attack failure!!\n") in 
                          Io_Action.printbuf player msg;
                          let ((result, _), player_hand) = perform (Now (Card_state.another_player player)) in
                          let msg = print_hand player player_hand in 
                          Io_Action.printbuf player msg;
                          result
    | Defend, Success ->  let msg = ("Player " ^ Card_state.print_player player ^ " defend success!!\n") in 
                          Io_Action.printbuf player msg;
                          let ((_, result), player_hand) = perform (Now (Card_state.another_player player)) in
                          let msg = print_hand player player_hand in 
                          Io_Action.printbuf player msg;
                          result
    | Defend, Failure -> let msg = ("Player " ^ Card_state.print_player player ^ " defend failure!!\n") in
                        Io_Action.printbuf player msg;
                        let ((_, result), player_hand) = perform (Now (Card_state.another_player player)) in
                        let _ = if result = Lose then
                                  let msg = sprintf "Player %s lose...\n" (Card_state.print_player player) in 
                                  Io_Action.printbuf player msg
                                else
                                  let msg = print_hand player player_hand in 
                                  Io_Action.printbuf player msg
                        in
                        result

(* ここでのplayerは自分のことを指す *)
(* TODO: 変数名いい感じにする *)
let rec player_action action player = match action with
  Attack -> 
    let msg = ("Player " ^ Card_state.print_player player ^ " attack!!\n") in
    Io_Action.printbuf player msg;
    let msg = "Please enter a card:\n" in
    Io_Action.printbuf player msg;
    let input = Io_Action.input player in 
    let (i, (j, c)) = Scanf.sscanf input "%d %d %s" (fun i j c -> (i, (j, Card_state.color_of_string c))) in
    let msg = sprintf "place %d attack (%d, %s)\n" i j (Card_state.print_color c) in 
    Io_Action.printbuf player msg;
    let (response,_) = perform (Try (Card_state.another_player player, (i, (j, c)))) in
    let result = player_last_action action player response in 
    if result = Win then
      ()
    else 
      player_action Defend player
  | Defend -> 
    let msg =  ("Player " ^ Card_state.print_player player ^ " defend!! ") in 
    Io_Action.printbuf player msg;
    let (_,response) = perform (Wait ()) in
    let result = player_last_action action player response in 
    if result = Lose then
      ()
    else
      player_action Attack player

let contains substring str =
  let regex = Str.regexp_string substring in
  try ignore (Str.search_forward regex str 0); true
  with Not_found -> false

let start_server port =
  let buffer_size = 200 in 
  let addr = ADDR_INET (inet_addr_any, port) in
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  bind server_socket addr;
  listen server_socket 5;
  Printf.printf "Waiting for connections on port %d...\n%!" port;
  let (client_socket, _) = accept server_socket in

  let buffer = Bytes.create buffer_size in
  let n = read client_socket buffer 0 buffer_size in
  let msg = Bytes.sub_string buffer 0 n in
  Printf.printf "%s" msg;
  flush Stdlib.stdout;
  let _ = Io_Action.print_eff (fun () -> Card_state.run (fun () -> run_both (step (player_action Attack) Card_state.A) (step (player_action Defend) Card_state.B))) client_socket buffer_size in 
  ()
  

let connect_to_peer ip port =
  let buffer_size = 200 in 
  let addr = ADDR_INET (inet_addr_of_string ip, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  connect socket addr;
  let msg = "start\n" in 
  let _ = write socket (Bytes.of_string msg) 0 (String.length msg) in
  try
    while true do
      let buffer = Bytes.create buffer_size in
      let n = read socket buffer 0 buffer_size in
      let response = Bytes.sub_string buffer 0 n in
      Printf.printf "%s" response;
      flush Stdlib.stdout;
      if contains "Please" response then
        let msg = read_line () in
        let _ = write socket (Bytes.of_string msg) 0 (String.length msg) in 
        ()
      else
        ()
    done
  with Exit ->
    close socket;
    Printf.printf "Connection closed.\n"


let () =
  let role = input_line Stdlib.stdin in
  match role with
  | "server" ->
      let port = int_of_string (input_line Stdlib.stdin) in
      start_server port
  | "client" ->
      let ip = input_line Stdlib.stdin in
      let port = int_of_string (input_line Stdlib.stdin) in
      connect_to_peer ip port
  | _ ->
      Printf.printf "Invalid role. Choose 'server' or 'client'.\n"

(* let _ = Card_state.run (fun () -> run_both (step (player_action Attack) Card_state.A) (step (player_action Defend) Card_state.B)) *)
