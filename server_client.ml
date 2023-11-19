open Unix

open Algo 


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
  let _ = algo_run client_socket buffer_size in 
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
  Printf.printf "server or client?\n";
  flush Stdlib.stdout;
  let role = input_line Stdlib.stdin in
  match role with
  | "server" ->
      Printf.printf "Enter port number:\n";
      flush Stdlib.stdout;
      let port = int_of_string (input_line Stdlib.stdin) in
      start_server port
  | "client" ->
      Printf.printf "Enter IP address:\n";
      flush Stdlib.stdout;
      let ip = input_line Stdlib.stdin in
      Printf.printf "Enter port number:\n";
      flush Stdlib.stdout;
      let port = int_of_string (input_line Stdlib.stdin) in
      connect_to_peer ip port
  | _ ->
      Printf.printf "Invalid role. Choose 'server' or 'client'.\n"