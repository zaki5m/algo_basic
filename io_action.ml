open Unix

open Effect
open Effect.Shallow

open State

module type IOACTION = sig
  val input : CardState.player -> string
  val printbuf : CardState.player -> string -> unit
  val print_eff : (unit -> 'a) -> file_descr -> file_perm -> 'a
end

module IoAction : IOACTION = struct
  type _ Effect.t += Input : CardState.player -> string Effect.t
  type _ Effect.t += PrintBuf : CardState.player * string -> unit Effect.t

  let input player = perform (Input player)

  let printbuf player msg = perform (PrintBuf (player, msg))

  let print_eff f client_socket buffer_size = 
    let rec loop: type a r. (a, r) continuation -> a -> r =
      fun k x -> 
        continue_with k x 
        { retc = (fun r -> r);
          exnc = (fun e -> raise e);
          effc = (fun (type b) (eff: b t) -> 
            match eff with 
              Input player -> Some (fun (k: (b,_) continuation) -> 
                match player with 
                  | CardState.A -> let msg = input_line Stdlib.stdin in
                                    loop k msg
                  | CardState.B ->  let buffer = Bytes.create buffer_size in
                                    let n = read client_socket buffer 0 buffer_size in
                                    let msg = Bytes.sub_string buffer 0 n in
                                    loop k msg)
              | PrintBuf (player, str) -> Some (fun (k: (b,_) continuation) -> 
                match player with 
                  | CardState.A -> print_string str; 
                                  flush Stdlib.stdout;
                                  loop k ()
                  | CardState.B -> let _ = write client_socket (Bytes.of_string str) 0 (String.length str) in loop k ())
              | _ -> None
          )}
    in
    loop (fiber f) ()
end

