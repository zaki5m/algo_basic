open Effect
open Effect.Deep

type _ Effect.t += Hit : int -> int Effect.t
type _ Effect.t += Miss : int -> int Effect.t

type color = White | Black

type game_satate = 
  Attack of int * (int * color)
  | Defend

type user_status =
    Done
  | Input of int * (int, user_status) continuation

let step f v () = 
  match_with f v 
    { retc = (fun _ -> Done);
      exnc = (fun e -> raise e);
      effc = (fun (type b) (eff: b t) -> 
        match eff with 
          Miss i -> Some (fun (k: (b,_) continuation) ->
            Input (i, k))
          | _ -> failwith "unhandled effect"
      )}



 
