module type STATE = sig

    type player =  A | B
  
    type color = White | Black
  
    type card_state = Open | Close
  
    val get : player -> (card_state * (int*color)) 
    val run : (unit -> unit) -> unit
    val hand : player -> (card_state * (int*color)) list
    val change : player -> (int*color)-> (card_state * (int*color))
    val print_card : card_state * (int*color) -> string
end

module CardState : STATE