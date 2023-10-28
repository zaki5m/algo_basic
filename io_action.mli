open State
open Unix

module type IOACTION = sig
    val input : CardState.player -> string
    val printbuf : CardState.player -> string -> unit
    val print_eff : (unit -> 'a) -> file_descr -> file_perm -> 'a
end

module IoAction : IOACTION