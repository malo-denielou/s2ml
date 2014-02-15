(* Global constants and functions *)

type rel = Before of int * int

val set_debug : bool -> unit
val debug : string -> string -> unit
val test_eq : 'a -> 'a -> string -> unit
val test_inf : int -> int -> string -> unit


