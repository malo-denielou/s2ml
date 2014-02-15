(* Global constants and functions *)

val debug_bool : bool ref
val debug : string -> string -> unit
val encrypting : bool ref
val macing : bool ref
val caching : bool ref
val keyexch : bool ref

val test : bool -> string -> unit
val assume : 'a -> unit
