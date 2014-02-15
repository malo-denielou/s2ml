(* Global constants and functions *)

val debug_bool : bool ref
val debug : string -> string -> unit
val encrypting : bool ref
val macing : bool ref
val signing : bool ref
val caching : bool ref
val keyexch : bool ref
val verifying : bool ref
val tcp : bool ref

val test : bool -> string -> unit
val test_eq : 'a -> 'a -> string -> unit
val test_inf : 'a -> 'a -> string -> unit
val assume : 'a -> unit
