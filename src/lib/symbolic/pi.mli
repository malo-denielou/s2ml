val yield : int -> unit
val fork : (unit -> unit) -> unit
val fork_dep : (unit -> unit) -> (unit -> unit) -> unit

type chan

val create_chan : string -> unit
val recv : string -> Data.str
val send : string -> Data.str -> unit
val assume : 'a -> unit
val expect : 'a -> unit
