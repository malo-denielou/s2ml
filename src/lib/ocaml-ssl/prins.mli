
type pr = { id:string; cert:string; ip:string; port:int;}

val register : pr -> unit

val psend : string -> Data.str -> unit
val precv : string -> Data.str


val get_privkey : Data.str -> Data.bytes
val get_pubkey : Data.str -> Data.bytes

val get_symkey : Data.str ->  Data.str -> Data.bytes
val get_mackey : Data.str ->  Data.str -> Data.bytes

val gen_keys : Data.str ->  Data.str -> Data.bytes
val reg_keys : Data.str ->  Data.str -> Data.bytes -> unit

val bind : string -> unit
val close : unit -> unit

val check_cache : string -> string -> Data.bytes -> unit


val connect : string -> string -> unit
val listen : string -> unit
