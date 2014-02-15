type str = string
type bytes = string

val base64 : bytes -> str
val ibase64 : str -> bytes
val utf8 : str -> bytes
val iutf8 : bytes -> str
val concat : bytes -> bytes -> bytes
val iconcat : bytes -> bytes * bytes
val concat3 : bytes -> bytes -> bytes -> bytes
val iconcat3 : bytes -> bytes * bytes * bytes
val cS : string -> str
val iS : str -> string
val spstr: str -> string
val spbytes: bytes -> string 
