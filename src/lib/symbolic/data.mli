type str =
    | Literal of string
    | Base64 of bytes
and bytes =
    | Concat of bytes * bytes
    | Utf8 of str
    | Bin of blob
and blob =
    | Hash of bytes
    | Nonce of bytes
    | AsymSign of bytes * bytes
    | AsymEncrypt of bytes * bytes
    | SymEncrypt of bytes * bytes
    | Mac of bytes * bytes

val pickle: 'a -> 'a
val unpickle: 'a -> 'a

val hash : bytes -> bytes
val ihash : bytes -> bytes
val sign : bytes -> bytes -> bytes
val isign : bytes -> bytes * bytes
val encr : bytes -> bytes -> bytes 
val iencr : bytes -> bytes * bytes
val asym_encr : bytes -> bytes -> bytes 
val asym_iencr : bytes -> bytes * bytes
val mac : bytes -> bytes -> bytes 
val imac : bytes -> bytes * bytes

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
val spmmac: bytes -> string
val spmsg: str -> string
