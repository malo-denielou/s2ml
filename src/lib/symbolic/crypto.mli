
type 'a hkey
type 'a symkey

val mkNonce : unit -> Data.bytes
val sha1 : Data.bytes -> Data.bytes
val sha1_verify : Data.bytes -> Data.bytes -> unit
val rsa_sign : Data.bytes -> Data.bytes -> Data.bytes
val rsa_encrypt : Data.bytes -> Data.bytes -> Data.bytes
val rsa_decrypt : Data.bytes -> Data.bytes -> Data.bytes
val rsa_verify : Data.bytes -> Data.bytes -> Data.bytes -> unit
val mac : Data.bytes -> Data.bytes -> Data.bytes
val mac_verify : Data.bytes -> Data.bytes -> Data.bytes -> Data.bytes
val sym_encrypt : Data.bytes -> Data.bytes -> Data.bytes
val sym_decrypt : Data.bytes -> Data.bytes -> Data.bytes
