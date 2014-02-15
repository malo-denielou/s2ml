
open Global
open Data


type 'a hkey = bytes
type 'a symkey = bytes


let debug_crypto = debug "crypto"

(*let _ = Random.init (int_of_float (Unix.time ()))

let mkNonce () = 
  let s = Printf.sprintf "%i" (Random.int (1000000)) in
  Bin (Nonce (utf8 (cS s)))
*)

let counter = ref 0 

let mkNonce () =
  let n = "nonce"^(string_of_int !counter) in
  let () = incr counter in
  utf8 (cS n)
 

let sha1 s =
  hash s

let sha1_verify s h =
  let () = debug_crypto 
    ("Verifying known hash: "^(spbytes h)^" against "^(spbytes s)) in
  let ss = ihash h in
  if s = ss then () else failwith "Verification of hash failed"
 





let rsa_sign k t =
  sign k t

let test_eq k1 k2 = 
  let k1 = iS (iutf8 k1) in
  let k2 = iS (iutf8 k2) in
  let kk1 = String.sub k1 0 (String.length k1 -3) in
  let kk2 = String.sub k2 0 (String.length k2 -3) in
  kk1 = kk2


let rsa_verify k m s = 
  let () = debug_crypto ("Verifying: "^(spbytes s)) in
  let () = debug_crypto ("against    "^(spbytes m)^" with "^(spbytes k)) in
  let (k',m') = isign s in
  assert (test_eq k k' && m = m')


let sym_encrypt k t = encr k t

let sym_decrypt k s =
  let () = debug_crypto 
    ("Decrypting : "^(spbytes s)^" with key "^(spbytes k)) in
  let k',m = iencr s in 
  if test_eq k k' 
  then m 
  else failwith "Decryption failed"
  

let mac k t = mac k t

let mac_verify k s msg = 
  let mm = mac k msg in 
  let () = debug_crypto ("Verifying MAC: "^(spmmac s)) in
  let () = debug_crypto (" against     : "^(spmmac mm)) in
  let (k',m) = imac s in 
  if test_eq k k' && m = msg 
  then msg
  else failwith "Verification failed" 


let rsa_encrypt key text = asym_encr key text
let rsa_decrypt key msg =  
  let () = debug_crypto 
    ("AsymDecrypting : "^(spbytes msg)^" with key "^(spbytes key)) in
  let k',m = asym_iencr msg in 
  if test_eq key k' 
  then m 
  else failwith "Decryption failed"
   
