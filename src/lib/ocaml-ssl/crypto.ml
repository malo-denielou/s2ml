open Unix
open Global
open Data
(*
external openssl_base64: string -> string -> unit = "base64" 
external openssl_ibase64: string -> string -> unit = "ibase64"
*)
external openssl_mkNonce: unit -> string = "mkNonce" "noalloc"
external openssl_sha1: string -> string -> unit = "sha1" "noalloc"
external openssl_rsa_sha1: string -> string -> string -> unit = "rsa_sha1" "noalloc"
external openssl_rsa_verify: string -> string -> string -> bool = "rsa_verify" 

external openssl_rsa_enc: string -> string -> string -> unit = "rsa_enc" "noalloc"
external openssl_rsa_dec: string -> string -> string -> int = "rsa_dec" "noalloc"

external openssl_aes_enc: string -> string -> string -> int = "aes_enc" "noalloc"
external openssl_aes_dec: string -> string -> int -> string -> int = "aes_dec" "noalloc"

external openssl_mac: string -> string -> string -> int = "mac" "noalloc"

type 'a hkey = bytes
type 'a symkey = bytes

let debug = debug "Crypto"


(* cryptography *)

(*let nonce = 
  let fd = Unix.openfile "/dev/random" [Unix.O_RDONLY] 0o640 in
  let nonce = String.create 16 in
  ignore (Unix.read fd nonce 0 16) ;
  Unix.close fd;
  utf8 (cS (nonce))*)

let c = ref 0

let mkNonce () =
  let t = string_of_float (Unix.gettimeofday()) in
  let rdm = String.sub t (String.length t -7) 7 in
  let () = incr c in
  let x = string_of_int (!c) in
  let nonce = 
    "MdP"
    ^rdm
    ^"Nb"
    ^(if (String.length x)<10 then "000" else 
        (if (String.length x)<100 then "00" else
           (if (String.length x)<1000 then "0" else "")))
    ^x in
  let () = debug ("Creating nonce: "^(nonce)) in
  utf8 (cS (nonce))
(*
  utf8 (cS ("MonMotdePasseSur"
                          (*openssl_mkNonce ()*)
                           ))*)

let sha1 msg = 
    let s = String.create 20 in
    openssl_sha1 (iS (iutf8 msg)) s ; utf8 (cS s)


let sha1_verify s h =
  let h' = String.create 20 in
  let () = openssl_sha1 (iS (iutf8 s)) h' in
  if h = h' then () else failwith "Hash verification failure" 





let rsa_sign sk msg = 
    let s = sha1 msg in
    let sign = String.create 128 in
    openssl_rsa_sha1 (iS (iutf8 sk)) (iS (iutf8 s)) sign;
    utf8 (cS sign)

let rsa_verify pk msg s = 
  let m = (sha1 msg) in
    if openssl_rsa_verify (iS (iutf8 pk)) (iS (iutf8 m)) (iS (iutf8 s))
    then 
    ()
    else failwith ("RSA_verify failed: "^pk)



let rsa_encrypt pk msg =
  let enc = String.create 128 in
  let () = debug ("Encrypting "^(spstr (msg))^
                    " with "^(spstr (pk))) in
  openssl_rsa_enc (iS (iutf8 pk)) (iS (iutf8  msg)) enc;
  utf8 (cS enc)

let rsa_decrypt sk msg =
  let dec = String.create 128 in  
  let d =  openssl_rsa_dec (iS (iutf8 sk)) (iS (iutf8 msg)) dec in
  utf8 (cS (String.sub dec 0 40(*d*)))

let sym_decrypt sk msg = 
  let length = String.length (iS (iutf8  msg)) in
  let () = debug ("Decrypting "^(spstr (base64 msg))^
                    " of size "^(string_of_int length)^
                    " with "^(spstr (base64 sk))) in
  if !encrypting then
    let dec = String.create 1024 in  
    let d =  openssl_aes_dec (iS (iutf8 sk)) (iS (iutf8 ( msg))) length dec in
    let res = utf8 (cS (String.sub dec 0 d)) in
    let () = debug ("Decrypted: "^(spstr (base64 res))) in
    res
  else
    msg

let sym_encrypt sk msg = 
  if !encrypting then
    let enc = String.create 1024 in
    let () = debug ("Encrypting "^(spstr (base64 msg))^
                      " with "^(spstr (base64 sk))) in
    let d = openssl_aes_enc (iS (iutf8 sk)) (iS (iutf8  msg))  enc in
    let res = (utf8 (cS (String.sub enc 0 d))) in
    let () = debug ("Encrypted "^(spstr (base64 res))) in
    res
  else
    msg




let mac k msg = 
  if !macing then
    if !signing then begin
      rsa_sign "alice.key" msg
    end
    else begin
      let s = String.create 1024 in
      let d = openssl_mac k (iS (iutf8 msg)) s in
      utf8 (cS (String.sub s 0 d))
    end
  else
    utf8 (cS "")


let mac_verify k hm m = 
(*  Printf.printf "\nchecking MAC\nmac=\n%s\nk=\n%s\nm=\n%s\n" hm k m;*)
  if !macing then
    if !signing then begin
      if !verifying then begin
        rsa_verify "alice.pub" m hm end
      else () ;
      (utf8 "")
    end
    else begin
      if !verifying then begin
        let s = mac k m in
        (*  Printf.printf "\nrecomputed:\n%s\n" s;*)
        if s=hm 
        then (utf8 "") 
        else failwith "wrong mac" end
      else (utf8 "")
    end
  else  (utf8 "")




let pickle x = x
let unpickle x = x
