(* Copyright (c) Microsoft Corporation.  All rights reserved.  *)
module Crypto

open System.Security.Cryptography
open Printf
open Data
open Global

type 'a hkey = bytes
type 'a symkey = bytes

type key = 
    SymKey of bytes
  | RSAKey of RSACryptoServiceProvider

let keytobytes kb = match kb with
    SymKey(k) -> k
  | _ -> failwith "keytobytes: asym, gss not implemented concretely"
 

let symkey k = SymKey k

(* random number generation *) 

let rng = new RNGCryptoServiceProvider ()


let mkNonce () : bytes =
  let x = Bytearray.make 16 in
  rng.GetBytes x; x


let sha1_instance = SHA1.Create ()
let sha1 (x:bytes) : bytes = sha1_instance.ComputeHash x

let sha1_verify (x:bytes) (h:bytes) =
  if sha1 x=h then () else failwith "Verification of hash failed"


let hmacsha1 (k:byte[]) (x:bytes) : bytes = 
  (new HMACSHA1 (k)).ComputeHash x

let mac (k:bytes) (msg:bytes) =   
  if !macing then 
(*    (Printf.printf "\nk %d:\n" k.Length;
     print_any k;
     Printf.printf "\nmsg:%d\n" msg.Length;
     Printf.printf "%s\n" (base64 msg);
     Printf.printf "\n%s\n" (base64 (Bytearray.sub msg 0 39)); *)
     let hm = hmacsha1 k msg in
(*       Printf.printf "\nmac: %d\n" hm.Length;
       print_any hm;*)
       hm
  else
    utf8 (cS "")

let mac_verify k (hm:bytes) msg = 
  if !macing then
    ( let s = mac k msg in
	(*Printf.printf "\nreceived mac: %d\n" hm.Length; print_any hm;
        Printf.printf "\nrecomputed:\n%s\n" s;*)
      if s=hm then msg else failwith "wrong mac" )
  else msg




let rsa_sign (k:key) (v:bytes)  = 
  match k with 
      RSAKey skey ->    
	let oid = CryptoConfig.MapNameToOID "sha1" in
	  skey.SignHash (sha1 v, oid)
    | _ -> failwith "RSA signature only defined for asymmetric keys"
    

let rsa_verify (k:key) (v:bytes) (x:bytes)  =
  match k with 
      RSAKey pkey ->    
	let oid = CryptoConfig.MapNameToOID "sha1" in
	  if not (pkey.VerifyHash (sha1 v, oid, x))
	  then failwith "RSA_verify failed"
    | _ -> failwith "RSA signature only defined for asymmetric keys"

let rsa_encrypt (k:key)  (v:bytes)  = 
  match k with 
      RSAKey pkey ->  pkey.Encrypt(v,false) 
    | _ -> failwith "RSA encryption only defined for asymmetric keys"
    
let rsa_decrypt (k:key) (v:bytes) =
  match k with 
      RSAKey skey ->  skey.Decrypt(v,false) 
    | _ -> failwith "RSA encryption only defined for asymmetric keys"


let pad plain = concat plain [| |]
let unpad plain = let v, pad = iconcat plain in v 

let aes_encrypt (key:byte[]) (v:bytes) = 
  let plain = concatfix (size v) v in
  let keyl = 8 * key.Length  in
  let block = 16 in (* 128 bits *)
  let rij = new RijndaelManaged() in
    rij.set_KeySize(keyl);
    rij.set_Padding(PaddingMode.ANSIX923);
    rij.GenerateIV();
    let iv = rij.get_IV() in
    let enc = rij.CreateEncryptor(key,iv) in
    let mems = new System.IO.MemoryStream() in
    let crs = new CryptoStream(mems,enc,CryptoStreamMode.Write) in
    let _ =  crs.Write(plain,0,plain.Length) in
    let _ = crs.FlushFinalBlock() in
    let cip = mems.ToArray() in
    let ctxt = Bytearray.make (iv.Length + cip.Length) in
    let _ = System.Array.Copy(iv,0,ctxt,0,iv.Length) in
    let _ = System.Array.Copy(cip,0,ctxt,iv.Length,cip.Length) in
      mems.Close();
      crs.Close();
      ctxt

let aes_decrypt (key:byte[]) (cipher:bytes) = 
  let rij = new RijndaelManaged() in
  let keyl = 8 * key.Length  in
    rij.set_KeySize(keyl);
    rij.set_Padding(PaddingMode.ANSIX923);
    let block = 16 in (* 128 bits *)
    let iv = Bytearray.make (block) in
    let _ = System.Array.Copy(cipher,0,iv,0,iv.Length) in
    let cip = Bytearray.make (cipher.Length - iv.Length) in
    let _ = System.Array.Copy(cipher,iv.Length,cip,0,(cipher.Length - iv.Length)) in
    let dec = rij.CreateDecryptor(key,iv) in
    let mems = new System.IO.MemoryStream(cip) in
    let crs = new CryptoStream(mems,dec,CryptoStreamMode.Read) in
    let plain = Bytearray.make(cipher.Length) in  
    let _ =  crs.Read(plain,0,plain.Length) in
    let s,pl = iconcatfix 2 plain in
    let l = Byte.to_int(s.(0))+256*Byte.to_int(s.(1)) in
      Bytearray.sub pl 0 l

let sym_decrypt sk msg = 
  if !encrypting then
    let plain = aes_decrypt sk msg in
(*      Printf.printf "decr:\n";
      Printf.printf "\nsk:\n";
      print_any sk;
      Printf.printf "\ncipher:\n";
      print_any msg;
      Printf.printf "\nplain:\n";
      print_any plain; *)
      plain

  else
    msg

let sym_encrypt sk msg = 
  if !encrypting then
    let cipher = aes_encrypt sk msg in
(*      Printf.printf "encr:\n";
      Printf.printf "\nsk:\n";
      print_any sk;
      Printf.printf "\nplain:\n";
      print_any msg;
      Printf.printf "\ncipher:\n";
      print_any cipher;*)
      cipher
  else 
    msg



open System.Security.Cryptography.X509Certificates

(*---- RSA. *)

let rsa_skey (key:str) = 
  let rsa = new RSACryptoServiceProvider () in
    rsa.FromXmlString(key);
    RSAKey rsa

let rsa_pub (k:key) = 
  match k with
      RSAKey skey ->
	let pub = skey.ExportParameters(false) in
	let rsa = new RSACryptoServiceProvider () in
	  rsa.ImportParameters(pub);
	  RSAKey rsa
    | _ -> failwith "Rsa_pub only defined for asymmetric private keys"

let rsa_pkey (key:str) = rsa_pub (rsa_skey key)

(* only works for 1024 bits keys *)
let rsa_pkey_bytes (key:byte[]) = 
  let mutable rkey = new RSAParameters() in
  rkey.Exponent <- Array.sub key (key.Length - 3) 3;
  rkey.Modulus <- Array.sub key (key.Length - 128 - 5) 128; 
  let rsa = new RSACryptoServiceProvider () in
    rsa.ImportParameters(rkey);
    RSAKey rsa

(* probably only works for 1024 bits keys *)
let rsa_skey_bytes (key:byte[]) = 
  let mutable rkey = new RSAParameters() in
  rkey.Modulus <- Array.sub key 11 128;
  rkey.Exponent <- Array.sub key 141 3;
  rkey.D <- Array.sub key 147 128;
  rkey.P <- Array.sub key 278 64;
  rkey.Q <- Array.sub key 345 64;
  rkey.DP <- Array.sub key 412 64;
  rkey.DQ <- Array.sub key 479 64;
  rkey.InverseQ <- Array.sub key 545 64; 
  let rsa = new RSACryptoServiceProvider () in
    rsa.ImportParameters(rkey);
    RSAKey rsa


let asympubkey k = (rsa_pkey_bytes k)
let asymprivkey k = (rsa_skey_bytes k)

let asympub k = (rsa_pkey k)
let asympriv k = (rsa_skey k)

let rsa_keygen () = 
  let rsa = new RSACryptoServiceProvider() in
    RSAKey rsa

(* RSA Encryption *)

(* Setting OAEP = true for indigo, = false for WSE *)


let rsa_decrypt_oaep (k:key) (v:bytes) =
  match k with 
      RSAKey skey ->  skey.Decrypt(v,true) 
    | _ -> failwith "RSA encryption only defined for asymmetric keys"

(* RSA Signatures *)


(* ARCfour (see [draft-kaukonen-cipher-arcfour-03.txt]) *)

type arcfour_state = {i : int ref; j : int ref; s : int array}
//type stream_cipher_state = arcfour_state



let arcfour_init (key:byte[]) = 
  let s = Array.init 256 (fun i -> i) in
  let keylen = key.Length in
  let j = ref 0 in
  for i = 0 to 255 do
    j := (!j + s.(i) + (Byte.to_int (Bytearray.get key (i%keylen)))) &&& 0xff;
    let temp = s.(i) in
    s.(i) <- s.(!j);
    s.(!j) <- temp
  done;
  {i=ref 0;j=ref 0;s=s}

let print_array str (msg:int[]) =  
  Printf.printf "\n%s (%d)\n" str msg.Length; 
  Array.iter (fun x -> Printf.printf "%02x " x) msg


let arcfour_encrypt (p:byte[]) s = 
  let len = p.Length in
  let c = Bytearray.make len in      
  let i = s.i in
  let j = s.j in
  let ss = s.s in
  for l = 0 to len-1 do
    i := (!i+1) &&& 0xff;
    j := (!j + ss.(!i)) &&& 0xff;
    let temp = ss.(!i) in
    ss.(!i) <- ss.(!j);
    ss.(!j) <- temp;
    Bytearray.set c l (Byte.of_int 
		       ((ss.((ss.(!i) + ss.(!j)) &&& 0xff))
			 ^^^
		       (Byte.to_int (Bytearray.get p l))))
  done;
  //print_array "[arcfour_encrypt] ss" ss;
  c

type stream_cipher_state = ArcFourState of arcfour_state

let stream_cipher_init key = 
  let st = arcfour_init key in
  ArcFourState st

let stream_cipher_encrypt p st = 
  match st with
  | ArcFourState afst -> arcfour_encrypt p afst

(* used by TLS *)


let xor s1 s2 nb =
  if Bytearray.length s1 < nb or Bytearray.length s2 < nb then
    failwith "xor: arrays too short"
  else
    let res = Bytearray.make nb in  
    for i=0 to nb-1 do
      res.(i) <- Byte.of_int ((Byte.to_int (Bytearray.get s1 i)) ^^^ (Byte.to_int (Bytearray.get s2 i)))
    done;
    res

let pickle x = x
let unpickle x = x
