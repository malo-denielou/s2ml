open Printf

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
      (*    | DerivedKey of bytes * bytes     PSHA-1(basekey,nonce)/(utf8 pwd,seed) *)
  | AsymSign of bytes * bytes
  | AsymEncrypt of bytes * bytes
  | SymEncrypt of bytes * bytes
  | Mac of bytes * bytes

let pickle x = x
let unpickle x = x

let hash s = Bin (Hash s)
let ihash s = match s with Bin (Hash ss) -> ss | _ -> assert false
let sign k s = Bin (AsymSign (k,s))
let isign s = match s with Bin(AsymSign (k,t)) -> (k,t) | _ -> assert false
let encr k s = Bin (SymEncrypt (k,s))
let asym_encr k s = Bin (AsymEncrypt (k,s))
let iencr s = match s with Bin(SymEncrypt (k,t)) -> (k,t) | _ -> assert false
let asym_iencr s = match s with Bin(AsymEncrypt (k,t)) -> (k,t) | _ -> assert false
let mac k s = Bin (Mac (k,s))
let imac s = match s with Bin(Mac (k,t)) -> (k,t) | _ -> assert false

let cS s = Literal s
let iS s = match s with
  | Literal s -> s
  | _ -> failwith "iS failed"

let base64 b = Base64(b)
let ibase64 = function
  | Base64 s -> s
  | _ -> failwith "ibase64 failed"

let append x y = Concat(x,y)
  
let concat x y = Concat(x,y)
let iconcat s = match s with
  | Concat(x,y) -> (x,y)
  | _ -> failwith "iconcat failed"
let concat3 x y z = concat x (concat y z)
let iconcat3 e = (let x,yz = iconcat e in let y,z = iconcat yz in x,y,z)

let utf8 x = Utf8 x
let iutf8 b = match b with
  | Utf8 s -> s
  | _ -> failwith "iutf8 failed"


let rec spstr s = match s with
  | Base64(t) -> sprintf "%s" (spbytes t)
  | Literal(s) -> sprintf "%s" s
and spblob b = 
  let keyed name key arg = sprintf "%s{%s}[%s]" name (spbytes key) (spbytes arg) in
  match b with
    | AsymSign(k,t) -> keyed "RSA-SHA1" k t
    | AsymEncrypt(k,t) -> keyed "RSA-Enc" k t
    | Hash(s) -> sprintf "SHA1(%s)" (spbytes s)
    | Nonce(s) -> sprintf "Nonce(%s)" (spbytes s)
    | SymEncrypt(k,t) -> keyed "Enc" k t
    | Mac(k,t) -> keyed "Mac" k t
and spbytes b = 
  match b with
    | Utf8(s) -> sprintf  "\'%s\'" (spstr s)
    | Concat(x,y) -> sprintf "%s | %s" (spbytes x) (spbytes y)
    | Bin(b) -> sprintf "%s" (spblob b)
and spmsg m =
  match m with
    | Base64(
        Concat(
          Concat(sid,Utf8(Literal ts)),
          Concat
            (Utf8(Literal visib),
             Concat(
               Utf8(Literal payload),
               Concat(macs,
                      keys))))) -> 
        sprintf "  Message nb %s '%s':\n  Payload = %s\n  Keys = %s\n  MACS = %s"
          ts
          visib
          payload
          (splist spkey keys)
          (splist (spmac "\n   - ") macs)
    | Base64(
        Concat(
          Concat(
            Concat(sid,Utf8(Literal ts)),
            prins),
          Concat
            (Utf8(Literal visib),
             Concat(
               Utf8(Literal payload),
               Concat(macs,
                      keys))))) -> 
        sprintf "  Initial message nb %s '%s':\n  Principals = %s\n  Payload = %s\n  Keys = %s\n  MACS = %s"
          ts
          visib
          (splist spvar prins)
          payload
          (splist spkey keys)
          (splist (spmac "\n   - ") macs)
    | _ -> assert false
and splist f = function
  | Concat(e,Utf8(Literal "")) -> f e
  | Concat(e,q) -> f e ^","^(splist f q)
  | Utf8(Literal "") -> ""
  | _ -> assert false
and spvar = function
  | Bin (SymEncrypt(Utf8(Literal k),Utf8(Literal s))) -> sprintf "Enc{%s}[%s]" k
      s
  | Utf8 (Literal s) -> sprintf "'%S'" s
  | _ -> assert false
and spmmac = function
  | Bin
      (Mac 
	 (Utf8(Literal k),
	  Concat
            (Concat(sid,Utf8(Literal ts)),  
             Utf8(Literal label))))   -> 
      sprintf "Mac{%s} of message %s (ts: %s)"
        k label ts
  | _ -> assert false
and spmac head = function
  | Concat(
      Concat(sid,Utf8(Literal ts)),
      Bin
	( Mac 
	    (Utf8(Literal k),
             Concat
	       (_,Utf8(Literal label)))))  ->
      let sid = if String.length (spbytes sid) < 15 
      then (spbytes sid)^"]" 
      else (String.sub (spbytes sid) 0 15)^"...]" in
      sprintf "%sMac{%s} of message %s (sid: [%s, ts: %s)"
        head k label sid ts
  | Utf8(Literal "") -> "Error: MAC not known when forwarded"
  | _ -> assert false
and sphash = function
  | Bin(Hash(Utf8(Literal v))) -> sprintf "Sha1(%S)" v 
  | _ -> assert false
and spkey = function
  | Concat(Bin(key),_) -> spblob key
  | Utf8(Literal k) -> ""
  | _ -> assert false
