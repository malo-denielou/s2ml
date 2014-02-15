open Global
open Data

(* let cacheDb : (string * ()) list ref = ref []  *)
let debug_prins = debug "prins"

type pr = {id:string; cert:string; ip:string; port:int}

let register (prin:pr) =
  Pi.create_chan prin.id

let precv  (self:string) =
  let text = Pi.recv self in
  debug_prins (self^" received the message!") ;
  text

let psend  (dest:string) (text:str): unit = 
  debug_prins ("sent to "^dest^": "^(spmsg text)) ;
  Pi.send dest text

let get_privkey s = utf8 (cS ((iS s)^".key"))
let get_pubkey s = utf8 (cS ((iS s)^".pub"))


let bind (self:string) = 
  ()


let bind_r (self:string) = 
  ()

let close () = 
  ()

let check_cache (prin:string) (role:string) (sid:bytes) = 
  ()
(*  commenting out as it always fails in symbolic mode...
    let rec read_lines chan =
    let s = 
      try Some (input_line chan) with
          End_of_file -> None in
    match s with
        Some m -> (m)::(read_lines chan)
      | None -> [] in 
  let f = Unix.openfile (prin^(role)^".cache") [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
  let ch = Unix.in_channel_of_descr f in
  let l = read_lines ch in
  if List.mem (spstr (base64 sid)) l 
  then (
      Unix.close f;
      failwith "cache error" )
  else (
    debug_prins ("Adding sid to cache: ..."(*^(spstr (base64 sid)) *) ) ;
    let sid_string = (spstr (base64 sid))^"\n" in
    let _ = Unix.write f sid_string 0 (String.length sid_string) in
    Unix.close f)*)


let keyDb : ((Data.str * Data.str) * ((*bytes **) bytes)) list ref = ref []

(*let get_simkey s t = utf8 (cS ((iS s)^(iS t)^".sym"))*)

let get_symkey a b = 
  debug "Prins" ("Looking for shared symenc key between "^(iS a)^" and "^(iS b));
  let ab_enc = (*fst*) (List.assoc (a,b) !keyDb) in
  debug "Prins" ("Found "^(spbytes ab_enc));
  ab_enc

let get_mackey a b = 
  debug "Prins" ("Looking for shared mac key between "^(iS a)^" and "^(iS b));
  let ab_mac = (*snd*) (List.assoc (a,b) !keyDb) in
  debug "Prins" ("Found "^(spbytes ab_mac));
  ab_mac

let gen_keys a b = 
    if (List.mem_assoc (a,b) !keyDb) then
      (debug "Prins" ("Shared keys already generated for "^(iS a)^" and "^(iS b)); 
      (utf8 (cS "")))
    else
      (*let ab_enc = Crypto.mkNonce() in*)
      let ab_mac = Crypto.mkNonce() in
      debug "Prins" ("Generating shared keys for "^(iS a)^" and "^(iS b)^": "
                       (*^spbytes ab_enc^","*)^spbytes ab_mac);
      keyDb := ((a,b),((*ab_enc,*)ab_mac))::!keyDb;
      let signk = get_privkey a in 
      let pubk  = get_pubkey b in  
      let ab = concat (concat (utf8 a) (utf8 b)) ab_mac in
      let encmsg = Crypto.rsa_encrypt pubk ((*concat ab_enc*) ab) in
      let msg = concat encmsg (Crypto.rsa_sign signk encmsg) in
      debug "Prins" ("Encrypted key: "^spbytes msg); 
      msg

let reg_keys a b msg =
  if (List.mem_assoc (a,b) !keyDb )
  then
    debug "Prins" ("Shared keys for "^(iS a)^" and "^(iS b)^" already registered.")
  else
    let _ = debug "Prins" 
      ("Registering keys for "^(iS a)^" and "^(iS b)^".") in
    let signk = get_privkey b in 
    let pubk  = get_pubkey a in  
    let encmsg,signedmsg = iconcat msg in
    Crypto.rsa_verify pubk encmsg signedmsg;
    let (*decmsg*) ab = Crypto.rsa_decrypt signk encmsg in
    let ab,ab_mac = iconcat ab in
    let ia,ib = iconcat ab in
    let () = test_eq a (iutf8 ia) ""; test_eq b (iutf8 ib) "" in
    (*let ab_enc,ab_mac = iconcat decmsg in*)
    keyDb := ((a,b),((*ab_enc,*)ab_mac))::!keyDb;
    debug "Prins" 
      ("Shared keys registered : "^(*spbytes ab_enc^","^*)spbytes ab_mac);
    ()
 
            
