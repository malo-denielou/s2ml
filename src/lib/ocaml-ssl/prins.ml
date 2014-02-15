open Unix
open Global
open Data

(* open Route *)
(* Routing tables *)

let debug = debug "Prins"



type pr = {id:string; cert:string; ip:string; port:int}

let certDb : (string * string) list ref = ref []
let addrDb : (string * (string * int)) list ref = ref []
let revaddrDb : ((string * int) * string) list ref = ref []
let localaddrDb : (string * (string * int)) list ref = ref []

let get_address ad subject = 
  List.assoc subject ad

let udp = ref (Unix.stderr)
let tcpsock = ref []


let register (prin:pr) =
  debug ("Registering "^prin.id);
  certDb := (prin.id,prin.cert)::!certDb ;
  addrDb := (prin.id,(prin.ip,prin.port))::!addrDb ;
  revaddrDb := List.map (function (x,y) -> (y,x)) !addrDb ;
  localaddrDb := (prin.id,(prin.ip,prin.port))::!localaddrDb
  

let bind prin =
  if !tcp then begin
    let (url,port) = List.assoc prin !addrDb in
    let sock = socket PF_INET SOCK_STREAM 0 in
    setsockopt sock SO_REUSEADDR;
    bind sock (Unix.ADDR_INET (inet_addr_of_string url,port));
    listen sock 25 ;
    let listen () = 
      while true do
        let (s,(ADDR_INET(addr,port)))= accept sock in
        let orig = List.assoc (string_of_inet_addr addr,port) !revaddrDb in
        tcpsock:= (orig,s)::!tcpsock
      done in
    let _ = Thread.create listen () in
    ()
  end
  else begin
    let (_,localport) = get_address (!localaddrDb) prin in
    udp := Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0;
    Unix.setsockopt !udp SO_REUSEADDR true;
    Unix.bind !udp (Unix.ADDR_INET (Unix.inet_addr_loopback, localport))
  end

(* communication *)
let psend (dest:string) (text:str): unit = 
  let text = iS text in
  if !tcp then begin
    if (List.mem_assoc dest !tcpsock) then begin
      let sock = List.assoc dest !tcpsock in
      ignore (send sock text 0 (String.length text) []) end
    else begin
      let (url,destport) = List.assoc dest !addrDb in
      let sock = socket PF_INET SOCK_STREAM 0 in
      let rec f () =
        try 
          connect sock (ADDR_INET ((inet_addr_of_string url),destport))
        with
            _ -> (Thread.delay 0.1 ; f())
      in
      f();
      tcpsock:= (dest,sock):: !tcpsock ;
      ignore (send sock text 0 (String.length text) []) end
  end else begin
    let (url,destport) = List.assoc dest !addrDb in
    let udpSend = socket PF_INET SOCK_DGRAM 0 in
    let addr = inet_addr_of_string url in
    let _ = sendto udpSend text 0 (String.length text) [] (ADDR_INET(addr,destport)) in 
    close udpSend
  end ;
  debug ("sent:\n"^text) ;
  ()
      
let precv (self:string) : str =
  let text = String.create 2056 in
  (*   let (len,_) = recvfrom udp text 0 1024 [] in *)
  let len =
    if !tcp then begin
      let rec f () = 
        let l = List.map snd !tcpsock in
        let ls,_,_ = select l [] [] 0.1 in
        if ls != [] then (
          let sock = List.hd ls in
          recv sock text 0 2056 [])
        else f () in
      f ()
    end 
    else recv !udp text 0 2056 [] in
  debug ("received:\n"^text) ;
  cS (String.sub text 0 len)

(* principal library *)

let get_privkey s = 
  let key = List.assoc (iS s) !certDb in
  utf8 (cS (key^".key"))

let get_pubkey s = 
  let key = List.assoc (iS s) !certDb in
  utf8 (cS (key^".pub"))

let keyDb : ((string * string) * (bytes * bytes)) list ref = ref []

let get_symkey a b = 
  fst (List.assoc (a,b) !keyDb)

let get_mackey a b = snd (List.assoc (a,b) !keyDb)

let gen_keys a b = 
  if !keyexch then
   if (List.mem_assoc (a,b) !keyDb) then
      (debug ("Shared keys already generated for "^(iS a)^" and "^(iS b)); 
      (utf8 (cS "")))
   else
     let () = 
       debug ("Generating shared keys for "^(iS a)^" and "^(iS b)) in
     let ab_enc = Crypto.mkNonce() in
     let ab_mac = Crypto.mkNonce() in
     debug(Printf.sprintf "The keys for %s and %s are of size %d and %d"
             (iS a) (iS b) 
             (String.length ab_enc) (String.length ab_mac));
     let ab_enc64 = Base64.base64 ab_enc in
     let ab_mac64 = Base64.base64 ab_mac in
     debug ("The keys for "^(iS a)^" and "^(iS b)^" are: "^
              spbytes ab_enc^" and "^spbytes ab_mac);
     keyDb := ((a,b),(ab_enc,ab_mac))::!keyDb;
     debug ("Getting private/public keys");
     let signk = get_privkey (cS a) in 
     let pubk  = get_pubkey (cS b) in  
     debug ("Encrypting and signing the shared keys.");
     let ab = concat (concat (utf8 a) (utf8 b)) (concat ab_enc ab_mac) in
     let encmsg = Crypto.rsa_encrypt pubk ab in
     debug ("The cyphered keys are: "^spbytes (base64 encmsg));
     let res = 
       concat 
         (Base64.base64 encmsg) 
         (Base64.base64 (Crypto.rsa_sign signk encmsg)) in
     debug ("The keys are: "^spbytes res);
     res
  else
    if (List.mem_assoc (a,b) !keyDb) then
      (debug ("Shared keys already generated for "^(iS a)^" and "^(iS b)); 
       (utf8 (cS "")))
    else
      begin keyDb := ((a,b),(utf8 (cS ""),utf8 (cS "")))::!keyDb;
        concat (utf8 (cS "")) (utf8 (cS "")) end

let reg_keys a b msg =
  debug ("Registering keys ") ;
  if !keyexch then
    if (List.mem_assoc (a,b) !keyDb )
    then
      debug ("Shared keys for "^(iS a)^" and "^(iS b)^" already registered.")
    else
      let signk = get_privkey (cS b) in 
      let pubk  = get_pubkey (cS a) in  
     debug ("The received keys are: "^spbytes msg);
      let encmsg,signedmsg = iconcat msg in
      Crypto.rsa_verify pubk (Base64.ibase64 encmsg) (Base64.ibase64 signedmsg);
      debug ("The cyphered keys are: "^spbytes (encmsg));
      let decmsg = Crypto.rsa_decrypt signk (Base64.ibase64 encmsg) in
      debug ("The decyphered keys are: "^spbytes (decmsg));
      let ab,ab_cry = iconcat decmsg in
      let ab_enc,ab_mac = iconcat ab_cry in
      let ia,ib = iconcat ab in
      let () = test_eq a (iutf8 ia) ""; test_eq b (iutf8 ib) "" in
      debug(Printf.sprintf "The keys for %s and %s are of size %d and %d"
              (iS a) (iS b) 
              (String.length ab_enc) (String.length ab_mac));
      let ab_enc64 = Base64.base64 ab_enc in
      let ab_mac64 = Base64.base64 ab_mac in
      keyDb := ((a,b),(ab_enc,ab_mac))::!keyDb;
      debug ("Registering shared keys for "^(iS a)^" and "^(iS b)
             ^": "^(spbytes ab_enc64)^" and "^(spbytes ab_mac64));
      ()
  else
    if (List.mem_assoc (a,b) !keyDb )
    then
      debug ("Shared keys for "^(iS a)^" and "^(iS b)^" already registered.")
    else
      keyDb := ((a,b),(utf8 (cS ""),utf8 (cS "")))::!keyDb

(*
let _ =
 let ab_enc = Crypto.mkNonce() in
 let ab_mac = Crypto.mkNonce() in
   keyDb := (("Alice","Bob"),(ab_enc,ab_mac))::!keyDb;
   keyDb := (("Bob","Alice"),(ab_enc,ab_mac))::!keyDb
*)


let bind prin =
  let (_,localport) = get_address (!localaddrDb) prin in
  udp := Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0;
  Unix.setsockopt !udp SO_REUSEADDR true;
  Unix.bind !udp (Unix.ADDR_INET (Unix.inet_addr_loopback, localport))

let close () = Unix.close !udp

(* Cache handling *)

let opened = ref []

let check_cache (prin:string) (role:string) (sid:bytes) = 
  debug ("Checking the cache of "^(spstr (base64 sid))) ;
  if !caching then
    let rec read_lines chan =
      let s = 
        try Some (input_line chan) with
            End_of_file -> None in
      match s with
          Some m -> (m)::(read_lines chan)
        | None -> [] in 
    let l = if List.mem_assoc (prin,role) !opened 
    then 
      let f = Unix.openfile (prin^(role)^".cache") [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
      let ch = Unix.in_channel_of_descr f in
      let l = read_lines ch in
      let () = Unix.close f in
      let () = opened := ((prin,role),l)::!opened in
      l
    else List.assoc (prin,role) !opened in
    if List.mem (spstr (base64 sid)) l 
    then (
      let f = Unix.openfile (prin^(role)^".cache") [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
      List.iter (fun x -> ignore (Unix.write f x 0 (String.length x))) l ;
      Unix.close f;
      failwith "cache error" )
    else (
      debug ("Adding to the cache "^(spstr (base64 sid))) ;
      let sid_string = (spstr (base64 sid))^"\n" in
      opened := ((prin,role),sid_string::l)::(List.remove_assoc (prin,role) !opened)
      )
  else ()


let connect _ _ =()
let listen _ =()

let close () = 
  Unix.close !udp ; 
  List.iter (function ((prin,role),l) -> let f = Unix.openfile (prin^(role)^".cache")
               [Unix.O_RDWR; Unix.O_CREAT] 0o640 in 
             List.iter (fun x -> ignore (Unix.write f x 0 (String.length x))) l ;
             Unix.close f) !opened
