module Prins

open Data
open Crypto
open System.Net
open Global

open System.Security.Cryptography.X509Certificates
open System.Security.Cryptography

type pr = { id:string; cert:string; ip:string; port:int}

type conn = Sock of Sockets.NetworkStream
type sslconn = SslSock of Security.SslStream


let addrDb : (string * (string * int)) list ref = ref []
let connDb : (string * conn) list ref = ref []
let sslconnDb : (string * sslconn) list ref = ref []

let localaddrDb : (string * (string * int)) list ref = ref []


let register (prin:pr) =
  addrDb := (prin.id,(prin.ip,prin.port))::!addrDb ;
  localaddrDb := (prin.id,(prin.ip,prin.port))::!localaddrDb

let ValidateServerCertificate _ _ _ _ = true

let connect s u = 
  let (hs, ps) =  List.assoc u !addrDb in
  let uri = new System.Uri("http://"^hs^":"^(string_of_int ps)) in
  let h = uri.Host in
  let p = uri.Port in
  let c = new Sockets.TcpClient(h,p) in
    c.GetStream().Write(utf8 s,0,(utf8 s).Length);
    if !ssl then
      let v = new Security.RemoteCertificateValidationCallback  (ValidateServerCertificate) in
      let newStream =  new Security.SslStream(c.GetStream(), false, v, null) in
	newStream.AuthenticateAsClient("BenchSSL");
	Printf.printf "registering SSL conn: %s\n" u;
	sslconnDb := (u,SslSock newStream)::!sslconnDb;
	sslconnDb := (s,SslSock newStream)::!sslconnDb
    else
      connDb := (u,Sock (c.GetStream()))::!connDb
     
let listen u =
  let (hs, ps) =  List.assoc u !addrDb in
  let uri = new System.Uri("http://"^hs^":"^(string_of_int ps)) in
  let h = IPAddress.Parse(uri.Host) in
  let p = uri.Port in
  let sock = new Sockets.TcpListener (h,p) in
    sock.Start (); 
    let tcp = sock.AcceptTcpClient () in
      
    let lbuf:byte[] = Bytearray.make 18438 in   
    let l = tcp.GetStream().Read(lbuf,0,18438) in 
    let r = iutf8 (Bytearray.sub lbuf 0 l) in

      if !ssl
      then
	let newStream = new Security.SslStream(tcp.GetStream(), false) in
	let serverCertificate = X509Certificate.CreateFromCertFile("bench_ssl.cer") in
	  newStream.AuthenticateAsServer(serverCertificate, 
					false, System.Security.Authentication.SslProtocols.Tls, false  ); 
	Printf.printf "registering SSL conn: %s\n" u;
	sslconnDb := (u,SslSock newStream)::!sslconnDb;
	sslconnDb := (r,SslSock newStream)::!sslconnDb
      else      
	connDb := (r, Sock (tcp.GetStream()))::!connDb

let psend  (subj:string) (ms:string) = 
  let m = utf8 ms in
    if !ssl
    then
      begin
      let SslSock t  =  List.assoc subj !sslconnDb in
	t.Write(m,0,m.Length)
      end
    else
      let Sock t  =  List.assoc subj !connDb in
	t.Write(m,0,m.Length)

let rec precv  (subj:string) = 
  let rec look_conn ls : string = 
    match ls with
	[] -> ""
      | (j,Sock t)::ls' ->
	  if t.DataAvailable then
	    let lbuf:byte[] = Bytearray.make 18438 in   
	    let l = t.Read(lbuf,0,18438) in 
	      iutf8 (Bytearray.sub lbuf 0 l)
	  else
	    look_conn ls'	    
  in
  let look_sslconn ls : string =  (* only works for binary sessions as there's no DataAvailable for SSL streams *)
    let SslSock t = List.assoc subj ls in
    let lbuf:byte[] = Bytearray.make 18438 in   
    let l = t.Read(lbuf,0,18438) in 
      iutf8 (Bytearray.sub lbuf 0 l)
  in
    if !ssl then
      let msg = look_sslconn !sslconnDb in
	if msg="" then precv subj else msg
    else
      let msg = look_conn !connDb in
	if msg="" then precv subj else msg

(* 18437 = 2^14+2048; see rfc2246 sec.6.2(.3) *)


exception Cert_Not_Found of string

type principalX = 
       {subject:str;
	cert: bytes;
	pubkey: key;
	privkey: key;}

let x509pubkey (x:bytes) : bytes = 
  let c = new System.Security.Cryptography.X509Certificates.X509Certificate (x) in
  c.GetPublicKey()

let principalX (subject:str) (cert:bytes) (pubkey:key) (privkey:key) = 
{subject=subject;cert=cert;pubkey=pubkey;privkey=privkey}
let subject px = px.subject
let cert px = px.cert
let pubkey px = px.pubkey
let privkey px = px.privkey

let genX509 (u:str) : unit = failwith "Disabled in concrete code. Use makecert.exe or acquire and install a signed certificate."

open System.Security.Cryptography.X509Certificates
open System.Security.Cryptography
let localMachine:X509Store = 
  new X509Store(StoreName.My,StoreLocation.LocalMachine) 
let currentUser:X509Store = 
  new X509Store(StoreName.My,StoreLocation.CurrentUser) 
   
let getX509Certificate (store:X509Store) (subject:string) :  X509Certificate2 = 
  store.Open(OpenFlags.ReadOnly);
  let certs = store.get_Certificates() in
  let search = certs.Find(X509FindType.FindBySubjectName,subject,false) in
  if (search.get_Count()) < 1 then raise (Cert_Not_Found subject )
  else  
    let cert = search.Item(0) in
      cert

let getX509Prin (store:X509Store) (subject:string) : principalX =
  let cert = getX509Certificate store subject in
    if (cert.get_HasPrivateKey()) = false then raise (Cert_Not_Found subject )
    else  
      let priv = (cert.get_PrivateKey()).ToXmlString(true) in
	principalX subject (cert.Export(X509ContentType.Cert))
	  (asympub priv)
	  (asympriv priv)

let getX509Cert (subject:string) : bytes = 
  let c = try (getX509Certificate currentUser subject) with 
           _ -> getX509Certificate localMachine subject in
    c.Export(X509ContentType.Cert)

let getX509 (subj:str) = 
  try (getX509Prin currentUser subj)
  with _ -> getX509Prin localMachine subj

let get_pubkey_i (u:str) = 
  let c = getX509Cert u in
    asympubkey (x509pubkey c)

let get_privkey_i (u:str) = 
  let p = getX509 u in
    p.privkey






 
let ix509 (x:bytes) : string * string * bytes =
  let c = new System.Security.Cryptography.X509Certificates.X509Certificate (x) in
  (c.Subject, c.GetKeyAlgorithm (), c.GetPublicKey ())

let privkey1 = ibase64 (cS
"MIICXQIBAAKBgQC+L6aB9x928noY4+0QBsXnxkQE4quJl7c3PUPdVu7k9A02hRG4
81XIfWhrDY5i7OEB7KGW7qFJotLLeMec/UkKUwCgv3VvJrs2nE9xO3SSWIdNzADu
kYh+Cxt+FUU6tUkDeqg7dqwivOXhuOTRyOI3HqbWTbumaLdc8jufz2LhaQIDAQAB
AoGAIbKXshdzP8Qe1iIsctaAYlzC2IrBEhQLpoH4cFNi6LZFUQ+q4DZdULTHt5Aj
VmvaQlkHGJMXiNCMwPiZhbtrIVb6EaC+3leFQntQH9989uBOOvnGOYK8TYR7fy8g
p2SnIrbwnK81xMVHOZ+ip4F9JjCoJFu7JGQ8LJys4clFfMECQQDdm0DHgEkFTqnX
6gXRYAvzT5ymNnLg4uoeufUDdZfpIF98sHWeyin7SaNRvZeOUjfWKZMhHfcxDujz
BWrnyb2VAkEA27QFsZNIvAU91EzVgk//W1EETj3BmWoMMJ0RQcl+4KZn5H9SJiL2
1lV0qV+vPyI240pE8NZ4/2I9TcBWdJ8XhQJBAMz4UaV1QZT4Gl5L5c+wH72PsFuI
Vx8sotGCvF3TwYtuVwOUj2YspUH47Weojeqkn2KmF6D0/3NP0CUfqK71STUCQQDY
UTzV5Y1UythgwrHqF3f2YiOaIcYPllGgP9Xw70LEUDO00AODVyOwTv2JkpOK1tYy
HjsY4iLKWODBQCIfu1C9AkBS+s04OsH3jJRGc74KsAhqC7r056rb8oOOc+6X3K4N
nB3YAlyMvZBK3I8BorugTnrxANak1kZPm79LZJ1r3M/7")

let cert1 = ibase64 (cS
"MIIBxDCCAW6gAwIBAgIQxUSXFzWJYYtOZnmmuOMKkjANBgkqhkiG9w0BAQQFADAW
MRQwEgYDVQQDEwtSb290IEFnZW5jeTAeFw0wMzA3MDgxODQ3NTlaFw0zOTEyMzEy
MzU5NTlaMB8xHTAbBgNVBAMTFFdTRTJRdWlja1N0YXJ0Q2xpZW50MIGfMA0GCSqG
SIb3DQEBAQUAA4GNADCBiQKBgQC+L6aB9x928noY4+0QBsXnxkQE4quJl7c3PUPd
Vu7k9A02hRG481XIfWhrDY5i7OEB7KGW7qFJotLLeMec/UkKUwCgv3VvJrs2nE9x
O3SSWIdNzADukYh+Cxt+FUU6tUkDeqg7dqwivOXhuOTRyOI3HqbWTbumaLdc8juf
z2LhaQIDAQABo0swSTBHBgNVHQEEQDA+gBAS5AktBh0dTwCNYSHcFmRjoRgwFjEU
MBIGA1UEAxMLUm9vdCBBZ2VuY3mCEAY3bACqAGSKEc+41KpcNfQwDQYJKoZIhvcN
AQEEBQADQQAfIbnMPVYkNNfX1tG1F+qfLhHwJdfDUZuPyRPucWF5qkh6sSdWVBY5
sT/txBnVJGziyO8DPYdu2fPMER8ajJfl")


let get_pubkey (subj:str) =
(*  let (subject, cert, pubk, privk) = getPrinFromStore localMachine subj in
    pubk  *)
  let (s,a,p) = ix509 cert1 in asympubkey p

	   
let get_privkey (subj:str) =
(*  showStore ();
  let (subject, cert, pubk, privk) = getPrinFromStore localMachine subj in 
    privk *)
  asymprivkey privkey1

let keyDb : ((string * string) * (bytes * bytes)) list ref = ref []

let gen_keys a b = 
  if !keyexch then
   if (List.mem_assoc (a,b) !keyDb) then
      (debug "Prins" ("Shared keys already generated for "^(iS a)^" and "^(iS b));
      (utf8 (cS "")))
   else
     let ab_enc = Crypto.mkNonce() in
     let ab_mac = Crypto.mkNonce() in
       debug "Prins" ("Generating shared keys for "^(iS a)^" and "^(iS b));
       keyDb := ((a,b),(ab_enc,ab_mac))::!keyDb;
       let signk = get_privkey (cS a) in 
       let pubk  = get_pubkey (cS b) in  
       let encmsg = Crypto.rsa_encrypt pubk (concat ab_enc ab_mac) in
	 concat  encmsg  (Crypto.rsa_sign signk encmsg)
  else
    begin keyDb := ((a,b),(utf8 (cS ""),utf8 (cS "")))::!keyDb;
      concat (utf8 (cS "")) (utf8 (cS "")) end

let reg_keys a b msg =
  if !keyexch then
    if (List.mem_assoc (a,b) !keyDb )
    then
      debug "Prins" ("Shared keys for "^(iS a)^" and "^(iS b)^" already registered.")
    else
      let signk = get_privkey (cS b) in 
      let pubk  = get_pubkey (cS a) in  
      let encmsg,signedmsg = iconcat msg in
	Crypto.rsa_verify pubk  encmsg  signedmsg;
	let decmsg = Crypto.rsa_decrypt signk  encmsg in
	let ab_enc,ab_mac = iconcat decmsg in
	  keyDb := ((a,b),(ab_enc,ab_mac))::!keyDb;
	  debug "Prins" ("Registering shared keys for "^(iS a)^" and "^(iS b));
  else
    keyDb := ((a,b),(utf8 (cS ""),utf8 (cS "")))::!keyDb


let get_symkey a b = 
  fst (List.assoc (a,b) !keyDb)

let get_mackey a b = snd (List.assoc (a,b) !keyDb)


let check_cache (prin:string) (role:string) (sid:bytes) = 
()




let getPrinFromStore (store:X509Store) (subject:string)  =
  store.Open(OpenFlags.ReadOnly);
  let certs = store.get_Certificates() in
  let search = certs.Find(X509FindType.FindBySubjectName,subject,false) in 
  if (search.get_Count()) < 1 then raise (Cert_Not_Found subject )
  else  
    let cert = search.Item(0) in
      if (cert.get_HasPrivateKey()) = false then raise (Cert_Not_Found ("No priv key:"^subject) )
      else  
	let priv = (cert.get_PrivateKey()).ToXmlString(true) in
	  (subject, (cert.Export(X509ContentType.Cert)),
	    (asympub priv),
	    (asympriv priv))

let showStore ()= 
  let printCert (c:X509Certificate2) = 
    let str = "SUBJECT: "^c.Subject^" - ISSUER: "^c.Issuer^" - Privatekey: "^(if c.HasPrivateKey then "yes" else "no") in 
(*    let str = c.ToString() in*)
    (Printf.printf "%s\n" str) in
  let (mach:X509Store) = new X509Store(StoreName.My,StoreLocation.LocalMachine) in
    mach.Open(OpenFlags.ReadOnly);
    let certs = mach.get_Certificates() in
    let n = certs.get_Count()-1 in
      Printf.printf "\nCurrent Certificate Store\n";
      Printf.printf "*************************\n";
      for i=0 to n do
	(printCert (certs.Item(i))) done


let get_address a subject = 
  List.assoc subject a

let udp = ref (stderr)

let bind prin = 
 ()


let close () = ()

(* preliminary steps done elsewhere: 

C:\Program Files\Microsoft Visual Studio 8\SDK\v2.0>Makecert -n "CN=client" client.cer
Succeeded

C:\Program Files\Microsoft Visual Studio 8\SDK\v2.0>Makecert -n "CN=server" server.cer
Succeeded

C:\Program Files\Microsoft Visual Studio 8\SDK\v2.0>cp client.cer "c:\Documents and Settings\t-ricarc\inria-msr-sec\lang-sec\session-types\src\crypto"

C:\Program Files\Microsoft Visual Studio 8\SDK\v2.0>cp server.cer "c:\Documents and Settings\t-ricarc\inria-msr-sec\lang-sec\session-types\src\crypto"
*)


let rec find_destination label role_info = 
  let rec isin label list = match list with [] ->  false | a::l -> label=a || isin label l in
  let rec find label count role_info =
  match role_info with
    | [] -> assert false
    | l::r -> if isin label l then count else find label (count+1) r in
    find label 0 role_info



