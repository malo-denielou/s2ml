open Unix

(* external openssl_base64: string -> string -> unit = "base64" *)
(* external openssl_ibase64: string -> string -> unit = "ibase64" *)

let base64 s = Base64.base64 s
let ibase64 s = Base64.ibase64 s

let utf8 s = s
let iutf8 s = s
let concat x y = 
  let cv i = (if i<10 then "000" else 
                (if i<100 then "00" else
	           (if i<1000 then "0" else "")))
    ^(string_of_int i) in
  let ret = 
    (cv (String.length x + String.length y))
    ^(cv (String.length x))
    ^x^y in
(*    Printf.printf "Concatenating %d + %d = %d\n" (String.length x) (String.length y) (String.length ret); *) ret

let iconcat z = 
(*  Printf.printf "Attempting decat %d: " (String.length z);*)
  let total = int_of_string (String.sub z 0 4) in
  let shift = int_of_string (String.sub z 4 4) in
(*  Printf.printf "shift: %d\n " (shift);*)
  let x1 = String.sub z 8 shift in
  let x2 = String.sub z (shift+8) (total-shift) in 
(*    Printf.printf "success %d %d!\n" (String.length x1) (String.length x2);*)
    (x1,x2)
								     
let concat3 x y z = concat x (concat y z)
let iconcat3 e = (let x,yz = iconcat e in let y,z = iconcat yz in x,y,z)

type str = string
type bytes = string

let spstr s = s
let spbytes b = b

let cS (x:string) : str = x
let iS (x:str) : string = x
