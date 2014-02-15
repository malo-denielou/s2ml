
module Data

open Printf

type bytes = byte[]
type str = string

type qname = str * str * str
type att = (qname * str) 
type item = Txt of str | Xn of qname * (att list) * (item list)
type env = (str * (item list)) list



let base64 (x:bytes) : string = System.Convert.ToBase64String x
let ibase64 (x:string) : byte[] = System.Convert.FromBase64String x

let cS (x:str) = x
let iS (x:str) = x

let B (x:byte[]) = x
let iB (x:bytes) = x
  
(*---- Representations. *)

let utf8 (x:string) : bytes = System.Text.Encoding.UTF8.GetBytes x
let iutf8 (x:bytes) : string = System.Text.Encoding.UTF8.GetString x

let concatfix (x:bytes) (y:bytes) : bytes = Array.append x y
let iconcatfix (n:int)  (x:bytes) = 
  let x1 = Array.sub x 0 n in 
  let x2 = Array.sub x n (Array.length x - n) in (x1,x2) 

let size (x:bytes) = 
  let l = Array.length x in 
  // printf "\n+%i\n" l;
  [| Byte.of_int(l mod 256) ; Byte.of_int(l / 256) |] : bytes

let concatvar (x:bytes) (y:bytes) : bytes = concatfix (size x) (concatfix x y)
let iconcatvar (x:bytes) = 
  let s,x' = iconcatfix 2 x in
  let l = Byte.to_int(s.(0))+256*Byte.to_int(s.(1)) in
  // printf "\n-%i\n" l;
  iconcatfix l x'

let concat x y = concatvar x y
let iconcat z = iconcatvar z
let concat3 x y z = concat x (concat y z)
let iconcat3 e = (let x,yz = iconcat e in let y,z = iconcat yz in x,y,z)

let append x y = concatfix x y

//let concat (x:bytes) (y:bytes) : bytes = Array.append x y
//let iconcat (x:bytes) = failwith "iconcat would need an index" : bytes * bytes

let rev (x:bytes) : bytes = let y = Array.copy x in System.Array.Reverse y; y
