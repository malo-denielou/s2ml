(* Base64 encoding and decoding *)

let char_table = 
  [|'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';
    'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';
    'm';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'0';'1';'2';'3';'4';
    '5';'6';'7';'8';'9';'+';'/'|] 

let table_back c =
  match c with
    | 'a' .. 'z' -> Some (int_of_char c - 71)
    | 'A' .. 'Z' -> Some (int_of_char c - 65)
    | '0' .. '9' -> Some (int_of_char c + 4)
    | '+' -> Some (62)
    | '/' -> Some (63)
    | '=' -> None
    | _ -> assert false

let eq = '='
  
let base64 (s:string) =
  let size = 8*String.length s in
  let ssize = 
    (size / 24 ) * 4 + (if size mod 24 <> 0 then 4 else 0) in
  let res = String.create ssize in

(*  let _ = Printf.printf "Taille : %i donc %i\n" size ssize in *)

  let rec encode p =
    if p*8 >= size then ()
    else
      let byte1 = int_of_char (s.[p]) in
      let bits1 = (byte1 / 4) in
      let _ = res.[p*8/6] <- (char_table.(bits1)) in
      if 8*p+8 >= size 
      then 
        let bits2 = (byte1 mod 4)*16 in
        let _ = res.[p*8/6+1] <- (char_table.(bits2)) in
        ()
      else
        let byte2 = int_of_char (s.[p+1]) in
        let bits2 = (byte1 mod 4)*16 + (byte2 / 16) in
        let _ = res.[p*8/6+1] <- (char_table.(bits2)) in
        if 8*p+16 >= size 
        then
          let bits3 = (byte2 mod 16)*4 in
          let _ = res.[p*8/6+2] <- (char_table.(bits3)) in
          ()
        else 
          let byte3 = int_of_char (s.[p+2]) in
          let bits3 = (byte2 mod 16)*4 + (byte3 / 64) in
          let bits4 = (byte3 mod 64) in
          let _ = res.[p*8/6+2] <- (char_table.(bits3)) in
          let _ = res.[p*8/6+3] <- (char_table.(bits4)) in
          encode (p+3)
  in
  (* filling the last bits with equals *)
  if size mod 24 <> 0 then 
    for i = ssize - 3 to ssize-1 do
      res.[i] <- eq
    done;
  encode 0 ;
  res

    
let ibase64 (s:string) =
  let size = String.length s in
  let ssize = size * 6 / 8 in
  let res = String.create ssize in
  
  let rec decode p =
    if p = size 
    then 0
    else
      let i = (p*6)/8 in
      let v1 = table_back s.[p] in
      let v2 = table_back s.[p+1] in
      let v3 = table_back s.[p+2] in
      let v4 = table_back s.[p+3] in
      match (v1,v2,v3,v4) with
        | Some a, Some b, Some c, Some d -> 
(*            let _ = Printf.printf "Chars : %i %i %i %i\n" a b c d in
              let _ = Printf.printf "donne : %i %i %i \n" (a * 4 + b / 16) ((b
              mod 16)*16 + c / 4) ((c mod 4)*64 + d) in *)
            let _ = res.[i] <- char_of_int (a * 4 + b / 16) in
            let _ = res.[i+1] <- char_of_int ((b mod 16)*16 + c / 4) in
            let _ = res.[i+2] <- char_of_int ((c mod 4)*64 + d) in
            decode (p+4)
        | Some a, Some b, Some c, None ->
            let _ = res.[i] <- char_of_int (a * 4 + b / 16) in
            let _ = res.[i+1] <- char_of_int ((b mod 16)*16 + c / 4) in
            1
        | Some a, Some b, None, None ->
            let _ = res.[i] <- char_of_int (a * 4 + b / 16) in
            (*            let _ = res.[i+1] <- char_of_int (b mod 16) in *)
            2 
        | _ -> assert false
  in
  
  let i = decode 0 in
 String.sub res 0 (ssize-i)

(*
let base = base64_encode Sys.argv.(1)
  
let _ = Printf.printf "Base64 : %s\n" base
let _ = Printf.printf "inverse : %s\n" (base64_decode base)
*)
