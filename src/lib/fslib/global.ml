
(* Global constants and functions *)

let debug_bool = ref false

let debug modul msg =
  if !debug_bool then begin
      print_string ("["^modul^"] ");
      print_string msg ;
      print_newline ();
      flush stdout
    end
  else ()



let keyexch = ref false
let encrypting = ref false
let macing = ref false
let caching = ref false

let ssl = ref false

let test b m =
  if b then ()
  else failwith m   

let assume = function e -> ()


type 'a rel = Before of  'a * 'a

let test_eq i j m =
  if i = j then ()
  else failwith m   

let test_inf i j m =
  if i < j then ()
  else failwith m   
