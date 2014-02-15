
(* Global constants and functions *)

let debug_bool = ref true 
let debug_crypto_bool = ref true
let debug_prins_bool = ref true

let set_debug b = debug_bool := b

let debug modul msg =
  if !debug_bool then begin
      print_string ("["^modul^"] ");
      print_string msg ;
      print_newline ();
      flush stdout
    end
  else ()

let crypto_on = ref true
let signing = ref true
let verifying = ref true

type rel = Before of  int * int

let test_eq i j m =
  if i = j then ()
  else failwith m   

let test_inf i j m =
  if i < j then ()
  else failwith m   
