
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


let keyexch = ref true
let encrypting = ref true
let macing = ref true
let signing = ref false
let caching = ref false
let verifying = ref true
let tcp = ref false


let test b m =
  if b then ()
  else failwith m   

let assume = function e -> ()
let test_eq i j m =
  ()

let test_inf i j m =
  ()
