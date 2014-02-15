open Wsn
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="Alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="Bob"; ip="193.55.250.83"; port=8002} in
  register alice; register bob; connect "Alice" "Bob"

let start_client () =
  let rec handler_Reply income =
    {hReply = 
        (function X x ->
           if income < 10000
           then Extra (Q "Quote?",handler_Reply (income+x))
           else Extra (Q "Money!",handler_Reply (income+x)));
     hFault =
        (function () -> printf "Won %i$.\n" income)
    } in
(* Begin Client *)
  Wsn.c "Alice" 
    (Request (
       C "Alice",W "Bob",Q "Quote?",handler_Reply 0)) 
(* End Client *)


let _ = start_client ()
