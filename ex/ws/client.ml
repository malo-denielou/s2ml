open Ws
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="Alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="Bob"; ip="127.0.0.1"; port=8002} in
  register alice; register bob; connect "Alice" "Bob"

let start_client () =
(* Begin Client *)
  Ws.c "Alice" 
    (Request (
       C "Alice",W "Bob",Q "Quote?",{
         hReply = (fun (X x) -> printf "Quoted at %i.\n" x) ;
         hFault = (fun () -> printf "Failed request.\n") })) 
(* End Client *)


let _ = for i = 1 to 5000 do start_client () done
