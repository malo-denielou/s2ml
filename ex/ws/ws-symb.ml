open Ws
open Ws_protocol
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="Alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="Bob"; ip="127.0.0.1"; port=8002} in
  register alice; register bob

let start_client () =
(* Begin Client *)
  Ws.c  {prins_c = "Alice";prins_w = "Bob"}  
    (Request ("Quote?",{
         hReply = (function (_,x) -> printf "Quoted at %i.\n" x) ;
         hFault = (fun _ -> printf "Failed request.\n") })) 
(* End Client *)


let start_server () = 
  let res = 
(* Begin W *)
    Ws.w "Bob" 
      {hRequest = function (_,query) -> 
         if query = "Quote?" 
         then Reply(42,"Reply sent\n")
         else Fault ((),"Request failed\n")
      }
(* End W *)
  in print_string res

let _ = Pi.fork start_server
let _ = start_client ()
