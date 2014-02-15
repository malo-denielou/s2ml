open Rpc
open Rpc_protocol
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  register alice; register bob

let start_client () =
  let res = 
    Rpc.c {prins_c = "Alice";prins_w = "Bob"} 
    (Request("Number?",
           {hReply = function (_,i)  -> i})) in
  printf "Answer is %i\n" res

let start_server () = 
  Rpc.w "Bob" 
    {hRequest = 
        function (_,query) -> 
          Reply(42,())}

let _ = Pi.fork start_server
let _ = start_client ()
