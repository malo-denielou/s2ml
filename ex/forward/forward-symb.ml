open Forward
open Forward_protocol
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
  register alice; register bob; register charlie

let start_client () =
  let res = 
    Forward.c {prins_c = "Alice";prins_p = "Bob";prins_w = "Charlie"} 
    (Request("Number?",
           {hReply = function (_,i)  -> i})) in
  printf "Answer is %i\n" res

let start_p () = 
  Forward.p "Bob" 
    {hRequest = 
        function (_,query) -> 
          Forward(query,())}

let start_w () = 
  Forward.w "Charlie" 
    {hForward = 
        function (_,query) -> 
          Reply(42,())}

let _ = Pi.fork start_w
let _ = Pi.fork start_p
let _ = start_client ()
