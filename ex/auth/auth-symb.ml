open Auth
open Auth_protocol
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
  let don = {id="Don"; cert="charlie"; ip="127.0.0.1"; port=8004} in
  register alice; register bob; register charlie; register don

let start_client () =
  let res = 
    Auth.c {prins_c = "Alice";prins_a = "Bob";prins_w = "Charlie";prins_d = "Don";} 
    (Request("Number?",
           {hReply = function (_,i)  -> i})) in
  printf "Answer is %i\n" res

let start_a () = 
  Auth.a "Bob" 
    {hRequest = 
        function (_,query) -> 
          Auth((),
               {hOk = 
                   function _ -> Forward(query,())})}

let start_w () = 
  Auth.w "Charlie" 
    {hForward = 
        function (_,query) -> 
          Reply(42,())}

let start_d () =
  Auth.d "Don"
    {hAuth =
        function (prins,_) ->
          if prins.prins_c <> "bob" 
          then Ok ((),())
          else failwith "Wrong client"}

let _ = Pi.fork start_w
let _ = Pi.fork start_a
let _ = Pi.fork start_d
let _ = start_client ()
