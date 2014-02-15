open Wsn
open Wsn_protocol
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="Alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="Bob"; ip="127.0.0.1"; port=8002} in
  register alice; register bob

let start_client () =
  let rec handler_Reply income =
    {hReply = 
        (function (_,x) ->
           if income < 50 
           then Extra ("Quote?",handler_Reply (income+x))
           else Extra ("Money!",handler_Reply (income+x)));
     hFault =
        (function _ -> printf "Won %i$.\n" income)
    } in
(* Begin Client *)
  Wsn.c  {prins_c = "Alice";prins_w = "Bob"}
    (Request ("Quote?",handler_Reply 0)) 
(* End Client *)


let start_server () = 
  let rec handler_Extra query =
    if query = "Quote?" 
    then 
      Reply(42,
            {hExtra = function (_,query) ->
               handler_Extra query})
    else 
      Fault ((),"Request failed\n")
  in
  let res = 
    Wsn.w "Bob" 
      {hRequest = function (_,query) -> 
         handler_Extra query}
  in print_string res

let _ = Pi.fork start_server
let _ = start_client ()
