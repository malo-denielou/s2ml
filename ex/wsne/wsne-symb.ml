open Wsne
open Wsne_protocol
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
           else Exit ((),printf "Won %i$.\n" (income+x)));
     hFault =
        (function _ -> printf "Won only x%i$.\n" income)
    } in
(* Begin Client *)
  Wsne.c  {prins_c = "Alice";prins_w = "Bob"}
    (Request ("Quote?",handler_Reply 0)) 
(* End Client *)


let start_server () = 
  let rec handler_Extra query =
    if query = "Quote?" 
    then 
      Reply(42,
            {hExtra = (function (_,query) ->
               handler_Extra query);
             hExit = function _ -> "Session exits\n"})
    else 
      Fault ((),"Request failed\n")
  in
  let res = 
    Wsne.w "Bob" 
      {hRequest = function (_,query) -> 
         handler_Extra query}
  in print_string res

let _ = Pi.fork start_server
let _ = start_client ()
