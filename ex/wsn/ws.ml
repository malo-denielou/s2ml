open Wsn
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="Alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="Bob"; ip="193.55.250.83"; port=8002} in
  register alice; register bob; listen "Bob"

let start_server () = 
  let rec handler_Extra =
    {hExtra = function (Q query) ->
         if query = "Quote?" 
         then Reply(X 1,handler_Extra)
         else Fault ("Request finished\n")
    } in
  let res = 
    Wsn.w "Bob" 
      {hRequest = function (_,_,Q query) -> 
       if query = "Quote?" 
         then Reply(X 1,handler_Extra)
         else Fault ("Request finished\n")
      }
  in print_string res

let _ = start_server ()
