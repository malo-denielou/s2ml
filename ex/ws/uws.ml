open Ws
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="Alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="Bob"; ip="127.0.0.1"; port=8002} in
  register alice; register bob; listen "Bob"


let start_server () = 
  let res = 
(* Begin W *)
    Ws.w "Bob" 
      {hRequest = function (_,_,Q query) -> 
         if query = "Quote?" 
         then Reply(X 42,"Reply sent\n")
         else Fault ("Request failed\n")
      }
(* End W *)
  in print_string res

let _ = for i = 1 to 5000 do start_server () done
