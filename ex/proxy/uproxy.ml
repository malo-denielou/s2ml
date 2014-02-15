open Proxy
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
  register alice; register bob; register charlie; listen "Bob"; connect "Bob" "Charlie"



let start_proxy () =
  let rec handler_details n =
    {hDetails = function D d ->
       if n<10000
       then Retry (O "Objections",handler_details (n+1))
       else Resume "Proxy done\n"
    }
  in
  let r =
    Proxy.p "Bob"
      {hRequest = 
          function (C _,P _,W _,Q query) ->
            match query with
              | "Simple" -> Forward ("Proxy done\n")
              | "Complex" -> Audit (handler_details 0)
      }       
  in
  print_string r

let _ =  start_proxy ()
