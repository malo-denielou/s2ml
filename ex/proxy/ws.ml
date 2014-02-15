open Proxy
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
  register alice; register bob; register charlie; listen "Charlie"; connect "Charlie" "Alice"



let start_ws () =
  let rec sender_details ()=
    Details
      (D "More",
        {hRetry = (function O o -> sender_details ());
         hResume = (function Q query -> Reply(X "There\n","Ws completed\n"))
        }
      )
  in
  let res =
    Proxy.w "Charlie"
      {hForward = (function (C _,P _,W _,Q query) ->
                     Reply (X "Here\n","Ws completed\n")) ;
       hAudit = (function (C _,P _,W _) ->
                   sender_details ())
      }
  in
  print_string res
  

let () = start_ws ()
