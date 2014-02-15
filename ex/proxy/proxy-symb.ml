open Proxy
open Printf
open Prins
open Proxy_protocol

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
  register alice; register bob; register charlie


let start_client () =
  let r = 
    Proxy.c {prins_c = "Alice";prins_p = "Bob" ; prins_w = "Charlie"}
      (Request ("Complex",
                {hReply = function (_,x) -> x}))
  in
  print_string r

let start_proxy () =
  let rec handler_details n =
    {hDetails = function (_,d) ->
       if n<2 
       then Retry ("Objections",handler_details (n+1))
       else Resume ("","Proxy done\n")
    }
  in
  let r =
    Proxy.p "Bob" 
      {hRequest = 
          function (_,query) ->
            match query with
              | "Simple" -> Forward ("","Proxy done\n")
              | "Complex" -> Audit ("",handler_details 0)
      }
  in
  print_string r

let start_ws () =
  let rec sender_details ()=
    Details
      ("More",
        {hRetry = (function _,o -> sender_details ());
         hResume = (function _,query -> Reply("There","Ws completed\n"))
        }
      )
  in
  let res =
    Proxy.w "Charlie"
      {hForward = (function (_,query) ->
                     Reply ("Here!","Ws completed\n")) ;
       hAudit = (function _ ->
                   sender_details ())
      }
  in
  print_string res
  


let _ = Pi.fork start_proxy 
let _ = Pi.fork start_ws
let () = start_client ()
