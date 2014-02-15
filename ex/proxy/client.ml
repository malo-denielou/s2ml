open Proxy
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
  register alice; register bob; register charlie; connect "Alice" "Bob"; listen "Alice"


let start_client () =
  let r = 
    Proxy.c "Alice"
      (Request (C "Alice",
                P "Bob",
                W "Charlie",
                Q "Complex",
                {hReply = function X x -> x}))
  in
  print_string r

let () = start_client ()
