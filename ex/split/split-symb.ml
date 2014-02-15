open Global
open Split
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
  register alice; register bob; register charlie

let start_client () =
  let res = 
    Split.c "Alice" 
    (Write(Q 42,C "Alice",S "Bob",R "Charlie","")) 
  in
    print_string "Client done\n"

let start_server () = 
  let res = 
    Split.s "Bob" 
      {hWrite = function _ -> Fwd("")}
  in print_string "Server done\n"

let start_receiver () = 
  let res = 
    Split.r "Charlie" 
      {hFwd = (function _ -> ""); hRebind = function _ -> ""}
  in print_string "Receiver done\n"


let _ = Pi.fork start_server
let _ = Pi.fork_dep start_receiver start_client
