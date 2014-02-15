open Single
open Single_protocol
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  register alice; register bob

let start_client () =
  Single.c 
    {prins_c = "Alice"; prins_w = "Bob"} 
    (Request ("Action!", ()))

let start_server () = 
  let res = 
    Single.w "Bob" {hRequest = function (_,content) -> content}
  in print_string res

let _ = Pi.fork_dep start_server start_client
