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

let _ = start_client ()
