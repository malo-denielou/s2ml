open Global
open Split
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
  register alice; register bob; register charlie

let start_receiver () = 
  let res = 
    Split.r "Charlie" 
      {hFwd = (function _ -> ""); hRebind = function _ -> ""}
  in print_string "Receiver done\n"

let _ =  start_receiver ()
