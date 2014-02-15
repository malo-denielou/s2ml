open Conf
open Conf_protocol
open Printf
open Prins


let _ = 
  let alice = {id="alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
    register alice; register bob; register charlie


let start_confman () =
  let rec handler_decision = { 
    hClose = (function _ -> Done((),"No more revisions")); 
    hReqRevision = (function _ -> Revise((),handler_submission))
  }
  and handler_submission = { 
    hSubmit = (function (_,p) -> Paper("Paper",handler_decision)); 
    hWithdraw = (function _ -> Retract((),"Retracted"))
  } in
  let rec handler_paper counter _ = 
    if counter > 0 
    then BadFormat ("Try again",{hUpload = handler_paper (counter-1)})
    else if String.length "toto" > 12 
    then BadFormat ("Too long",{hUpload = handler_paper 0})
    else Ok((),handler_submission) in
  let result = confman "charlie" {hUpload = handler_paper 250} in
  printf "ConfMan: session complete: %s\n\n" result

let _ = for i = 1 to 1 do start_confman () done
