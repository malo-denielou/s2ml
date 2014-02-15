open Conf
open Conf_protocol
open Printf
open Prins


let _ = 
  let alice = {id="alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
    register alice; register bob; register charlie


let start_pc () =
  let rec handler_discuss counter = {
    hRebuttal = 
      (function _-> 
         if counter > 0 then Shepherd("Shepherded",handler_discuss (counter-1))
         else Accept ("Accept",
                      {hFinalVersion = (function _ -> "We accepted a paper!")})
      )
  } in
  let rec handler_paper counter = { 
    hPaper = 
      (function (_,s) -> 
	 if counter > 1 
         then ReqRevision("Work more!",handler_paper (counter-1))
         else if counter > 0 
	 then ReqRevision("Nearly done!",handler_paper (counter-1)) 
	 else Close ((),
                     {hDone = 
                         (function _ -> 
                            Shepherd("Shepherded",handler_discuss 250))})
      );
    hRetract = (function _ -> "Retracted")} in
  let result = pc {prins_pc="alice";prins_author="bob";prins_confman="charlie"}
    (Cfp("Send your papers",handler_paper 250)) in 
  printf "PC: session complete: %s\n\n" result

let _ = for i = 1 to 1 do start_pc () done
