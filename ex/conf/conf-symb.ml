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
	 if counter > 0 
         then ReqRevision("Work more!",handler_paper (counter-1))
         else if counter > -2 
	 then ReqRevision("Nearly done!",handler_paper (counter-1)) 
	 else Close ((),
                     {hDone = 
                         (function _ -> 
                            Shepherd("Shepherded",handler_discuss 1))})
      );
    hRetract = (function _ -> "Retracted")} in
  let result = pc {prins_pc="alice";prins_author="bob";prins_confman="charlie"}
    (Cfp("Send your papers",handler_paper 2)) in 
  printf "PC: session complete: %s\n\n" result

let start_author () =
  (* The reviewing process = 4 different incoming messages *)
  let rec handler_response = 
    { hAccept = (function (_,comments) -> 
                   FinalVersion("Final", "Accepted! " ^ comments));
      hReject = (function _ -> "Rejected !");
      hShepherd = (function _ -> 
                     (* Very simple answering strategy *)
                     Rebuttal("No!",handler_response));
      hRevise = (function _ -> 
                   Submit("Here.",handler_response)) } in
  (* Exchange with the confman about formatting *)
  let rec handler_format = 
    { hBadFormat = (fun (_,error) -> 
                      printf "Formatting error: %s\n" error;
                      Upload("Submission", handler_format)); 
      hOk = (function _ -> 
               if true (* We could flip a coin *)
               then Submit ("Paper",handler_response)
               else Withdraw ((),"Paper withdrawn")) } in
  (* First message is always a Cfp *)
  let handler_cfp =
    { hCfp = function _ -> 
        Upload("First draft", handler_format)} in
  let result = author "bob" handler_cfp in
  printf "Author session complete: %s\n" result

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
  let result = confman "charlie" {hUpload = handler_paper 1} in
  printf "ConfMan: session complete: %s\n\n" result


let _ = Pi.fork start_confman 
let _ = Pi.fork_dep start_author start_pc
