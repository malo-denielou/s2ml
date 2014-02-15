open Conf
open Conf_protocol
open Printf
open Prins


let _ = 
  let alice = {id="alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
    register alice; register bob; register charlie


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
   (*                   printf "Formatting error: %s\n" error;*)
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

let _ = for i = 1 to 1 do start_author () done
