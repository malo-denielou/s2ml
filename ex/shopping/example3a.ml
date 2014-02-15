open Shopping
open Shopping_protocol
open Printf
open Prins

let _ = 
  let alice = {id="alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="charlie"; cert="charlie"; ip="127.0.0.1"; port=8003} in
    register alice; register bob; register charlie

type choiceOnOffer = UChange of string | UAccept

let start_customer () =
  let offer_ui offer =
    if offer = "Redmond, 8am-9am"
    then UChange "Cambridge" 
    else UAccept in
  let prins = { 
    prins_c = "alice"; 
    prins_o = "charlie";
    prins_w = "bob"; } in
  let r = "12 March 2007" in
  let rec msg1 = { 
      hReject = (fun (_,s) -> printf "%s\n" s;Abort((),()));
      hOffer = (fun (_,offer) -> 
        match offer_ui offer with
          | UChange location -> Change(location,msg1)
          | UAccept -> Accept((),()))} in
  let msg0 = Request(r,msg1) in
  let worker () = 
    Shopping.c prins msg0;
    printf "Customer: session complete.\n\n" in
  worker()



let start_store () = 
  let offer loc = List.assoc loc 
    [ "Paris", "Paris, 8am-9am";  
      "Redmond", "Redmond, 3pm-4pm";
      "Orsay", "Orsay, lunchtime";
      "Cambridge", "Cambridge, 6pm-7pm" ]  in
  let server (prins,req) = 
    printf "Server: session starting for %s.\n" req;
    let rec new_offer (prins,loc) = 
      try
        let o = offer loc in
        Offer(o, {
            hChange = new_offer;
            hAccept = (fun _ -> Confirm((),"in "^o)); })
      with _ -> Reject("No offer available","No offer available") in
    new_offer (prins,"Paris") in
  let status = Shopping.w "bob" { hContract = server; } in
  printf "Store: Done! %s.\n" status


let start_officer () = 
  let say s = Printf.printf "\nOffice: %s.\n\n" s in
  let log s _  = say s in
  let request (id,date) =
    say ("running with "^id.prins_c^" and "^id.prins_w);
    Contract(date,
            { hConfirm = log "run confirmed";
              hAbort = log "run aborted"; }) in
  Shopping.o "charlie" {hRequest = request}

let _ = Pi.fork start_store
let _ = Pi.fork_dep start_officer start_customer
