
(* family of functions indexed by label, in charge of building the
   messages to be sent *)

(* the compiler computes: 
   (future_visible_roles: (int tuple) potential future roles where this node is visible
   (variables_to_lift: (variable tuple) variables that need lifting 
*)

let send_from_label st (ps:payload tuple) = 


(* fetchs pending macs MAC_AB(...) where A is the current principal
   and B is in future_visible_roles (see below) *)

let get_macs_to_be_forwarded_label 
    st (* current_state *)
    =
  let mac_0 = get_mac_label 0 st in
  let mac_1 = concat mac_0 (get_mac_label 1 st) in
    ...
  let mac_n = concat mac_nminus1 (get_mac_label n st) in
    mac_n in


(* family of functions indexed by label that computes the new
   history *)
let make_new_history_label st ps = 
  let new_hash history (variable,value) = 
    sha1 history (concat variable value) in
    List.fold_left new_hash st.history ps in

  (* computes the new variables that need lifting *)
  let get_store_vars_to_lift_label st = 
    ((var_0,get_payload_label_0 st), ... (var_n,get_payload_label_n st)) in


(* *)
let make_macs_label st label =
  let add_single_mac previous_macs label = 
    let prin = sourceprin_label st in
    let destprin = destprin_label st in 
    let shared_key = Prins.fetch_key prin destprin in 
      concat previous_macs (mac shared_key label st.history) in (* TODO: add "counter", i.e. unit var in loops to prevent replays *)
    List.fold_left add_single_mac (utf8 "") [destlabel0,...,destlabeln]

  let vars_and_payloads_to_lift = get_store_vars_to_lift_label st in
  let st = new_state st new_history in
  let new_history = make_new_history_label st vars_and_payloads_to_lift in

  let fwdmacs = get_macs_to_be_forwarded_from_label st in 


  let (localmacs, st) = 
    make_macs_label st future_visible_roles in

  let macs = concat localmacs fwdmacs in
  (st, macs)


