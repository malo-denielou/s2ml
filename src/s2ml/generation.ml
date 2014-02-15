(********************************************************************)
(* Session - Implementation                                         *)
(*                                                                  *)
(* generation.ml                                                    *)
(********************************************************************)
(* $Id: generation.ml 9310 2009-01-27 15:07:25Z denielou $ *)

(*#use "c:/Documents and Settings/t-ricarc/inria-msr-sec/lang-sec/session-types/src/syntax.ml";;*)
(*#use "/Users/ricardocorin/inria-msr-sec/lang-sec/session-types/src/syntax.ml";;*)

open Common
open Syntax 
open Graph

let debug = gen_debug debug_generation "gen"


(********************************************)
(*             Initial Part                 *)
(********************************************)

(********************************************)
(*         Global Helper Functions          *)
(********************************************)

let rec print_stategraph (g:stategraph) = 
  match g with
      [] -> ""
    | ((a,b),m,(a1,b1))::rest -> 
        (Printf.sprintf "(%s,%s),%s,(%s,%s))" 
           (print_state a)
           (print_state b)
            m.label
           (print_state a1)
           (print_state b1))
         ^(print_stategraph rest)

let global_def (g:stategraph) =
  "open Global\n"
  ^"open Base64\n"
  ^"open Data\n"
  ^"open Pi\n"
  ^"open Crypto\n"
  ^"open Prins\n\n"
  ^"let debug_impl = debug \"impl\"\n\n" 
  ^"type principal = string\n"
  ^"\ntype mackey = bytes hkey\n"
  ^"let get_mackey (a:principal) (b:principal) = Prins.get_mackey (cS a) (cS b)\n"




(********************************************)
(*              Wired Part                  *)
(********************************************)

let seq_to_s (seq:seq) = 
  let s = String.concat "_" (List.map (function (_,m) -> m.label) seq) in
  if s = "" then "start"
  else s

(* Store management *)
let recreate_store hd lm lk lr =
  let rec vars = function
      [] -> ""
    | v::q -> v^" = "^v^"; "^(vars q)
  in
  let rec macs = function
      [] -> ""
    | (role,m,r)::q -> 
        "mac_"^r^m.label^" = mac_"^r^m.label^"; "^(macs q)
  in
  let rec keys = function
      [] -> ""
    | (x,y)::q -> "key_"^x^y^" = key_"^x^y^"; "^(keys q)
  in
  let rec prins = function
      [] -> ""
    | x::q -> "prins_"^x^" = prins_"^x^"; "^(prins q)
  in
  let p = if lr = [] then "store.prins" else ("{store.prins with "^(prins lr)^"}") in
  let m = if lm = [] then "store.macs" else ("{store.macs with "^(macs lm)^"}") in
  let k = if lk = [] then "store.keys" else ("{store.keys with "^(keys lk)^"}" )in
  let t = if hd = [] then "store.header" else 
      if hd = ["sid";"ts"] ||  hd = ["ts";"sid"] then ("{"^(vars hd)^"}")
    else ("{store.header with "^(vars hd)^"}")
  in
  if (lm = []) && (lk = []) && (hd = []) && (lr = [])
  then ""
  else (
  "  let store = { \n      prins = "^(p)^"; \n      macs = "^(m)^"; \n      keys = "^(k)^"; \n      header = "^(t)^"} in\n"
    )
   

(* Marshalling and Unmarshalling *)   
let unmar_a_var vl v =
  match List.assoc v vl with
    | "string" -> 
        "  let string_"^v^" = iutf8 mar_"^v^" in\n"^
          "  let "^v^" = iS string_"^v^" in\n"
    | "int" -> 
        "  let string_"^v^" = iutf8 mar_"^v^" in\n"^
          "  let marred_"^v^" = iS string_"^v^" in\n"^
          "  let "^v^" = int_of_string marred_"^v^" in\n"
    | "principal" -> 
        "  let string_"^v^" = iutf8 mar_"^v^" in\n"^
          "  let "^v^" = iS string_"^v^" in\n"
    | _ -> assert false

let unmar_var = function
  | "string" -> "  let x = iS (iutf8 value)  in\n"
  | "int" ->  "  let x = int_of_string (iS (iutf8 value))  in\n"
  | "principal" -> "  let x = iS (iutf8 value)  in\n"
  | "unit" -> "  let x = () in\n"
  | _ -> assert false

let rec decr_vars send recv lr hl vl = function
    [] -> ""
  | v::q -> 
      if List.mem v lr then (decr_vars send recv lr hl vl q)
      else if List.mem v hl 
      then 
        ("  let mar_"^v^" = unpickle (sym_decrypt (get_symkey (cS store.vars."^send^") (cS store.vars."^recv^")) encr_"^v^") in\n"^
           (unmar_a_var vl v)^
           (decr_vars send recv lr hl vl q))
      else 
        ("  let key"^send^recv^" = get_symkey (cS store.vars."^send^") (cS store.vars."^recv^") in\n"^
           "  let mar_"^v^" = unpickle (sym_decrypt key"^send^recv^" encr_"^v^") in\n"^
           (unmar_a_var vl v)^
           "  let h"^v^" = sha1 mar_"^v^" in\n"
        )^(decr_vars send recv lr hl vl q)
        

let rec unmar_vars send recv lr hl vl = function
    [] -> ""
  | v::q -> 
      if List.mem v lr then
        ("  let mar_"^v^",variables = iconcat variables in\n"^ 
           (unmar_a_var vl v)^
           "  let h"^v^" = sha1 mar_"^v^" in\n")
        ^(unmar_vars send recv lr hl vl q)
      else
        ("  let encr_"^v^",variables = iconcat variables in\n"^ 
           (unmar_vars send recv lr hl vl q))


let mar_a_var vl prefix v =
  match List.assoc v vl with
  | "string" -> 
        "  let string_"^v^" = cS "^prefix^v^" in\n"^
          "  let mar_"^v^" = utf8 string_"^v^" in\n"
  | "int" -> 
        "  let marred_"^v^" = string_of_int "^prefix^v^" in\n"^
        "  let string_"^v^" = cS marred_"^v^" in\n"^
          "  let mar_"^v^" = utf8 string_"^v^" in\n"
  | "principal" -> 
        "  let string_"^v^" = cS "^prefix^v^" in\n"^
          "  let mar_"^v^" = utf8 string_"^v^" in\n"
  | _ -> assert false

let rec mar_vars send recv lr vl = function
    [] -> "  let variables = nil in\n"
  | v::q -> (mar_vars send recv lr vl q)^(
      (if List.mem v lr then
         ((mar_a_var vl "store.vars." v)^
	    "  let variables = concat mar_"^v^" variables in\n")
	else
          ((mar_a_var vl "store.vars." v)^
             "  let key"^send^recv^" = get_symkey (cS store.vars."^send^") (cS store.vars."^recv^") in\n"^
             "  let encr_"^v^" = sym_encrypt key"^send^recv^" (pickle mar_"^v^") in\n"^
	     "  let variables = concat encr_"^v^" variables in\n")
      )
    )

let mar_var = function
  | "string" -> "  let value = utf8 (cS x)  in\n"
  | "int" -> "  let value = utf8 (cS (string_of_int x))  in\n"
  | "principal" ->  "  let value = utf8 (cS x)  in\n"
  | "unit" -> "  let value = utf8 (cS \"\")  in\n"
  | _ -> assert false


let rec mar_macs = function
  | [] -> "  let macs = nil in\n"
  | (role,m,r)::q -> 
      (mar_macs q)^
      ("  let macs = concat store.macs.mac_"^r^m.label^" macs in\n")

let rec unmar_macs = function 
    [] -> ""
  | [role,m,r] -> 
      "  let mac_"^r^m.label^",_ = iconcat macs in\n"
  | (role,m,r)::q -> 
      ("  let mac_"^r^m.label^",macs = iconcat macs in\n"^
        (unmar_macs q))


(* Creation of macs *)
let rec gen_macs st nextst role m = function 
  | [] -> ""
  | r::q -> 
      "  (* Generation of a MAC from state "^(print_state st)^" to role "^r^" *)\n"^
	"  let content = content_"^(print_state nextst)^" store.header.ts store in\n"^
        "  let mackey"^role^r^" = get_mackey store.prins.prins_"^role^" store.prins.prins_"^r^" in\n"^
        "  let macmsg = mac mackey"^role^r^" (pickle content) in\n"^
        "  let mac_"^r^m.label^" = concat header macmsg in\n"^
        (gen_macs st nextst role m q)


(* Verifications *)

let rec mac_verify first_recv role : state list -> string = function 
    [] -> "  let _ = test_eq oldts store.header.ts \"Time-stamp verification\" in\n"
  | s::q -> 
      let (r,m) = List.hd (snd s) in
      "  (* Verification of a MAC from state "^(print_state s)^"*)\n"^
        "  let macheader,maccontent = iconcat store.macs.mac_"^role^m.label^" in\n"^
        "  let macsid,ts_mar = iconcat macheader in\n"^
        "  let ts_str = iutf8 ts_mar in\n"^
        "  let ts_string = iS ts_str in\n"^
        "  let ts = int_of_string ts_string in\n"^
        "  let _ = test_inf oldts ts \"MAC verification\" in\n"^
        "  let oldts = ts in\n"^
        "  let _ = test_eq macsid "^(if first_recv 
                                      then "sid" 
                                      else "store.header.sid")^" \"MAC verification\" in\n"^
        "  let mackey"^r^role^" = get_mackey store.prins.prins_"^r^" store.prins.prins_"^role^" in\n"^
        "  let _ = mac_verify_"^role^"_"^(print_state s)^" ts store mackey"^r^role^" maccontent in\n"^
        (mac_verify first_recv role q)


let mac_fun (visib:visib) = 
  let mac_aux (st,l) =
    let rec unfold l =
      match l with
          [] -> []
        | s::q ->
            ("let mac_verify_"^(fst st)^"_"^(print_state s)^" = fun ts store k m ->\n"^
               "  let content = content_"^(print_state s)^" ts store in\n"^
               "  let cc = mac_verify k m content in\n"^
               "  let pc = pickle content in\n"^
               "  let _ = test_eq cc pc \"MAC incorrect\" in\n"^
               "  let up = unpickle cc in\n"^
               "  up\n")::
              (unfold q)
    in
    unfold (List.flatten l)
  in
  let mac_content (st,l) =
    let rec unfold l =
      match l with
          [] -> []
        | s::q -> 
            ("let content_"^(print_state s)^" = fun ts store ->\n"^
               "  let nextstate = cS \""^(print_state s)^"\" in\n"^
               "  let nextstate_string = utf8 nextstate in\n"^
               "  let ts_string = string_of_int ts in\n"^
               "  let ts_str = cS ts_string in\n"^
               "  let ts_mar = utf8 ts_str in\n"^
               "  let header = concat store.header.sid ts_mar in\n"^
	       "  let content = concat header nextstate_string in\n"^
               "  content\n")::
              (unfold q)
    in
    unfold (List.flatten l)
  in
  (remove_duplicates (List.flatten (List.map mac_content visib)))@
  (remove_duplicates (List.flatten (List.map mac_aux visib)))

(* Keys *)

let rec mar_keys sender = function 
  | [] -> "  let keys = nil in\n"
  | (s,r)::q -> (mar_keys sender q)^
      if s = sender || r = sender then
        ("  let key"^s^r^" = gen_keys (cS store.prins.prins_"^s^") (cS store.prins.prins_"^r^") in\n"^
           "  let keys = concat key"^s^r^" keys in\n")
      else
        ("  let keys = concat store.keys.key_"^s^r^" keys in\n"
        )

let rec unmar_keys role lk = 
  let rec aux = function
      [] -> assert false
    | [s,r] -> 
(*          "  let key_"^r^s^",keys = iconcat keys in\n"^*)
          "  let key_"^s^r^",_ = iconcat keys in\n"^
	  if role = s || role = r
          then ((*"  let () = reg_keys (cS store.prins.prins_"^r^") (cS store.prins.prins_"^s^") key_"^r^s^" in\n"^*)
	          "  let () = reg_keys (cS store.prins.prins_"^s^") (cS store.prins.prins_"^r^") key_"^s^r^" in\n")
          else ""
    | (s,r)::q -> 
        ( (*"  let key_"^r^s^",keys = iconcat keys in\n"^*)
            "  let key_"^s^r^",keys = iconcat keys in\n"^
	    (if role = s || role = r
             then (
	       (*"  let () = reg_keys (cS store.prins.prins_"^r^") (cS store.prins.prins_"^s^") key_"^r^s^" in\n"^*)
	         "  let () = reg_keys (cS store.prins.prins_"^s^") (cS store.prins.prins_"^r^") key_"^s^r^" in\n")
             else ""))^
          (aux q)
  in
  if lk = [] then "" else (
    "  (* Unmarshalling keys *)\n"^(aux lk)
  )
       
(*let rec unmar_keys = function
    [] -> ""
  | [s,r] -> 
        "  let key_"^s^r^",_ = iconcat keys in\n"^
	"  let () = reg_keys (cS store.prins.prins_"^s^") (cS store.prins.prins_"^r^") key_"^s^r^" in\n"
  | (s,r)::q -> 
      ("  let key_"^s^r^",keys = iconcat keys in\n"^
	 "  let () = reg_keys (cS store.prins.prins_"^s^") (cS store.prins.prins_"^r^") key_"^s^r^" in\n")^
        (unmar_keys q)
*)
(* Principals *)

let rec mar_prins = function
  | [] -> "  let prins = nil in\n"
  | v::q -> (mar_prins q)^(
      "  let string_"^v^" = cS store.prins.prins_"^v^" in\n"^
        "  let mar_"^v^" = utf8 string_"^v^" in\n"^
        "  let prins = concat mar_"^v^" prins in\n")

let rec unmar_prins = function
  | [] -> ""
  | [v] -> 
      "  let mar_"^v^",_ = iconcat prins in\n"
      ^"  let string_"^v^" = iutf8 mar_"^v^" in\n"
      ^"  let prins_"^v^" = iS string_"^v^" in\n"
  | v::q -> (
      "  let mar_"^v^",prins = iconcat prins in\n"
      ^"  let string_"^v^" = iutf8 mar_"^v^" in\n"
      ^"  let prins_"^v^" = iS string_"^v^" in\n")^
      (unmar_prins q)

(* Code Generation *)

let check_sid_ts flag role= 
  if flag then (
    "  let sid,ts_mar = iconcat header in\n"^
      "  let ts_str = iutf8 ts_mar in\n"^
      "  let ts_string = iS ts_str in\n"^
      "  let ts = int_of_string ts_string in\n"^
      "  let () = check_cache store.prins.prins_"^role^" \""^role^"\" sid in\n"^
      "  let oldts = store.header.ts in\n"^
      (recreate_store ["sid";"ts"] [] [] []) )
  else (
    "  let sid,ts_mar = iconcat header in\n"^
      "  let ts_str = iutf8 ts_mar in\n"^
      "  let ts_string = iS ts_str in\n"^
      "  let ts = int_of_string ts_string in\n"^
      "  let oldts = store.header.ts in\n"^
      "  let _ = test_inf oldts ts \"Replay attack!\" in\n"^
      "  let _ = test_eq store.header.sid sid \"Session confusion attack!\" in\n"^
      (recreate_store ["ts"] [] [] []))
    

let rec generate_wired_recv lr fut visib fwd nextrecvg : state list -> string list = function 
    [] -> []
  | (state)::q -> 
      let fwdm,fwdk = fwd in
      let recv = fst state in
      let lv = subset_assoc state visib in
      (*debug (Printf.sprintf "Generating wired recving of state %s "(print_seq state));*)
      let gen_recv vis =        String.concat "\n" 
        (List.map 
           (function v ->
              let (send,seq) = List.hd v in
              let (_,m) = List.hd seq in
              let edge = List.assoc (state,m,(send,seq)) nextrecvg in
              let (sendst,sendnext),_,(_,recvnext) = edge in
              debug ("Looking at message "^m.label^" at position "^
                       (print_state_display state)^" going to "^(print_state_display recvnext));
              let recvmacs = List.sort 
                (function (_,m,_) -> (function (_,m',_) -> compare m m')) (safe_assoc edge fwdm) in
              let recvkeys = safe_assoc edge fwdk in
       
              let recvops =
                "  | \""^(print_vi v)^"\" ->\n"^
                  "  let value,protocol = iconcat payload in\n"^
                  "  let macs,keys = iconcat protocol in\n"^
                  "  (* Unmarshalling value *)\n"^
                  (unmar_var m.payload)^
                  (*"  let x = iS (iutf8 value) in\n"^*)
(*                  (unmar_vars send recv lr tocheck vl recvvars)^*)
                  (unmar_keys recv recvkeys)^
(*                  "  (* Decrypting variables *)\n"^*)
                  "  (* Unmarshalling MACs *)\n"^
                  (unmar_macs recvmacs)^
                  (recreate_store [] recvmacs recvkeys [])^
                  (mac_verify (snd state=[]) recv (List.rev v))^ 
                  "  (* Verification Ended *)\n"^
                  (Printf.sprintf  "    let wired = Wired_in_%s_of_%s(x,store) in\n    wired"
                     (print_state state)
                     (print_vi v))   
              in
              recvops )
           vis)
      in
      let recvprin = "  let msg = precv  store.prins.prins_"^recv^" in\n" in
      let sep = 
        if snd state = [] 
        then ("  let start,content = iconcat (ibase64 msg) in\n"^
                "  let header,prins = iconcat start in\n"^
                (unmar_prins lr)^
                (recreate_store [] [] [] lr)
             )
        else 
	"  let header,content = iconcat (ibase64 msg) in\n" in
      let check =
      check_sid_ts (snd state=[]) recv (* pass flag telling whether its first message or nor *)
      in
      let tags = "  let tag,payload = iconcat content in\n" in
      (Printf.sprintf "\nlet receiveWired_%s : store -> wired_%s = fun store ->\n%s%s%s%s  match iS (iutf8 tag) with"
          (print_state state)
          (print_state state)
          recvprin
          sep
	  check
          tags
      )
      ::(String.concat "\n" (List.map gen_recv lv))
      ::("  | _ -> failwith \"Critical Error\"\n")
      :: (generate_wired_recv lr fut visib fwd nextrecvg q)


let rec generate_wired_send lr antivisib (fut:future) fwd = function
    [] -> []
  | (((sendst,sendnext),m,(recvst,recvnext)) as e)::q -> 
      (*debug (Printf.sprintf "Generating wired sending of message %s " m.label);*)
      let send = fst sendst in
      let recv,recvseq = recvst in
      let fwdm,fwdk = fwd in
      let sentmacs = List.sort 
        (function (_,m,_) -> (function (_,m',_) -> compare m m')) (safe_assoc e fwdm) in
      let sentkeys = safe_assoc e fwdk in
      let macto = List.assoc (m,sendst) fut in
      let lm = List.map (function r -> (send,m,r)) macto in
      let v = List.hd (subset_assoc e antivisib) in 
      (* let v = (hist_to_visib recv
         (snd recvnext)) in *)
      let makehdr = 
        if snd recvst = [] 
        then ((mar_prins lr)^
                "  let header = concat store.header.sid ts_mar in\n"^
                "  let start = concat header prins in\n")
        else 
          "  let header = concat store.header.sid ts_mar in\n"^
            "  let start = header in\n" 
      in
      let fstcomment = Printf.sprintf "(* Sending message from %s to %s *)\n"
        (send) (recv) in
      let sndcomment = Printf.sprintf "(* Message has to be MACed for %s *)\n"
        (String.concat " " macto) in
      let sendops = 
          "  let ts = store.header.ts + 1 in\n"^
            (recreate_store ["ts"] [] [] [])^(* Update store *)
          "  let ts_string = string_of_int store.header.ts in\n"^
          "  let ts_str = cS ts_string in\n"^
          "  let ts_mar = utf8 ts_str in\n"^
          (makehdr)^
          (mar_keys send sentkeys)^
          (gen_macs sendst sendnext send m macto)^
          (recreate_store [] lm [] [])^
          (mar_macs sentmacs)^
          (mar_var m.payload)^
            (*"  let value = utf8 (cS x) in\n"^*)
          "  let protocol = concat macs keys in\n"^
          "  let payload = concat value protocol in\n"^
          "  let visib = cS \""^(print_vi v)^"\" in\n"^
          "  let visib_string = utf8 visib in\n"^
          "  let content = concat visib_string payload in\n"^
          "  let msg = base64 (concat start content) in\n"^
          "  let () = psend  store.prins.prins_"^recv^" msg in "^
          "" in
      let binding1 = "  let empty_str = cS \"\" in\n" in
      let binding2 = "  let nil = utf8 empty_str in\n" in
      (Printf.sprintf "%s%slet sendWired_%s_%s = %sfun store ->\n%s%s%s\n  store \n"
         fstcomment
         sndcomment
         m.label
         (print_state sendst)
         ("fun x -> ")
         binding1
         binding2
         sendops
      )
      :: (generate_wired_send lr antivisib fut fwd q)


let generate_wired lr (visib:visib) antivisib (fut:future) (fwd:fwd) (stategraph:stategraph) =
  let states = remove_duplicates (List.map fst visib) in
  let nextrecvg = List.map (function ((d,e),b,(a,c)) -> ((a,b,e),((d,e),b,(a,c)))) stategraph in
  "\n(*******************************************)
(* Wired types, send and receive functions *)
(*******************************************)\n\n"^
    (String.concat "\n" (mac_fun visib))^
    "\n\n"^
    (String.concat "\n" (Geninterface.generate_wired_type visib states))^
    "\n\n"^
    String.concat "\n" (remove_duplicates (generate_wired_recv lr fut visib fwd nextrecvg states))^ 
    "\n\n" ^
    String.concat "\n" (remove_duplicates (generate_wired_send lr antivisib fut fwd stategraph))^ 
    "\n\n"
  


(********************************************)
(*           Proxy generation               *)
(********************************************)
(* String generation *)

(*let gen_handler_name s = Printf.sprintf "handlers.h%s newSt.prins " s*)

let gen_handler_name s = Printf.sprintf "handlers.h%s" s
let crypto_params3 = "newSt"
let crypto_params3_recv = "newSt"

(* Sending operations *)
let crypto_params = " (st:store)"
let crypto_params_recv = " (st:store)"
let crypto_wired = "st"
let crypto_wired_init_recv = "prin "


(*------------------*)
(* Proxy generation *)
(*------------------*)



let rec gen_proxy_send sn sg_recv = function
    [] -> []
  | (s,lm)::q ->
      let role = fst s in
      let rec gen_pattern s = function
          [] -> ""
        | (m,ss)::q -> 
            (*debug (Printf.sprintf "Message %s" m.label);*)
            let conting = List.mem_assoc ss sg_recv in
            let state = if conting then "newSt" else "_" in
            let cont = 
              if conting 
              then Printf.sprintf "%s newSt next" (print_state ss)  
              else "next" 
            in 
            let myself = "st.prins.prins_"^role in
            (Printf.sprintf "  | %s(payload,next) ->\n%s    let %s = sendWired_%s_%s payload st in\n    %s\n"
               (m.label) 
               (Printf.sprintf 
                  "    let ts1 = st.header.ts+1 in\n    let () = assume (Send_%s(%s,st.header.sid,ts1)) in\n"
                  m.label myself)
               state
               (m.label)
               (print_state s)
               
               cont)
            ^(gen_pattern s q)
      in
      let n = 
        if List.mem_assoc s sn 
        then List.assoc s sn  
        else (debug (Printf.sprintf "Error at point %s" (print_state s));assert false) 
      in 
      ((Printf.sprintf "%s : store -> %s -> %s = fun st -> function\n" 
            (print_state s) 
            (Geninterface.gen_typename n) 
            (Geninterface.gen_result_type role))^         
         (gen_pattern s lm))
      ::(gen_proxy_send sn sg_recv q)
        

let rec gen_proxy_recv2 (visib:visib) sn sg_send (sg_sendnext:(state*edge) list) = function
    [] -> []
  | state::q ->
      let role = fst state in
      let lv = List.assoc state visib in
      let rec gen_pattern = function
          [] -> ""
        | v::q -> 
            (*debug (Printf.sprintf "Message %s" m.label);*)
            let senderst = List.hd v in
            let edge = List.assoc senderst sg_sendnext in
            let _,m,(_,rr) = edge in
            let conting = List.mem_assoc rr sg_send in
            let cont = 
              if conting 
              then Printf.sprintf "%s %s next" (print_state rr) crypto_params3 
              else "next" 
            in 
            (Printf.sprintf "  | Wired_in_%s_of_%s (payload, newSt) ->\n%s    let next = %s (st.prins,payload) in\n    %s\n"
               (print_state state (*m.label*) )
               (print_vi v) 
               (Printf.sprintf 
                     "    let () = assume (Recv_%s(newSt.prins.prins_%s,newSt.header.sid,newSt.header.ts)) in\n"
                     m.label role)
               (gen_handler_name m.label)
               cont)
            ^(gen_pattern q)
      in
      let n = 
        if List.mem_assoc state sn 
        then List.assoc state sn
        else (debug (Printf.sprintf "Error at point %s" (print_state state));assert false) 
      in 
      ((Printf.sprintf "%s : store -> %s -> %s = fun st handlers ->\n  let r = receiveWired_%s %s in\n  match r with\n" 
          (print_state state)
          (Geninterface.gen_typename n) 
          (Geninterface.gen_result_type role)
          (print_state state) 
	  (crypto_wired))^
         (gen_pattern lv))
      ::(gen_proxy_recv2 visib sn sg_send sg_sendnext q)

let proxy_functions (role:string) (visib:visib) (state_to_node:state_node) (sg:stategraph): string list =
  let sg_recv = List.map (function (_,m,(p,pp))-> p,(m,pp)) sg in
  let sg_recv = List.filter (function (r,_),_ -> r = role) sg_recv in
  let sg_send = List.map (function ((s,ss),m,_)->s,(m,ss)) sg in
  let sg_send = List.filter (function (r,_),_ -> r = role) sg_send in
  let send_states = remove_duplicates (List.map (function (_,s),_ -> s) sg_send) in
  let send_sorted = 
    List.map (function s -> (role,s),remove_duplicates (subset_assoc (role,s) sg_send)) send_states in
  let recv_states = remove_duplicates (List.map fst sg_recv) in
(*  let recv_sorted = 
    List.map (function s -> (role,s),remove_duplicates (subset_assoc (role,s)
    sg_recv)) recv_states in *)

  let sg_sendnext = List.map (function ((s,ss),m,r)->ss,((s,ss),m,r)) sg in

  (gen_proxy_send state_to_node sg_recv send_sorted)
  @(gen_proxy_recv2 visib state_to_node sg_send sg_sendnext recv_states)
(*
  List.flatten (List.map (gen_proxy loc sg_send sg_recv visib v role) l)
*)


(* Pretty printing functions *)
let rec gen_rec_fun : (string*string) list -> string = function
    [] -> ""
  | [(s1,s2)] -> Printf.sprintf "(* %s *)\nlet rec %s" s1 s2
  | (s1,s2)::q -> (gen_rec_fun q)^"(* "^s1^" *)\nand "^s2


let rec gen_rec_fun2 : string list -> string = function
    [] -> ""
  | [s] -> Printf.sprintf "let rec %s" s
  | (s)::q -> (gen_rec_fun2 q)^"and "^s



(*----------------------*)
(* First and last lines *)

let gen_user_inputs (l:role_type list) : string =
  match l with
      [] -> assert false
    | (Send (n,_ ))::_-> Printf.sprintf "(user_input : %s) " ((Geninterface.gen_typename n))
    | (Receive (n,_))::_-> Printf.sprintf "(user_input : %s) " ((Geninterface.gen_typename n))
    | _ -> assert false


let gen_empty_store (rl:role_list) (fut:future) = 
  let macs = remove_duplicates
    (List.flatten 
       (List.map 
          (function ((m,a),l) -> 
             List.map 
               (function r -> "mac_"^r^m.label) l) fut))  in
  let macs = 
    String.concat "; " (List.map (function s -> s^" = utf8 (cS \"\") ") macs) in 
  let roles = List.map (function (r,_,_) -> r) rl in
  let pairs = List.filter (function (x,y) -> x<>y) (product roles roles) in
  let keys = String.concat "; " 
    (List.map (function (x,y)-> "key_"^x^y^" = utf8 (cS \"\") ") pairs) in
  let header_init = " sid = sha1 (concat (mkNonce()) session); ts = 0 "
  in
  let header = " sid = utf8 (cS \"\") ; ts = 0 "
  in 
  let prins role =
    "{"^(String.concat "; " 
           (List.map 
              (function s -> 
                 if s = role 
                 then 
                   "prins_"^s^" = host "
                 else
                   "prins_"^s^" = \"\" "
              ) roles))^"}"
  in
  String.concat ""
    (List.map (fun (role,_,r) ->
                 if Geninterface.first_send r 
                 then
                   "\nlet empty_store_"^role^" prins = \n"^
                     "  {prins = prins ;\n"^
                     "   macs = { "^macs^"};\n"^
                     "   keys = { "^keys^"};\n"^
                     "   header = {"^(header_init)^"} }\n"
                 else
                   "\nlet empty_store_"^role^" host = \n"^
                     "  {prins = "^(prins role)^";\n"^
                     "   macs = { "^macs^"};\n"^
                     "   keys = { "^keys^"};\n"^
                     "   header = {"^(header)^"} }\n"
              )
       rl)


let crypto_first_line = " (prins: principals) "
let crypto_first_line_recv = " (prin: principal) "

let first_line role rt (state_to_node:state_node) = 
  let i = List.assoc (role,[]) state_to_node in
  if Geninterface.first_send rt
  then
    Printf.sprintf "(prins:prins) (user_input:msg%d)" i
  else
    Printf.sprintf "(host:principal) (user_input:msg%d)" i
  
let crypto_last_line c= 
  "  let empty_store = empty_store_"^c^" prins in\n"^(*gen_empty_store "" vl rl fut rt*)
    "  let () = bind prins.prins_"^c^" in\n"^
    "  debug_impl (\"Executing role "^c^" ...\");\n"^
    "  let result ="

let crypto_last_line_recv c = 
  "  let empty_store = empty_store_"^c^" host in\n"^(*gen_empty_store c vl rl fut rt)^" in\n"^*)
    "  let () = bind host in\n"^
    "  debug_impl (\"Executing role "^c^" ...\");\n"^
    "  let result ="

let last_line rl fut (role:string) (r:role_type) = 
  if Geninterface.first_send r
  then
    (Printf.sprintf "%s %s_start empty_store user_input in (*close();*)\n  result\n"
       (crypto_last_line role)
       role)
  else
    (Printf.sprintf "%s %s_start empty_store user_input in (*close();*)\n  result\n" 
       (crypto_last_line_recv role)
       role)
(* Function generation complete *)

let generate_functions2  (role:string) (visib:visib) (fut:future) (state_to_node:state_node) (stategraph:stategraph)
    (rl:role_list) (rt:role_type) = 
  Printf.sprintf "%s\n\nlet %s %s =\n%s"
    (gen_rec_fun2 (proxy_functions role visib state_to_node stategraph))
    role
    (first_line role rt state_to_node)
    (last_line rl fut role rt)


(********************************************)
(*           Complete generation            *)
(********************************************)


let generate_from_role (rl:role_list) (visib:visib) (fut:future)
    (state_to_node:state_node) (stategraph:stategraph) (role_name:string)
    (typ:string) (role:role_type) : string = 
  let flatten_session = flatten role in
  (Printf.sprintf "\n(* Proxy function for %s *)\n" role_name)^
    (Printf.sprintf "\ntype %s = %s\n" (Geninterface.gen_result_type role_name) typ)^
    (Geninterface.gen_types "" role_name (List.rev flatten_session))^"\n"^
    (generate_functions2 role_name visib fut state_to_node stategraph rl role)
   
let rec generate_from_session (visib:visib) (fut:future) (state_to_node:state_node) (stategraph:stategraph) (s:role_list)
    (rl:role_list) : string = 
  match s with
    [] -> ""
  | (str,typ,rol)::q -> 
      let () = debug (Printf.sprintf "Generating code for role %s" str) in
      let g : string = (generate_from_role rl visib fut state_to_node stategraph str typ rol) in
      let () = debug (Printf.sprintf "Generation done for role %s" str) in
      (g^(generate_from_session visib fut state_to_node stategraph q rl))



let generate_proxy (session_name:string) (g:graph) (s:session) (visib:visib)
    (fut:future) (state_to_node:state_node) (stategraph:stategraph) : string =
  let rl = s in
  let res = 
    ("\n(*******************)\n(* Proxy functions *)\n(*******************)\n\n")
    ^("\n\nopen Global\nopen Pi\nopen Crypto\nopen Data\nopen Prins\nopen "^session_name^"_protocol\n\n")
    ^"let debug_impl = debug \"impl\"\n\n" 
    ^(generate_from_session visib fut state_to_node stategraph rl rl) 
  in
  let () = debug "Generation of the proxy module done!" in
  res
   
let generate_code (s:session) (g:graph) (visib:visib) antivisib (fut:future)
    (fwd:fwd) (state_to_node:state_node) (stategraph:stategraph) 
    : string = 
  let rl = s in
  let lr = List.map (function (r,_,_) -> r) rl in
  let res = 
    (global_def stategraph)
    ^"let session = sha1 (utf8 (cS\""
    ^(print_stategraph stategraph)^"\"))\n\n"
    ^(Geninterface.gen_prins rl fut)
    ^(gen_empty_store rl fut)
    ^(Geninterface.genEvents g stategraph)^"\n\n"
    ^(generate_wired lr visib antivisib fut fwd stategraph)
(*    ^("\n(*******************)\n(* Proxy functions *)\n(*******************)\n\n")
    ^(generate_from_session visib fut state_to_node stategraph rl rl vl) *)
  in
  let () = debug "Generation of the module done!" in
  res
   
