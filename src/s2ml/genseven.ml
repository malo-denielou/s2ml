(********************************************************************)
(* Session - Implementation                                         *)
(*                                                                  *)
(* genseven.ml                                                      *)
(********************************************************************)
(* $Id: genseven.ml 9310 2009-01-27 15:07:25Z denielou $ *)

open Common
open Syntax
open Graph

(* Header Generation *)


let gen_prins (rl:role_list) (fut:future) = 
  let macs = remove_duplicates 
    (List.flatten (List.map 
                     (function ((m,a),l) -> 
                        List.map 
                          (function r -> r^m.label) l) fut))  in
  let macs = String.concat ";\n  " (List.map (function s -> s^" : bytes ") macs)
  in 
  let roles = List.map (function (r,_,_) -> r) rl in
  let pairs = List.filter (function (x,y) -> x<>y) (product roles roles) in
  let keys = String.concat ";\n  " 
    (List.map (function (x,y)-> "key_"^x^y^" : bytes ") pairs) in
    "public type macs = {\n  "^macs^"}\n"^
    "public type keys = {\n  "^keys^"}\n"^
    "public type header = {\n"^
    "  ts : int ;\n"^
    "  sid : bytes }\n"^
    "public type store = \n"^
    "  { macs : macs ; keys : keys ; header : header}\n"^
  (String.concat "" 
      (List.map 
         (fun r -> 
            "val empty_store_"^r^" : principal -> bool -> store\n") roles))

(* WiredTypes *)
let genWiredTypes visib =  
  let states = remove_duplicates (List.map fst visib) in
  let rec generate_wired_type visib : state list -> string list= function 
      [] -> []
    | state::q -> 
        let gen_type sv = 
          String.concat "\n" 
            (List.map (function v ->
                         Printf.sprintf "  | Wired_in_%s_of_%s of string * store"
                           (print_state state)
                           (print_vi v))
               sv)
        in
        let lv = remove_duplicates (subset_assoc state visib) in
        (Printf.sprintf "\npublic type wired_%s =\n"
         (print_state state))::
          (String.concat "\n" (List.map gen_type lv))
        ::(generate_wired_type visib q)
  in
  generate_wired_type visib states
 
(* Predicates and Events *)

let genEvents (g:graph) (sg:stategraph) =
  let g = List.rev g in
  let lsend =
    List.map 
      (function (_,m,_)-> 
         Printf.sprintf "| Send_%s of (principal * bytes * int)\n" m.label
      ) g in
  let lrecv =
    List.map 
      (function (_,m,_)-> 
         Printf.sprintf "| Recv_%s of (principal * bytes * int)\n" m.label
      ) g in
  let genp = ("\n| P_start of store\n"::
    List.map (function (_,m,_)-> 
                Printf.sprintf "| P_%s of store\n" 
                  m.label) g) in
  let genpp = ("\n| P'_start of store\n"::
    List.map (function (_,m,_)-> 
                Printf.sprintf "| P'_%s of store\n" 
                  m.label) g) in
  let aftersendingstates = remove_duplicates (
    (List.map (function ((_,s),_,_) -> s) sg) @
      (List.map (function (_,_,(s,_)) -> s) sg)) in
  let afterrecvingstates =  remove_duplicates (
    (List.map (function ((s,_),_,_) -> s) sg) @
      (List.map (function (_,_,(_,s)) -> s) sg)) in
  let genq = ("\n" ::
    List.map (fun s -> Printf.sprintf "| Q_%s of store\n" (print_state s)) 
      aftersendingstates) in
  let genqp = ("\n" ::
    List.map (fun s -> Printf.sprintf "| Q'_%s of store\n" (print_state s)) 
      afterrecvingstates) in
  let genqpp = ("\n" ::
    List.map (fun s -> Printf.sprintf "| Q''_%s of store\n" (print_state s)) 
      afterrecvingstates) in
  lsend @ lrecv @ genp @ genpp @ genq @ genqp @ genqpp @ ["\n"]

(* Axioms *)
  
  
let genqaxioms (sg:stategraph) =
  let aftersendingstates = remove_duplicates (
    (List.map (function ((_,s),_,_) -> s) sg) ) (* @
                   (List.map (function (_,_,(s,_)) -> s) sg)) *) in
  let initqstates = substract (List.map (function ((_,s),_,_) -> s) sg) 
    (List.map (function (_,_,(s,_)) -> s) sg) in
  let prevstates = remove_duplicates (
    (List.map (function ((p,s),_,_) -> (s,p)) sg) ) in
  let from_state2 = function state ->
    let start = "\n  => Q_"^(print_state state)^"(s)\n" in 
    let (role,seq) = state in
    let (_,m) = List.hd seq in
    let send = "Send_"^m.label^"("^role^",s.header.sid,s.header.ts) /\\ \n" in
    let prev = subset_assoc state prevstates in
    let prevstores st =
      let start2 = 
        if snd st = [] then "" else "   (s'.header.sid = s.header.sid) /\\\n" in
      Printf.sprintf "assume !s,s'. (%s%s   Q'_%s(s')) %s"
        send
        start2
        (print_state st)
        start
    in
    (String.concat " \n\n" (List.map prevstores prev))^"\n\n"
  in
  let init_state state =
    "assume !s. Q_"^(print_state state)^"(s)\n\n"
  in 
  (List.map init_state initqstates)@  
    (List.map from_state2 aftersendingstates)


let genqpaxioms (visib:visib) (sg:stategraph) =
  let afterrecvingstates =  remove_duplicates (
    (List.map (function (_,_,(_,s)) -> s) sg)) in
  let initqpstates = substract (List.map (function (_,_,(_,s)) -> s) sg) 
    (List.map (function ((s,_),_,_) -> s) sg) in
  let prevstates = remove_duplicates (
    (List.map (function (_,_,(p,s)) -> (s,p)) sg) ) in
  let from_state2 = function state ->
    let (role,seq) = state in
    let (_,m) = List.hd seq in
    let prev = subset_assoc state prevstates in
    let onprevstates st =
      let vis = List.assoc st visib in
      let vis = List.filter
        (fun v -> let (_,seq)= List.hd v in let _,msg = List.hd seq in msg=m)
        vis in
      let from_seq vs =
        let rec foldvisib n = function
            [] -> 
              let start = 
                if snd st = [] then "" 
                else Printf.sprintf "  (s%d.header.sid = s.header.sid) /\\\n" n in
              (Printf.sprintf "!s%d. (\n%s%s"
                 n 
                 start
                 (Printf.sprintf "  (Q_%s(%s) )"
                    (print_state st) (Printf.sprintf "s%d" n)))
          | s::q -> 
              (Printf.sprintf "!s%d.%s /\\\n  (s%d.header.sid = s.header.sid) /\\\n%s)"
                 n 
                 (foldvisib (n+1) q)
                 n
                 (Printf.sprintf "  (Q_%s(s%d) \\/ Leak(s%d.vars.%s)"
                    (print_state s) n n (fst s)))
        in
        "\nassume !s."^
          (foldvisib 0 vs)^
          (")\n  => Q''_"^(print_state state)^"(s)\n\n"
           ^"assume !s. (Recv_"^m.label
           ^"(s.vars."^role^",s.header.sid,s.header.ts) /\\\n"
           ^"   Q''_"^(print_state state)^"(s))\n"
           ^"   => Q'_"^(print_state state)^"(s)\n")
      in
      (String.concat "" (List.map (from_seq) vis))
    in
    (String.concat "" (List.map onprevstates prev))
  in
  let init_state state =
    "assume !s. Q'_"^(print_state state)^"(s)\n\n"
  in 
  (List.map init_state initqpstates)@  
  (List.map from_state2 afterrecvingstates)

(* Send and Recv wired interface *)

let genrecvwired (visib:visib) (sg:stategraph) =
  let states = remove_duplicates (List.map fst visib) in
  let nextrecvg = List.map (function ((d,e),b,(a,c)) -> ((a,b,e),(d,b,(a,c)))) sg in
  let from_state = function state ->
    let from_visib = function v ->
      let (send,seq) = List.hd v in
      let (_,m,_) = List.hd seq in
      let edge = List.assoc (state,m,(send,seq)) nextrecvg in
      let (sendst,sendnext),_,(_,recvnext) = edge in
      let header =
        if snd state = [] then "" 
        else Printf.sprintf "    /\\ s'.header.sid = s.header.sid\n" in
      Printf.sprintf
        "\n  (?s'. ?x:string. wired_msg = Wired_in_%s_of_%s(x,s') \n%s    /\\ (Q''_%s(s')))"
         (print_state state) (print_vi v)
        header
        (*"Recv_"^m.label^"(s'.vars."^(fst state)^",s'.header.sid,s'.header.ts"^exvars^")\n        => "*)
        (print_state recvnext)
    in
    let post = 
      String.concat "  \\/ " (List.map from_visib (List.assoc state visib)) in
    Printf.sprintf 
      "\nprivate val receiveWired_%s:\n  s:store{Q_%s(s)} ->\n  wired_msg:wired_%s{%s}\n" 
      (print_state state) (print_state state) (print_state state) post
  in
  (List.map from_state states)
  

let gensendwired (sg:stategraph) =
  let from_edge = function ((state,next),m,_) ->
    let post = Printf.sprintf "Q_%s(s') %s %s\n    /\\ s'.header.sid = s.header.sid"
      (print_state next)
      wvars ovars
    in
    let rolevar = 
      if List.mem (fst state) m.write then (fst state) else "s.vars."^(fst state) in
    let send = "\n       /\\ Send_"^m.label^"("^(rolevar)^",s.header.sid,Succ(s.header.ts))" in
    Printf.sprintf 
      "\nprivate val sendWired_%s_%s:\n  string -> \n  s :store {Q'_%s(s)%s} ->\n  s':store {%s}\n" 
      m.label
      (print_state state)
      (print_state state)
      send
      post
  in
  List.map from_edge sg

(* Proxy functions *)

let genproxy (state_to_node:state_node) (sg:stategraph) =
  let sendstates = remove_duplicates (
    (List.map (function ((s,_),_,_) -> s) sg)) in
  let recvstates = remove_duplicates (
    (List.map (function (_,_,(s,_)) -> s) sg)) in
(*  let states = sendstates @ recvstates in*)
  let from_state prime = function state ->
    let role = fst state in
    let n = List.assoc state state_to_node in
    Printf.sprintf 
      "\nprivate val %s: s:store{Q%s_%s(s)} -> msg%d -> result_%s\n"
      (print_state state)
      prime
      (print_state state)
      n role
  in
  (List.map (from_state "'") sendstates)
  @(List.map (from_state "") recvstates)
    

(* Queries *)

let genqueries (visib:visib) (sg:stategraph) =
  let states = remove_duplicates (List.map fst visib) in
  let afterrecvingstates =  remove_duplicates (
    (List.map (function (_,_,(_,s)) -> s) sg)) in
  let from_stateq = function state ->
    let _,seq = state in
    let edge = if seq = [] then "start" else
      let _,m  = List.hd seq in
      m.label in
    state,"Q","P",
    Printf.sprintf 
      "!s. Q_%s(s) => P_%s(s)"
      (print_state state)
      edge
  in
  let from_stateqp = function state ->
    let _,seq = state in
    let edge = if seq = [] then "start" else
      let _,m  = List.hd seq in
      m.label in
    state,"Q'","P'",
    Printf.sprintf 
      "!s. Q'_%s(s) => P'_%s(s)"
      (print_state state)
      edge
  in
  let impl = (List.map from_stateq states)@
    (List.map from_stateqp afterrecvingstates) in
  let rec ask hyp = function []-> []
    | (state,q,p,s)::r ->
        let _,seq = state in
        let edge = if seq = [] then "start" else
          let _,m  = List.hd seq in
          m.label in
        (Printf.sprintf "\nask !s. (%s%s_%s(s)) => %s_%s(s)\n"
           (hyp^(String.concat "" (List.map (function (_,_,_,s)-> "("^s^") /\\\n  ") r)))
           q
           (print_state state)
           p
           edge)::(ask (hyp^"("^s^") /\\\n  ") r)
  in
  ask "" impl
      
(*!!!!!!!!!!!!!!!!!!!!!! Warning: File not changed completely to v 1.5 !!!!!!!!!!!!!!!!!!!!*)
      
let gen_macs vl visib =
  let rec hashes = function
      [] -> "Utf8(Literal (\"\"))"
    | v::q -> 
        (match List.assoc v vl with
           | "string" -> "Concat(s.hashes.h"^v^","^(hashes q)^")"
           | "int" -> "Concat(s.hashes.h"^v^","^(hashes q)^")"
           | "principal" -> "Concat(s.hashes.h"^v^","^(hashes q)^")"
           | _ -> assert false
        )
  in
  let mac_aux (st,l) =
    let rec unfold l =
      match l with
          [] -> []
        | (s,v)::q -> 
            let lv = String.concat "" (List.map 
              (fun x -> Printf.sprintf "   (s0.hashes.h%s = s.hashes.h%s) /\\\n" x x) v)
            in
            let va = String.concat "" v in (
              "\nprivate val mac_verify_"^(fst st)^"_"^(print_state s)^"_"^va^" : \n"^
                "  (ts:int) -> \n"^
                "  (s:store) -> \n"^
                "  (k:(;s.vars."^(fst s)^",s.vars."^(fst st)^") mackey) -> \n"^
                "  (m:bytes) -> \n"^
                "  (c':bytes) { \n"^
                "   ?s0.  (s0.header.sid = s.header.sid) /\\\n"^
                lv^
                "   (s0.vars."^(fst s)^" = s.vars."^(fst s)^") /\\\n"^
                "  (Q_"^(print_state s)^"(s0) \\/ Leak(s0.vars."^(fst s)^"))}\n"
            )::(unfold q)
    in
    unfold (List.flatten l)
  in
  let mac_content (st,l) =
    let rec unfold l =
      match l with
          [] -> []
        | (s,v)::q -> 
            let va = String.concat "" v in (
              "\nprivate val content_"^(print_state s)^"_"^va^" : \n"^ 
                "  (ts:int) -> \n"^
                "  (s:store) -> \n"^
                "  (c:bytes) { \n"^
                "  c = Concat(Concat(s.header.sid,Utf8(Literal(SofI(ts)))),Concat(Utf8(Literal (\""^
                (print_state s)^"\")),"^(hashes v)^"))}\n"
            )::(unfold q)
    in
    unfold (List.flatten l)
  in
  (remove_duplicates (List.flatten (List.map mac_content visib)))@
  (remove_duplicates (List.flatten (List.map mac_aux visib)))
  


let gen_keys vl visib =
  let rec hashes = function
      [] -> "Utf8(Literal (\"\"))"
    | v::q -> 
        (match List.assoc v vl with
           | "string" -> "Concat(s.hashes.h"^v^","^(hashes q)^")"
           | "int" -> "Concat(s.hashes.h"^v^","^(hashes q)^")"
(*           | "int" -> "Concat(Hash(Utf8(Literal(SofI(s.vars."^v^")))),"^(hashes q)^")"*)
           | "principal" -> "Concat(s.hashes.h"^v^","^(hashes q)^")"
           | _ -> assert false
        )
  in
  let usage (st,l) = 
    let rec unfold l =
      match l with
          [] -> []
        | (s,v)::q -> (*let va = String.concat "" v in *)
            ("(?s. (s.vars."^(fst s)^"=sp) /\\ (s.vars."^(fst st)^"=rp) /\\\n"^
                "  (c = Concat(Concat(s.header.sid,Utf8(Literal(SofI(s.header.ts)))),Concat(Utf8(Literal (\""^
                (print_state s)^"\")),"^(hashes v)^"))) /\\\n"^
                "  (Q_"^(print_state s)^"(s) \\/ Leak(sp)))"
            )::(unfold q)
    in
    unfold (List.flatten l)
  in
  [Printf.sprintf 
     "type (;sp:principal,rp:principal) mackey = \n  (c:bytes{%s}) hkey\n"
     (String.concat "\n  \\/ " (remove_duplicates (List.flatten (List.map usage visib))))
  ;
   "\n\nval get_mackey: (p1:principal) -> (p2:principal) -> (;p1,p2) mackey\n\n\n"
  ]

let gensevenstringfix  (sg:stategraph) =
  let states = remove_duplicates 
    (List.map (function ((state,next),m,_) -> next) sg) in
  let rec product l =
    match l with
        [] -> []
      | st::q -> (List.map (function st' -> 
                              Printf.sprintf "assume %S <> %S\n" 
                                (print_state st)
                                (print_state st')) q)
          @(product q) in
  "\n\n"::(product states)@["\n\n"]
                         

let generate_protocol (s:session) (visib:visib) (known:known) (fut:future)
    (state_to_node:state_node)  (g:graph) (sg:stategraph) =
  let vl,tl,rl = s in
  let vl = (List.map (function (r,_,_) -> (r,"principal")) rl)@vl in
  let header = 
"(*****************************)\n"^
"(* Generated Seven Interface *)\n"^
"(*****************************)\n" in
  let opening = "open Data\nopen Prins\nopen Crypto\n" in
  let basetypes = "public type principal = string\n\n" in
  let store = gen_prins vl rl fut in
  let wiredheader = "\n\n(* Wired types *)\n" in
  let wiredtypes = genWiredTypes vl visib in
  let eventsheader = "\n\n\n(* Events and predicates declaration *)\n" in
  let predsdecl = "\npublic type preds = \n\n| Leak of principal\n\n" in
  let events = genEvents vl g sg in
  let stringfix = gensevenstringfix sg in
  let qaxioms = genqaxioms vl known sg in
  let qpaxioms = genqpaxioms vl known visib sg in
  let macs = gen_macs vl visib in
  let keys = gen_keys vl visib in
  let recvwired = remove_duplicates (genrecvwired vl known visib sg) in
  let sendwired = remove_duplicates (gensendwired vl sg) in
  header::opening::basetypes::store::wiredheader::wiredtypes@
    (eventsheader::predsdecl::events)@
    (stringfix)@
    (keys)@
    (qaxioms)@
    (qpaxioms)@
    (macs)@
    (recvwired)@
    (sendwired)

let generate (session_name:string) (s:session) (state_to_node:state_node) (sg:stategraph) =
  let vl,tl,rl = s in
  let vl = (List.map (function (r,_,_) -> (r,"principal")) rl)@vl in
  let header = 
"(*****************************)
(* Generated Seven Interface *)
(*****************************)\n" in
  let opening = "open Data\nopen Prins\nopen Crypto\nopen "^session_name^"_protocol\n"^(create_var_i vl) in
  let proxytypes = Geninterface.generate_from_session_i "public " rl in
  let moreproxytypes = genproxy  state_to_node sg in
   header::opening::(proxytypes)@
    (moreproxytypes)












(***********************************GARBAGE*******************************)
(*
let genpaxioms vl (state_to_node:state_node) (g:graph) =
  let init = "assume !s. P_start(s) <=> true\n\n" in 
  let ntr = List.map (function ((r,_),n)-> (n,r)) state_to_node in
  let nextg = List.map (function (_,m,p) -> (p,m)) g in
  let lv = List.map fst vl in
  let edge (n,m,p) =
    let args = if m.write = [] then "" else
      if List.length m.write = 1 then ",s.vars."^(List.hd m.write) else
	(","^(String.concat "," (List.map (fun x -> "s.vars."^x) m.write))) 
    in
    let bv = substract m.write lv in
    let lv = List.map 
      (fun x -> Printf.sprintf "    (s'.vars.%s = s.vars.%s) /\\\n" x x) bv
    in
    let prev msg =
      (Printf.sprintf "(?s'.(s'.header.sid = s.header.sid) /\\\n%s    P'_%s(s'))"
         (String.concat "" lv)
         msg.label)
    in
    let before = (subset_assoc n nextg) in
    let disj = 
      if before = [] 
      then ("(?s'.(s'.header.sid = s.header.sid) /\\\n"^
              (String.concat "" lv)^
              "    P'_start(s'))")
      else (String.concat "\\/" (List.map prev before)) in
    (Printf.sprintf 
       "assume !s. P_%s(s) => \n  ((Send_%s(%s,s.header%s) \\/ Leak(%s)) /\\\n   (%s))\n\n"
       m.label
       m.label
       ("s.vars."^List.assoc n ntr)
       (args)
       ("s.vars."^List.assoc n ntr)
       disj
    )
  in
  let edge2 (n,m,p) =
    let args = if m.write = [] then "" else
      if List.length m.write = 1 then ",s.vars."^(List.hd m.write) else
	(","^(String.concat "," (List.map (fun x -> "s.vars."^x) m.write))) 
    in
    let bv = substract m.write lv in
    let lv = List.map 
      (fun x -> Printf.sprintf "    (s'.vars.%s = s.vars.%s) /\\\n" x x) bv
    in
    let prev msg =
      (Printf.sprintf "\nassume !s,s'. ((Send_%s(%s,s.header%s) \\/ Leak(%s))  /\\\n    (s'.header.sid = s.header.sid) /\\\n%s    P'_%s(s'))\n => P_%s(s)\n"
         m.label
         ("s.vars."^List.assoc n ntr)
         (args)
         ("s.vars."^List.assoc n ntr)
         (String.concat "" lv)
         msg.label
         m.label)
    in
    let before = (subset_assoc n nextg) in
    if before = [] 
    then (Printf.sprintf 
            "\nassume !s,s'. ((Send_%s(%s,s.header%s) \\/ Leak(%s)) /\\\n    (s'.header.sid = s.header.sid) /\\\n%s)\n  => P_%s(s)\n"
            m.label
            ("s.vars."^List.assoc n ntr)
            (args)
            ("s.vars."^List.assoc n ntr)
            ((String.concat "" lv)^
               "    P'_start(s')")
            m.label)
    else (String.concat "\n" (List.map prev before))
  in
  init::(List.map edge g)@(List.map edge2 g)

let genppaxioms vl (state_to_node:state_node) (g:graph) =
  let lv = List.map fst vl in
  let init = "assume !s. P'_start(s) <=> true\n\n" in 
  let ntr = List.map (function ((r,_),n)-> (n,r)) state_to_node in
  let same x = Printf.sprintf "    (s'.vars.%s = s.vars.%s) /\\\n" x x in
  let edge (n,m,p) =
    let args = if m.read = [] then "" else
      if List.length m.read = 1 then ",s.vars."^(List.hd m.read) else
	(","^(String.concat "," (List.map (fun x -> "s.vars."^x) m.read))) 
    in
    (Printf.sprintf 
       "assume !s. P'_%s(s) => \n  ((Recv_%s(%s,s.header%s) \\/ Leak(%s)) /\\\n   (?s'.%s%s    P_%s(s')))\n\n"
       m.label
       m.label
       ("s.vars."^List.assoc p ntr)
       (args)
       ("s.vars."^List.assoc p ntr)
       ("(s'.header.sid = s.header.sid) /\\\n")
       (String.concat "" (List.map same lv))
       m.label
    )
  in
  let edge2 (n,m,p) =
    let args = if m.read = [] then "" else
      if List.length m.read = 1 then ",s.vars."^(List.hd m.read) else
	(","^(String.concat "," (List.map (fun x -> "s.vars."^x) m.read))) 
    in
    (Printf.sprintf 
       "assume !s. ((Recv_%s(%s,s.header%s) \\/ Leak(%s)) /\\\n   (?s'.%s%s    P_%s(s')))\n  => P'_%s(s) \n\n"
       m.label
       ("s.vars."^List.assoc p ntr)
       (args)
       ("s.vars."^List.assoc p ntr)
       ("(s'.header.sid = s.header.sid) /\\\n")
       (String.concat "" (List.map same lv))
       m.label
       m.label
    )
  in
  init::(List.map edge g)@(List.map edge2 g)
*)
