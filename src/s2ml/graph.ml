(********************************************************************)
(* Session - Implementation                                         *)
(*                                                                  *)
(* graph.ml                                                         *)
(********************************************************************)
(* $Id: graph.ml 9310 2009-01-27 15:07:25Z denielou $ *)

(*#use "c:/Documents and Settings/t-ricarc/inria-msr-sec/lang-sec/session-types/src/syntax.ml";;*)
(*#use "/Users/ricardocorin/inria-msr-sec/lang-sec/session-types/src/syntax.ml";;*)

open Common
open Syntax 

let debug = gen_debug debug_graph  "graph"
let debug_visibility = gen_debug debug_visibility  "graph"


(* type graph is a list of edges *)
type graph = (node * message * node) list

(* type assoc relates the nodes numbers to the roles*)
type node_to_role = (node * string) list

(* Errors *)
exception Graph_error of string

(* Associate local node numbers to roles *)
let rec assoc_from_session : role_list -> node_to_role = 
  let rec assoc_from_role_type (r:string) = function
    | End n -> [n,r]
    | Goto _ -> []
    | Send (n,l) -> (n,r)::(assoc_from_msg_list r l)
    | Receive (n,l) -> (n,r)::(assoc_from_msg_list r l)
  and assoc_from_msg_list (r:string) = function 
      [] -> []
    | (_,rt)::q -> (assoc_from_role_type r rt)@(assoc_from_msg_list r q)
  in function
      [] -> []
    | (r,_,rt)::q -> (assoc_from_role_type r rt)@(assoc_from_session q);;


(* The node substitution transforms the local node numbers to global ones *)
(* Normalize the node substitution *)
let rec normalize = function 
  | [] -> []
  | (a,b)::l ->  
      if List.mem (a,b) l 
      then normalize l 
      else
	begin
	  if List.mem_assoc a l 
	  then 
	    let newDest = List.assoc a l in
	    debug (Printf.sprintf "normalizing a=%d newDest=%d b=%d" a newDest b);
	    (normalize ((newDest,b)::l)) 
	  else ((a,b)::normalize l) 
	end


(* Transforms a session graph into a list of edges *)
let flatten : role_type -> role_type list =
    let rec flatten_aux : role_type list -> role_type list = 
      let rec aux = function
	  [] -> raise (Graph_error "flatten")
	| [msg,s] -> begin 
	      match s with
		  End _ -> [msg,s]
		| _ -> [msg,Goto (node_of s)]
	  end
	|(msg,s)::q -> (match s with
	      End _ -> (msg,s)
	    | _ -> (msg,Goto (node_of s)))::(aux q) 
      and aux_top = function
	  [] -> []
	|(_,s)::q -> (match s with
	      End _ -> []
	    | _ -> [s])@(aux_top q)
      in function
	  [] -> []
	| s::q -> begin 
	      match s with
		  End _ -> []
		| Goto _ -> []
		| Send (n,mrtl) -> (Send (n,aux mrtl))::(flatten_aux (aux_top mrtl))
		| Receive (n,mrtl) -> (Receive (n,aux mrtl))::(flatten_aux (aux_top mrtl))
	  end @ (flatten_aux q)
    in
      function s -> flatten_aux [s] 

(* Selects only a node from a flatten version of a protocol *)
let rec filter_node (n:node) = function
    [] -> []
  | (Send (i,l))::q -> if i = n then l else filter_node n q
  | _ -> raise (Graph_error "filter_node")

(* Selects only a node from a flatten version of a protocol *)
let rec find_node (n:node) : role_type list -> bool = function
    [] -> false
  | (Send (i,l))::q -> (i = n) || (find_node n q)
  | _ -> raise (Graph_error "find_node")

(* Test for End of protocol *)
let is_End (rt:role_type) : bool =
  match rt with
    | End _ -> true
    | _ -> false

(* Presence test *)
let rec isin (msg:message) : (message * role_type) list -> bool = function 
    [] -> false
  | (m,_)::r -> 
      (m.label=msg.label) || isin msg r

(* Strict Absence test *)
let rec notin (msg:message) : (message * role_type) list -> bool = function 
    [] -> true
  | (m,_)::r -> (m.label<>msg.label) && notin msg r

(* Strict Absence test *)
let rec notin_hard (msg:message) : (message * role_type) list -> bool = function 
    [] -> true
  | (m,_)::r ->
      if (m.label=msg.label) 
      then raise (Graph_error ("function 'notin' : msg "^(m.label)^" is wrong")) 
      else notin_hard msg r

(* Unique Presence test *)
let rec isin_once (msg:message) : (message * role_type) list -> bool = function 
    [] -> false
  | (m,_)::r -> 
      let _ = debug (Printf.sprintf "is %s present only once?" m.label) in 
      if (m.label=msg.label) then notin_hard msg r else isin_once msg r

(* Inclusion test *)
let rec included 
    (msg_list_1: (message * role_type) list)
    (msg_list_2: (message * role_type) list) = 
  match msg_list_1 with 
    | [] -> true
    | (m,_)::b -> (isin m msg_list_2) && (included b msg_list_2)

(* Void intersection test *)
let rec void_intersect
    (msg_list_1: (message * role_type) list)
    (msg_list_2: (message * role_type) list) = 
  match msg_list_1 with 
    | [] -> true
    | (m,_)::b -> 
(*        let _ = debug (Printf.sprintf "%s shouldn't be present anymore" (fst m)) in *)         
        (notin_hard m msg_list_2) && (void_intersect b msg_list_2)

(* Inclusion test with strictness constraint *)
let rec included_once 
    (n:node)
    (msg_list_1: (message * role_type) list)
    (msg_list_2: (message * role_type) list) = 
  try begin
  match msg_list_1 with 
    | [] -> true
    | (m,_)::b -> 
        let _ = debug (Printf.sprintf "is %s present in the expected msg of node %i?" m.label n) in
        if (isin_once m msg_list_2) 
        then 
          let _ = debug (Printf.sprintf "%s is present once" m.label) in
          (included_once n b msg_list_2)
        else 
          let _ = debug  (Printf.sprintf "%s is not present" m.label) in
          let _ = (void_intersect b msg_list_2) in
          false
    end
  with _ -> raise (Graph_error "Problem in function 'included_once'")
(*
  let role_list = List.map snd pingpong in
  let action_list = List.concat (List.map flatten role_list) in
  let rec split sl rl = function
      [] -> (sl,rl)
    | a::q -> 
        begin
          match a with
            | Send _ -> split (sl@[a]) rl q
            | Receive _ -> split sl (rl@[a]) q
            | _ -> raise (Graph_error "split")
        end in
  split [] [] action_list 
*)


    
(**********************)
(* Graph Manipulation *)
(**********************)

(* From a graph we select the edges starting from node k *)
let rec select k : graph -> graph * graph = function
    [] -> [],[]
  | (i,msg,j)::q -> 
      let a,b = select k q in
      if k = i then ((i,msg,j)::a,b) else (a,(i,msg,j)::b)
        
(* From a graph we get the unique edge determined by a node and a message *)
let rec extract m : graph -> node*message*node = function
    [] -> assert false
  | (i,p,j)::q -> if p=m then (i,p,j) else extract m q

(* Sorts by common starting nodes *)
let rec sort_by_send : graph -> (node * graph) list = function
    [] -> []
  | (i,m,j)::q -> 
      let a,b = select i ((i,m,j)::q) in
      (i,a)::(sort_by_send b)

(* Goes in depth through the graph and applies a function on the way *)
let depth_check (g:graph) f (initial:node) =
  let rec check acc k =
    let sl,_ = select k g in
    List.fold_left 
      (function b -> function (_,_,j) -> 
        b && (List.mem j acc || (f j && check (j::acc) j))) true sl
  in
  check [] initial

let rec find label (g:graph) = 
  match g with
    | [] -> assert false
    | (_,m,_)::q -> if label = m.label then m else find label q




(********************************************************)
(* Construction of the graph from the syntactic session *)
(********************************************************)

let graph_from_session (s:session) =
  
  (* Global definitions *)
  let rl = s in
  let role_list = List.map (fun (_,_,r)->r) rl in
  let action_list = List.concat (List.map flatten role_list) in 
  
  let rec find_receiver m = function
    | [] -> raise (Graph_error (Printf.sprintf "The message %s has no receiver." m.label))
    | (Receive (n,lm))::b -> (* Should perhaps also check that m is only once in lm *) 
        if List.exists (fun (msg,_) -> msg.label = m.label) lm
        then (find_none m b ; 
              let l = List.filter (fun (msg,_) -> msg.label = m.label) lm in
              if List.length l > 1 
              then 
                raise (Graph_error (Printf.sprintf "The message %s is expected twice. " m.label))
              else (n,List.hd l))
        else find_receiver m b
    | _ -> assert false
  and find_none m = function 
    | [] -> ()
    | (Receive (n,lm))::b -> 
        if List.exists (fun (msg,_) -> msg.label = m.label) lm
        then raise (Graph_error (Printf.sprintf "The message %s has more than one receiver." m.label))
        else find_none m b
    | _ -> assert false in

  (* Splits a list of actions into Sends and Receives *)
  let rec split sl rl = function
      [] -> (sl,rl)
    | a::q -> begin match a with
        | Send _ -> split (sl@[a]) rl q
        | Receive _ -> split sl (rl@[a]) q
        | _ -> raise (Graph_error "split")
      end in
  let send_list,recv_list = split [] [] action_list in

  let node_aliases = ref [] in
  let send_trans = ref [] in
  let seen = ref [] in
  let up a b = a:= b::!a in

  let rec build_from_send (i:node) =
    if List.mem i !seen then []
    else
      (* We find the messages sent from node i *)
      let msg_sent_from_i = filter_node i send_list in
      let lm = String.concat "," (List.map (function (m,_) -> m.label)
                                    msg_sent_from_i) in
      let () = 
        debug (Printf.sprintf "Looking for messages %s sent from node %d ... " lm i) in
      let () = up seen i in
      (* We look for a receiver for each message *)
      List.flatten (List.map (build_edge i) msg_sent_from_i)
  and build_edge (i:node) = function (m,send_goto) ->
    let () = debug (Printf.sprintf "Message %s ... " m.label) in
    let j,(msg,recv_goto) = find_receiver m recv_list in
    let next_node = node_of recv_goto in
    let () = if (m.payload <> msg.payload) then 
      raise (Graph_error 
               (Printf.sprintf "The message %s has two different payload types." m.label)) in
    let edge = (i,m,next_node) in
    let () = up node_aliases (j,i) in
    let () = up send_trans (i,m,node_of send_goto) in
    edge::(build_from_send next_node)
  in  
  let _ = debug (Printf.sprintf "Graph generation started.") in
  let g = build_from_send 0 in
  let _ = debug (sprint_session rl) in
  let _ = debug (Printf.sprintf "Graph generation completed.") in
  let send_g = remove_duplicates !send_trans in
  (List.rev g,send_g,!node_aliases)



(***********************************)
(* Compute the visibility function *)
(***********************************)


type seq = (role_name * message) list
type extgraph = ((role_name * seq * seq) * message * (role_name * seq * seq)) list
type visibility = (role_name * seq * seq list) list

type state = role_name * seq
type edge = (state * state) * message * (state * state)
type visib = (state * (state list) list) list
type antivisib = (edge * state list) list
type stategraph = edge list
type state_node = (state * node) list
type future = ((message*state) * role_name list) list
type fwd_macs = ((edge * (role_name * message * role_name) list) list)
type fwd_keys = (edge * (role_name * role_name) list) list
type fwd = fwd_macs * fwd_keys
    
(* pp *)
let rec print_visibility (l:(role_name * seq * seq list) list) =
  match l with
      [] -> ""
    | (r,s,v)::q -> (printword_state (r,s))^(print_l v)^(print_visibility q)
and print_sequence ml =
  let ml = List.rev ml in
  (if ml = [] then "start" else String.concat "_" (List.map (function (r,m) ->
                                                           m.label) ml))
and print_state (r,ml) = 
  let ml = List.rev ml in
  Printf.sprintf "%s_%s" r (print_sequence ml)
and print_state_display (r,ml) = 
  let ml = List.rev ml in
  Printf.sprintf "%s: %s" r 
    (if ml = [] then "start" 
     else String.concat "" (List.map (function (r,m) -> m.label) ml))
and printword_state (r,ml) =
  Printf.sprintf "\n  Role %s, at point %s\n    " r (print_seq ml) 
and print_msg m = m.label (*(String.concat "," m.read)^ *)
and print_seq l = 
  "["^(String.concat ";" 
          (List.map (function (s,m)-> s^","^(print_msg m)) l))^"]"
and print_seq_wo_vars l= 
  "["^(String.concat ";" 
         (List.map (function (s,m)-> s^","^m.label) l))^"]"
and print_l l =
  "["^(String.concat ";" (List.map print_seq l))^"]"
and print_ln (sg:(seq * message * seq) list)= 
  match sg with
      [] -> ""
    | (seq,m,v)::q ->
        (Printf.sprintf "\n  The receiver of message %s, goes from %s to %s"
            m.label
            (print_seq_wo_vars seq)
            (print_seq_wo_vars v)
        )^print_ln q
and print_gr (sg:extgraph)= 
  match sg with
      [] -> ""
    | ((r,s,ss),m,(rr,v,vv))::q ->
        (Printf.sprintf "\n  At point %s, after %s sent message %s (and goes to %s), receiver %s goes from state\n   %s to\n    %s"
            (print_seq_wo_vars s)
            r
            m.label
            (print_seq_wo_vars ss)
            rr
            (print_seq_wo_vars v)
            (print_seq_wo_vars vv)
        )^print_gr q
and print_dest (r,vars,fv) =
  Printf.sprintf "role %s (%s)" r (String.concat "," vars)
and print_dest2 r =
  Printf.sprintf "role %s" r
and print_fut = function
    [] -> ""
  | ((a,m,s),l)::q -> 
      (Printf.sprintf "\n  At point %s, the message %s sent by %s needs to be MACed for %s"
         (print_seq_wo_vars s)
         m.label
         a
         (String.concat " and " (List.map print_dest l))
      )^(print_fut q)
and print_futur : future -> string = function
    [] -> ""
  | ((m,s),l)::q -> 
      (Printf.sprintf "\n  At point %s, the message %s needs to be MACed for %s"
         (print_state_display s)
         m.label
         (String.concat " and " (List.map print_dest2 l))
      )^(print_futur q)
and print_fwdm = function
    [] -> ""
  | (((s,_),msg,_),l)::q -> 
      (Printf.sprintf "\n  At point %s, the message %s needs to carry the macs %s"
         (print_state_display s)
         msg.label
         (String.concat "," (List.map
                               (function(r,m,r')->
                                  "Mac{"^r^r'^m.label)
                               l)))^
        (print_fwdm q)
and print_fwdk = function
    [] -> ""
  | (((s,_),msg,_),lrr)::q -> 
      (Printf.sprintf "\n  At point %s, the message %s needs to carry the keys (%s)"
         (print_state_display s)
         msg.label
         (String.concat "," (List.map (function r,rr -> r^rr) lrr)))^
        (print_fwdk q)
and print_known = function
    [] -> ""
  | ((r,s),(vars,still,hashes))::q ->
      (Printf.sprintf "\n  At point %s, role %s knows variables %s and valid hashes %s and new hashes %s"
         (print_seq s)
         r
         (String.concat "," vars)
         (String.concat "," still)
         (String.concat "," hashes)
      )^(print_known q)
and print_visib : visib -> string = function
    [] -> ""
  | (s,ls)::q -> 
      (Printf.sprintf "\n  At state %s, the visible sequences are [%s]"
         (print_state_display s)
         (print_ls ls))^
        (print_visib q)
and print_ls ls = String.concat ";" (List.map print_vi ls)
and print_vi = function
    [] -> ""
  | s::q -> (print_state s)^"__"^(print_vi q)
and print_stategraph = function
    [] -> ""
  | ((a,b),m,(aa,bb))::q ->
        (Printf.sprintf "\n  Message %s: sender from %s to %s and receiver from %s to %s"
           m.label
           (print_state a)
           (print_state b)
           (print_state aa)
           (print_state bb))^print_stategraph q


let nodes_to_partpast (g:graph) (l:(node*(role_name *seq)) list) (rl:role_list) =
  let _ = debug "Node to past conversion" in
  let rll = List.map (function (r,_,p)->(r,flatten p)) rl in
  let rec update (l:(node*(role_name *seq)) list) role = function
      [] -> l
    | (Send (i,lm))::q -> 
        let r,s = List.assoc i l in
        update ((List.map (function 
          | (m,Goto k) -> (k,(role,(role,find m.label g)::s))
          | (m,End k) -> (k,(role,(role,find m.label g)::s))
          | _ -> assert false) lm)@l) role q
    | (Receive (i,lm))::q -> 
        if List.mem_assoc i l 
        then update l role q 
        else update ((i,(role,[]))::l) role q
    | _ -> assert false
  in
  let temp = List.concat (List.map (function (role,r) -> update l role r) rll)
  in 
  List.sort compare (remove_duplicates temp)





let hist_to_state role h = 
  let rec aux roles = function
      [] -> []
    | (sender,m)::q -> 
        if sender = role then [] else  (* Indexing with visible instead of
                                              state seq *)
        if List.mem sender roles 
        then (aux (sender::roles) q) 
        else ((sender,m)::(aux (sender::roles) q))
  in
  aux [] h
    
let rec hist_to_visib r = function
    [] -> []
  | (rr,m)::q -> if r = rr then [] else (rr,m)::(hist_to_visib r q)


let compute_stategraph (g:graph) (role_assoc:(node*string)list) (rl:role_list) =
  let _ = debug (Printf.sprintf "[State] State graph calculation started.") in
  let sorted = sort_by_send g in
  let rec state_node seen state hist node = 
    if not (List.mem_assoc node sorted) (* node is final *)
    then (debug_visibility (Printf.sprintf "[State] Node %d is final"
                              node);
          [])
    else
      let lm = List.assoc node sorted in (* messages sent in node *)
      List.concat 
	(List.map 
           (state_msg seen state hist)
           lm)
  and state_msg seen state hist = function (n,m,p) ->
    let sender = List.assoc n role_assoc in
    let recver = List.assoc p role_assoc in
    let state_sender = List.assoc sender state in
    let state_recver = List.assoc recver state in
    let new_hist = (sender,m)::hist in
    let new_state = hist_to_state recver new_hist in
    let edge =
      (((sender,state_sender),(sender,new_state)),m,((recver,state_recver),(recver,new_state)))
    in
    if List.mem edge seen
    then []
    else
      edge::(state_node (edge::seen) ((sender,new_state)::(recver,new_state)::state) new_hist p)
  in
  let init_state = List.map (function (r,_,_) -> (r,[])) rl in
  let sg = remove_duplicates (state_node [] init_state [] 0) in
  let _ = debug (Printf.sprintf "[State] State graph calculation done.") in
  let psg = print_stategraph sg in
  let () = debug ("[State] State graph:"^psg) in
  sg


let compute_visib (sg:stategraph) (rl:role_list) : 
    visib * antivisib =
  let _ = debug (Printf.sprintf "[Visib] Visibility calculation starting.") in
  let lr = List.map (function (r,_,_) -> r) rl in
  let sendg = remove_duplicates (List.map (function ((a,b),m,a1) -> (a,((a,b),m,a1))) sg) in
  let first_role = List.hd lr in
  
  let c =ref 0 in
  let rec from_edge path = function
    | [] -> []
    | (((a,b),m,(aa,bb)) as e)::q -> 
        (if List.mem e path 
         then [] 
         else  (incr c ; ((e::path)::(from_edge (e::path) (subset_assoc bb sendg)))))
        @(from_edge path q)
  in
  let paths = (from_edge [] (subset_assoc (first_role,[]) sendg)) in
  let _ = debug_visibility (Printf.sprintf "[Visib] Nb of paths = %i " !c) in
  let _ = if !c < 200 then
      let spaths = String.concat "\n" 
        (List.map (function p -> print_stategraph p) paths) in
      debug_visibility ("Paths:\n"^spaths) in

  let path_to_state role p = 
    let rec aux roles = function
        [] -> []
      | st::q -> 
          if fst st = role then [] (* [vars,None,[]]  [] Change here to remove vars *)
          else
            if List.mem (fst st) roles 
            then (aux ((fst st)::roles) q) 
            else 
              st::(aux ((fst st)::roles) q)
    in
    aux [role] p
  in
  let seqs = List.map
    (function [] -> assert false
       | (((((a,b),m,(aa,bb)) as e)::q) as p) -> 
           let visib =  path_to_state (fst aa) (
             List.map 
               (function (((a,b),m,(aa,bb))) -> b)
               p) in
           (aa,visib),(e,visib)
    )
    paths
  in
  let seqs,antivisib = List.split seqs in
  let states = remove_duplicates (List.map (function (s,_) -> s) seqs) in
  let vi = List.map 
    (function (r,s)-> ((r,s),remove_duplicates (subset_assoc (r,s) seqs))) states in
  let _ = debug (Printf.sprintf "[Visib] Visibility calculation done.") in
  let _ = debug ("Visib:"^(print_visib vi)) in
  vi,antivisib


let compute_statenodeassoc (sg:stategraph) (rl:role_list) : state_node=
  let _ = debug (Printf.sprintf "[State] State to node conversion.") in
  let rll = List.map (function (r,_,p)->(r,flatten p)) rl in
  let conv r = function
    | Send (i,lm) -> List.map (function (m,n) -> (r,m.label),(i,node_of n)) lm
    | Receive (i,lm) -> List.map (function (m,n) -> (r,m.label),(i,node_of n)) lm
    | _ -> assert false in
  let rn = List.flatten (List.flatten (List.map (function (r,lm)-> List.map (conv r) lm) rll))
  in 
  let s = String.concat "\n" 
    (List.map (function ((r,m),(i,j)) -> Printf.sprintf "For role %s, message %s goes from node %d to node %d"
                 r m i j) rn)
  in
  let _ = debug (Printf.sprintf "[State] Local node relation: \n%s" s) in
  
  let _ = debug (Printf.sprintf "[State] State to node intermediate point.") in
  let res =
    List.flatten
      (List.map (function (((r,a),b),m,((rr,aa),bb)) -> 
                   let _ = debug (Printf.sprintf "[State] Trying roles %s and %s and message %s" r
                                    rr m.label) in
                   let n,p = List.assoc (r,m.label) rn in
                   let nn,pp = List.assoc (rr,m.label) rn in
                   [((r,a),n);(b,p);((rr,aa),nn);(bb,pp)]) sg
      )
  in
  let _ = debug (Printf.sprintf "[State] State to node conversion done.") in
  res
  
(* Future relation and Mac forwarding *)
let compute_future (g:stategraph) (antivisib:antivisib) : future * fwd_macs =
  let sendg = List.map (function ((a,b),m,a1) -> (a,((a,b),m,a1))) g in
  
  let rec from_edge msg state path = function
    | [] -> 
        (*debug (Printf.sprintf "Future :        = completed");*) []
    | ((_,m,(_,(recver,recv_seq))) as e)::q -> 
        (*debug (Printf.sprintf "Future :        message %s" m.label);*)
        if (not (List.mem e path)) && msg.label <> m.label
        then
          (* visible sequence *)
          let v = List.assoc e antivisib in
          (if List.mem state v
           then (* Received here *) begin
             debug (Printf.sprintf "Future :        added %s in %s with path %s"
                      msg.label m.label (String.concat "," (List.map (function
                                                                          (_,m,_)->m.label) (e::path))));
             [recver,(e::path)] end
           else [] 
          )@(
            from_edge msg state (e::path)(subset_assoc (recver,recv_seq) sendg)
          )@(
            from_edge msg state path q 
          )
        else from_edge msg state path q 
  in
  let fut = remove_duplicates (
    List.map 
      (function (((a,b),m,(a1,b1))as e) -> 
         debug (Printf.sprintf "Future : Looking at message %s" m.label) ;
         ((m,a), (*remove_duplicates*)
            ((let v = List.assoc e antivisib in 
              if List.mem b v
              then (* Received here *)
                (fst a1,[e])
              else assert false)
             ::from_edge m b [e] (subset_assoc b1 sendg))))
      g)
  in
  let futur : future = remove_duplicates 
    (List.map (function (a,l) -> (a, remove_duplicates (
                 List.map (function (a,_) -> a) l))) fut)
  in
  let () = debug ("Future relation:"^(print_futur futur)) in
  let rec clean_path role (p:stategraph) = 
    match p with
        [] -> None
      | (((ro,st),ss),m,(r,rr))::q -> 
          if ro = role then 
            match clean_path role q with
                None -> Some q
              | Some q -> Some q
          else clean_path role q
  in
  let rec select_fwd (send:role_name) msg (recv:role_name) = function
      [] -> []
    | (((s,ss),m,(r,rr)) as e)::q -> 
(*        if List.mem recv (List.assoc (m,s) futur)
          then begin *)
          match clean_path (fst s) q with
            | None -> (e,(send,msg,recv))::(select_fwd send msg recv q)
            | Some q -> (e,(send,msg,recv))::(select_fwd send msg recv q)
(*        end
          else assert false *)
          (*select_fwd send msg recv (clean_path (fst s) q)*)
  in
(*  let rec clean_path role (p:stategraph) = 
    match p with
        [] -> assert false
      | ((s,ss),m,((r,st),rr))::q -> if r = role then q else clean_path role q
  in
  let rec select_fwd (send:role_name) msg (recv:role_name) = function
      [] -> []
    | (((s,ss),m,(r,rr)) as e)::q -> 
        if List.mem recv (List.assoc (m,s) futur)
        then (e,(send,msg,recv))::(select_fwd send msg recv q)
        else select_fwd send msg recv (clean_path (fst s) q)
  in*)
   let fwd_macs : (edge * (role_name * message * role_name)) list= 
    List.flatten ( 
      List.map 
        (function ((m,a),lmacs) -> List.flatten (
           List.map 
             (function (recv,path) -> select_fwd (fst a) m recv path)
             lmacs))
        fut)
  in
  let edges = remove_duplicates (List.map fst fwd_macs) in
  let fwd_macs = List.map (function e -> (e,remove_duplicates (subset_assoc e fwd_macs))) edges in
  let () = debug ("MACs to fwd:"^(print_fwdm fwd_macs)) in
  futur,fwd_macs


(* Key propagation *)

let compute_fwdk (g:stategraph) (futur:future) (rl:role_list):
    (edge * (role_name * role_name) list) list =

  let lr = List.map (function (r,_,_) -> r) rl in
  let sendg = List.map (function ((a,b),m,a1) -> (a,((a,b),m,a1))) g in
  let first_role = List.hd lr in
  let rec from_edge path paths = function
    | [] -> []
    | ((_,m,(_,(recver,recv_seq))) as e)::q -> 
        (if List.mem e path
         then []
         else if List.mem_assoc recver paths
         then 
           (let l,_ = List.assoc recver paths in
            (List.map (function (r,(l,p)) -> ((r,recver),e::p))
               (List.filter (function (r,(l,p)) -> not (List.mem recver l)) paths))
            @(let paths = List.map (function (r,(l,p)) -> r,(recver::l,e::p)) (List.remove_assoc recver paths) in
              let paths = (recver,(l,[]))::paths in  
              from_edge (e::path) paths (subset_assoc (recver,recv_seq) sendg)))

(*           (let l,_ = List.assoc recver paths in
           let paths = List.map (function (r,(l,p)) -> r,(recver::l,e::p)) (List.remove_assoc recver paths) in
           let paths = (recver,(l,[]))::paths in 
            from_edge (e::path) paths (subset_assoc (recver,recv_seq) sendg))*)
         else
           ((List.map (function (r,(_,p)) -> ((r,recver),e::p)) paths)
           @(let paths = List.map (function (r,(l,p)) -> r,(recver::l,e::p)) paths in
             let paths = (recver,([recver],[]))::paths in             
             from_edge (e::path) paths (subset_assoc (recver,recv_seq) sendg)))
        )@
          (from_edge path paths q)
  in
  let fwd_keys = 
    from_edge [] [first_role,([],[])] (subset_assoc (first_role,[]) sendg) in

  let fwd_keys = List.filter (function (r,rr),_ -> r <> rr) fwd_keys in

  let rec clean_path role (p:stategraph) = 
    match p with
        [] -> None
      | (((ro,st),ss),m,(r,rr))::q -> 
          if ro = role then 
            match clean_path role q with
                None -> Some q
              | Some q -> Some q
          else clean_path role q
  in
  let rec select_fwd (send:role_name) (recv:role_name) = function
      [] -> []
    | (((s,ss),m,(r,rr)) as e)::q -> 
(*        if List.mem recv (List.assoc (m,s) futur)
        then begin*)
          match clean_path (fst s) q with
            | None -> (e,(send,recv))::(select_fwd send recv q)
            | Some q -> (e,(send,recv))::(select_fwd send recv q)
(*        end
        else assert false*)
          (*select_fwd send msg recv (clean_path (fst s) q)*)
  in
(*  let rec clean_path role (p:stategraph) = 
    match p with
        [] -> assert false
      | ((s,ss),m,((r,st),rr))::q -> if r = role then q else clean_path role q
  in
  let rec select_fwd x role = function
      [] -> []
    | (((s,ss),m,(r,rr)) as e)::q -> 
        if List.mem role (List.assoc (m,s) futur)
        then (e,(x,role))::(select_fwd x role q)
        else select_fwd x role (clean_path (fst s) q)
  in *)
  let fwd_keys : (edge * (string * string)) list = 
    List.flatten (
      List.map 
        (function ((role,role'),path) -> select_fwd role role' path)
        fwd_keys)
  in
  let edges = remove_duplicates (List.map fst fwd_keys) in
  let fwd_keys = List.map (function e -> (e,remove_duplicates (subset_assoc e fwd_keys))) edges in
  let () = debug ("Key fwding:"^(print_fwdk fwd_keys)) in
  fwd_keys



(*
 
  let rec map_proc past assoc = function
  | End _ -> assoc
  | Goto _ -> assoc
  | Send (n,l) -> (n,past)::(List.flatten (List.map (function (m,p) -> map_proc )
  | Receive (n,l) -> 
*)





