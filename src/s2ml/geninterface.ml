(********************************************************************)
(* Session - Implementation                                         *)
(*                                                                  *)
(* geninterface.ml                                                    *)
(********************************************************************)
(* $Id: geninterface.ml 9310 2009-01-27 15:07:25Z denielou $ *)


open Common
open Syntax 
open Graph

(********************************************)
(*         Global Helper Functions          *)
(********************************************)

let first_send r = 
  match r with
    | Send (_,_) -> true
    | _ -> false 


let rec vars_to_type vl = function
    [] -> "unit"
  | [a] -> (List.assoc a vl)
  | a::q -> (List.assoc a vl)^" * "^(vars_to_type vl q)
 

(********************************************)
(*            Interface Generation          *)
(********************************************)

(* Principal singleton type name *)
let prin_type s = "var_"^s
let prin_cons s = capitalize s

(* Variable singleton type name *)
let var_type s = "var_"^s
let var_cons s = capitalize s

(* types and labels *)
let gen_result_type (role:string) = Printf.sprintf "result_%s" role
let gen_typename (n:node) = Printf.sprintf "msg%d" n
let gen_handler_label (cons:string) = Printf.sprintf "h%s" cons


let rec create_var_prin_i : role_list -> string = function
    [] -> ""
  | (r,_,_)::q -> 
      (Printf.sprintf "type %s = %s of principal\n" 
        (prin_type r)
        (prin_cons r)
      )^create_var_prin_i q

let rec create_var_i : varlist -> string = function
    [] -> ""
  | (n,t)::q -> 
      (Printf.sprintf "type %s = %s of %s\n" 
        (var_type n)
        (var_cons n)
        t
      )^create_var_i q



let gen_prins (rl:role_list) (fut:future) = 
  let macs = remove_duplicates 
    (List.flatten 
       (List.map 
          (function ((m,a),l) -> 
             List.map 
               (function r -> "mac_"^r^m.label) l) fut))  in
  let macs = String.concat ";\n  " (List.map (function s -> s^" : bytes ") macs)
  in 
  let roles = List.map (function (r,_,_) -> r) rl in
  let prins = String.concat ";\n  " 
    (List.map (function s -> "prins_"^s^" : principal ") roles) in 
  let pairs = List.filter (function (x,y) -> x<>y) (product roles roles) in
  let keys = String.concat ";\n  " 
    (List.map (function (x,y)-> "key_"^x^y^" : bytes ") pairs) in
  "\ntype prins = {\n  "^prins^"}\n"
  ^"type macs = {\n  "^macs^"}\n"
  ^"type keys = {\n  "^keys^"}\n"
  ^"type header = {\n"
  ^"  ts : int ;\n"
  ^"  sid : bytes }\n"
  ^"type store = \n"
  ^"  { prins : prins ; macs : macs ; keys : keys ; header :  header}\n"


(********************************************)
(*             Proof Support                *)
(********************************************)


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
  let predsdecl = "\ntype preds = \n" in
  predsdecl
  ^(String.concat "" (lsend @ lrecv))

(* Send and Recv wired interface *)

let rec generate_wired_type visib : state list -> string list= function 
    [] -> []
  | state::q -> 
      let gen_type sv = 
        String.concat "\n" 
          (List.map (function v ->
                       let (_,seq) = List.hd v in
                       let (_,m) = List.hd seq in
                       Printf.sprintf "  | Wired_in_%s_of_%s of (%s * store)"
                         (print_state state)
                         (print_vi v)
                         m.payload
                    )
             sv)
      in
      let lv = remove_duplicates (subset_assoc state visib) in
      (Printf.sprintf "\ntype wired_%s ="
         (print_state state))::
        (String.concat "\n" (List.map gen_type lv))
      ::(generate_wired_type visib q)
 

let genrecvwired (visib:visib) =
  let states = remove_duplicates (List.map fst visib) in
  let from_state = function state ->
    Printf.sprintf 
      "\nval receiveWired_%s: store -> wired_%s" (print_state state) (print_state state)
  in
  remove_duplicates (List.map from_state states)
  

let gensendwired (sg:stategraph) =
  let from_edge = function ((state,next),m,_) ->
    Printf.sprintf 
      "\n val sendWired_%s_%s: %s -> store -> store" 
      m.label
      (print_state state)
      m.payload
  in
  remove_duplicates (List.map from_edge sg)


(*------------------------------*)
(* First Lines of the interface *)
(*------------------------------*)

let candy_title =
"(*******************************)\n"
  ^"(* Generated Session Interface *)\n"
  ^"(*******************************)"

let global_def_i rl =
  candy_title^"\nopen Data\n\n"
  ^"type principal=string\n\n"

let gen_user_inputs_i (l:role_type list) : string =
  match l with
      [] -> assert false
    | (Send (n,_ ))::_-> Printf.sprintf " %s ->" ((gen_typename n))
    | (Receive (n,_))::_-> Printf.sprintf " %s ->" ((gen_typename n))
    | _ -> assert false

let crypto_first_line_i = ": prins ->"
let crypto_first_line_recv_i = ": principal ->"
let first_line_i (l:role_type list) = crypto_first_line_i^(gen_user_inputs_i l)
let first_line_recv_i (l:role_type list) = crypto_first_line_recv_i^gen_user_inputs_i l
  


(*------------*)
(* Flow types *)
(*------------*)

(* sending types *)
let gen_constructor_send (m:message) : string = 
  Printf.sprintf " %s of (%s * " m.label m.payload

let rec gen_sumtype_send (role:string) : (message * role_type) list -> string = function 
    [] -> ""
  | [m,Goto n] -> 
      (gen_constructor_send m)^(gen_typename n)^")\n"
  | [m,End _] -> 
      (gen_constructor_send m)^(gen_result_type role)^")\n"
  | [_,_] -> assert false
  | (m,Goto n)::q -> 
      (gen_constructor_send m)^(gen_typename n)^")\n |"^(gen_sumtype_send role q)
  | (m,End _)::q -> 
      (gen_constructor_send m)^(gen_result_type role)^")\n |"^(gen_sumtype_send role q)
  | _ -> assert false

(* receiving types *)
let gen_constructor_recv (m:message) : string = 
  Printf.sprintf "   %s : (prins * %s -> " (gen_handler_label m.label) m.payload

let rec gen_sumtype_recv (role:string) : (message * role_type) list -> string = function 
    [] -> ""
  | [m,Goto n] -> 
      (gen_constructor_recv m)^(gen_typename n)^")}\n"
  | [m,End _] -> 
      (gen_constructor_recv m)^(gen_result_type role)^")}\n"
  | [_,_] -> assert false
  | (m,Goto n)::q -> 
      (gen_constructor_recv m)^(gen_typename n)^") ;\n"^(gen_sumtype_recv role q)
  | (m,End _)::q -> 
      (gen_constructor_recv m)^(gen_result_type role)^") ;\n"^(gen_sumtype_recv role q)
  | _ -> assert false

let gen_decl (role:string) : role_type -> string = function
    End _ -> assert false
  | Goto _ -> assert false
  | Send (n,l) -> (gen_typename n)^" = \n  "^(gen_sumtype_send role l )
  | Receive (n,l) -> (gen_typename n)^" = {\n"^(gen_sumtype_recv role l )

 (*     let n = alias na n in *)

let rec gen_types (param:string) (role:string) : role_type list -> string = function
    [] -> ""
  | [r] -> "\n"^param^"type "^(gen_decl role r)
  | r::q -> (gen_types param role q)^"and "^(gen_decl role r)


let generate_functions_i (role:string) (r : role_type list) (original_role:role_type)=
let initiator = (first_send original_role) in
  Printf.sprintf "val %s %s result_%s\n"
    role
    (if initiator then
	(debug (Printf.sprintf "%s is initiator" role) ; first_line_i r)
      else 
	(debug (Printf.sprintf "%s is not initiator" role); first_line_recv_i r))
    role


(*-----------------*)
(* Main generation *)
(*-----------------*)


(* Generate the proxy interface for each role *)
let generate_from_role_i (param:string) (role_name:string) (typ:string) (role:role_type) (original_role:role_type)= 
  let flatten_session = flatten role in
  (Printf.sprintf "\n(* Function for role %s *)\n" role_name)
  ^(Printf.sprintf "\n%stype %s = %s\n" param (gen_result_type role_name) typ)
  ^(gen_types param role_name (List.rev flatten_session))^"\n"
  ^(generate_functions_i role_name flatten_session role)
      
(* Generate the proxy interfaces *)
let rec generate_from_session_i (param:string) (s:role_list) = 
  match s with
      [] -> []
    | (str,typ,rol)::q -> 
        debug (Printf.sprintf "Generating interface of role %s " str);
        (generate_from_role_i param str typ rol rol)
        ::(generate_from_session_i param q )

(* Generates the full interface *)
let generate_protocol (s:session) (g:graph) (visib:visib) (fut:future) (sg:stategraph): string =
  let rl = s in
  let states = remove_duplicates (List.map fst visib) in
  let roles = List.map (function (r,_,_) -> r) rl in
  (global_def_i rl)
  ^(gen_prins rl fut)
  ^(String.concat "" 
      (List.map (fun (r,_,rt) -> 
                   if first_send rt then
                     "val empty_store_"^r^" : prins -> store\n"
                   else "val empty_store_"^r^" : principal -> store\n"
                ) rl))
  ^(genEvents g sg)^"\n\n"
  ^(String.concat "\n" (generate_wired_type visib states))
  ^"\n\n"
  ^(String.concat "\n" (genrecvwired visib))
  ^(String.concat "\n" (gensendwired sg))

(* Generates the full interface *)
let generate (session_name:string) (s:session)=
  let rl = s in
  ("open "^session_name^"_protocol\n")
  ::(generate_from_session_i "" rl)

