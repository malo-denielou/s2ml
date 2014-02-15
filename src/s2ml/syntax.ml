(********************************************************************)
(* Session - Implementation                                         *)
(*                                                                  *)
(* syntax.ml                                                        *)
(********************************************************************)
(* $Id: syntax.ml 9310 2009-01-27 15:07:25Z denielou $ *)

(*#use "c:/Documents and Settings/t-ricarc/inria-msr-sec/lang-sec/session-types/src/common.ml";;*)
(*#use "/Users/ricardocorin/inria-msr-sec/lang-sec/session-types/src/common.ml";;*)

open Common

(************************************)
(* Abstract syntax for source files *)
(************************************)

type principal = string

type vars = string list

type message = 
    {label : string ;
     payload : string ;
    }

type as_role_type = 
  | ASEnd
  | ASGoto of info * string
  | ASMu of info * string * as_role_type
  | ASSend of info * ((message * as_role_type) list)
  | ASReceive of info * ((message * as_role_type) list)

type as_session =
    (string * string * as_role_type) list
      (* role name, type and description *)  

type varlist = 
    (string * string) list (* var name and type name *)

type trustlist =
    (string * string) list (* role name and role name *)

type ast = string * as_session

type node = int (* index *)


let node_ident = 
  Hashtbl.create 10
    
let clear_ident () = 
  Hashtbl.clear node_ident

    
let find_node (i:string) =
  Hashtbl.find node_ident i
    
let remove_ident (i:string) =
  Hashtbl.remove node_ident i

let nref = ref 0

let create_node_name ()=
  let i = !nref in 
  let _ = incr nref in
  i

let add_ident (i:string) =
  if Hashtbl.mem node_ident i
  then Hashtbl.find node_ident i
  else 
    let n = create_node_name () in
    let () = Hashtbl.add node_ident i n in
    n


let rec print_as_role  = function
  | ASEnd -> "End "
  | ASGoto (_,g) -> ("Goto "^g)
  | ASMu (_,g,r) -> ("Mu "^g^":")^(print_as_role r)
  | ASSend (_,l) -> "Send ("^(String.concat "+" (List.map (function (_,r) -> print_as_role r) l))^")"
  | ASReceive (_,l) -> "Receive ["^(String.concat "|" (List.map (function (_,r) -> print_as_role r) l))^"]"

    (*
let rec role_to_string  = function
  | ASEnd -> "End"
  | ASGoto _ -> "Goto"
  | ASMu (_,_,r) -> "Mu "^(role_to_string r)
  | ASSend (_,l) -> "Send"^()
  | ASReceive _ -> "Receive"
    *)



(*********************************************)
(* Graph-like representation of the protocol *)
(*********************************************)

type role_name = string

type role_type =
    | End of node
    | Goto of node
    | Send of node * ((message * role_type) list)
    | Receive of node * ((message * role_type) list)

type role_list = (role_name * string * role_type) list

type session = role_list 


(* Conversion function from AS to role_type *)
let rec conversion (next:node option) = function
  | ASEnd -> 
      let n = create_node_name () in End n
  | ASGoto (_,i) -> Goto (add_ident i)
  | ASMu (_,i,r) -> 
      let n = add_ident i in
      conversion (Some n) r
  | ASSend (_,l) -> 
      let n = match next with None -> create_node_name () | Some k -> k in
      Send (n, List.map (function (m,r) -> (m,conversion None r)) l)
  | ASReceive (_,l) -> 
      let n = match next with None -> create_node_name () | Some k -> k in
      Receive (n, List.map (function (m,r) -> (m,conversion None r)) l)

let to_role_type : as_session -> role_list =
  List.map 
    (function (x,y,z) -> 
       let () = clear_ident () in
      (* let _ = nref:=0 in *)
       (x,y,conversion None z))



(* Get the starting node of a role_type *)
let node_of : role_type -> node = function
  | End n -> n
  | Goto n -> n
  | Send (n,_) -> n
  | Receive (n,_) -> n


let rec sprint_role = function
  | End n -> Printf.sprintf "End %d " n
  | Goto n -> Printf.sprintf "Goto %d " n
  | Send (n,l) -> (Printf.sprintf "%d:Send{%s" n (sprint_list_messages l))
  | Receive (n,l) -> (Printf.sprintf "%d:Receive{%s" n (sprint_list_messages l))
and sprint_list_messages = function
    [] -> ""
  | (m,r)::q -> (Printf.sprintf "|%s:%s.%s%s" 
                    m.label 
                    m.payload
                    (sprint_role r) 
                    (sprint_list_messages q))

let rec sprint_session : role_list -> string = function
    [] -> ""
  | (s,u,r)::q -> 
      begin
        s^" : "^u^"\n"^
        (sprint_role r)^"\n"^
        (sprint_session q)
      end

