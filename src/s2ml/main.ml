(********************************************************************)
(* Session - Implementation                                         *)
(*                                                                  *)
(* main.ml                                                          *)
(********************************************************************)
(* Time-stamp: <2008-05-18 14:23:57 t-pierd> *)

open Common
open Syntax

let debug = gen_debug debug_main "main"

(* Main function *)
let compile_session session =
  let () = if Array.length (Sys.argv) > 1 then
    match Sys.argv.(1) with
        "debug-all" -> ()
      | _ -> failwith "unknown option"
  in
  debug "Compilation starting" ;
  let lexbuf = Lexing.from_string session in
  debug "Lexer built" ;
  let (session_name,desc) =
    (try 
        Parser.sess Lexer.token lexbuf
     with
         Common.Syntax_error (s,i) ->
	   (debug ("Syntax error: "^s^" "^(Common.info_to_string i));
            exit 1)
       | Common.Parse_error (s,i)  ->
	   (debug ("Parsing error: "^s^" "^(Common.info_to_string i)); 
            exit 2)
    ) in
  if Common.debug_parserlexer then
    List.iter 
      (fun (r,_,x) -> 
         debug (r^": "^(print_as_role x)))
      desc ;
  debug "Parsing succeded";

  debug "Conversion to internal representation";
  let rl = to_role_type desc in
  let s = rl in
  if Common.debug_parserlexer then debug (sprint_session rl) ;
  
  debug "Conversion to the graph representation";
  let g,sg,node_assoc = Graph.graph_from_session s in
  let role_assoc = Graph.assoc_from_session rl in
  
  debug (Printf.sprintf "Generation of `%s.dot'" session_name);
  let () = Dotgen.gen_dot (session_name^".dot") g role_assoc in

  debug "Checking properties";
  let () = Properties.check_all role_assoc g in 
  
  debug "State graph computation";
  let stategraph = Graph.compute_stategraph g role_assoc rl in
  let state_to_node = Graph.compute_statenodeassoc stategraph rl in

  debug (Printf.sprintf "Creating dot files `%s.dot' `%s-ext.dot' `%s-full.dot'"
           session_name session_name session_name);
  let () = Dotgen.gen_extgraph (session_name^"-ext.dot") stategraph in
  let () = Dotgen.gen_fullgraph (session_name^"-full.dot") stategraph in
  
  debug "Visibility computation";
  let visib, antivisib = Graph.compute_visib stategraph rl in
  let fut,fwd_macs = Graph.compute_future stategraph antivisib in
  let fwd_keys = Graph.compute_fwdk stategraph fut rl in
  let fwd = (fwd_macs,fwd_keys) in
  (* let visibility,loc,extg = Graph.compute_visibility g sg role_assoc rl in *)

  debug "Generation of the code";
  let m = Generation.generate_code s g visib antivisib fut fwd state_to_node stategraph in
  debug (Printf.sprintf "Creating module `%s_protocol.ml'" session_name);
  let f = open_out (session_name^"_protocol.ml") in
  let _ = output_string f m  in
  let _ = close_out f in
  let prox = Generation.generate_proxy session_name g s visib fut state_to_node stategraph in
  debug (Printf.sprintf "Creating module `%s.ml'" session_name);
  let f = open_out (session_name^".ml") in
  let _ = output_string f prox  in
  let _ = close_out f in

  debug "Generation of the interface";
  let gi = Geninterface.generate_protocol s g visib fut stategraph in
  let fi = open_out (session_name^"_protocol.mli") in
  debug (Printf.sprintf "Created interface `%s_protocol.mli'" session_name);
  let _ = output_string fi gi in
  let _ = close_out fi in
  let lgi = Geninterface.generate session_name s in
  let fi = open_out (session_name^".mli") in
  debug (Printf.sprintf "Created interface `%s.mli'" session_name);
  List.iter (output_string fi) lgi ;
  let _ = close_out fi in

(*  debug "Generation of the Seven interface";
  let lgs = Genseven.generate_protocol s visib fut state_to_node g stategraph in
  let fs = open_out (session_name^"_protocol.ml7") in
  debug (Printf.sprintf "Created seven interface `%s.ml7'" session_name);
  List.iter (output_string fs) lgs ;
  let _ = close_out fs in
  let lgs = Genseven.generate session_name s state_to_node stategraph in
  let fs = open_out (session_name^".ml7") in
  debug (Printf.sprintf "Created seven interface `%s.ml7'" session_name);
  List.iter (output_string fs) lgs ;
  let _ = close_out fs in
*)
 
  debug "Compilation ending"
    


let rec parse_until_end accum = 
   try 
     let s = input_char stdin in
     (parse_until_end  (accum^(String.make 1 s)))
   with End_of_file -> accum

    
let _ = 
  try 
    let session = parse_until_end "" in
    debug ("Session read: \n"^session) ;
    compile_session session
  with 
      End_of_file -> exit 0 
    
