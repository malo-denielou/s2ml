(********************************************************************)
(* Session - Implementation                                         *)
(*                                                                  *)
(* dotgen.ml                                                        *)
(********************************************************************)


(* Given a graph and a session, we want to output a corresponding dot file *)

open Common
open Syntax
open Graph
open Printf

let debug = gen_debug debug_bool "dotgen"
  
let gen_dot (filename:string) (g:graph) (ra:node_to_role) =
  let n = ref (-1) in
  (*let g = match g with
      a::r -> r@[a]
    | _ -> assert false in*)
  let printed = ref [] in
  let name =
    let mem = ref [] in
    function i ->
      if List.mem_assoc i !mem 
      then List.assoc i !mem
      else 
        let () = incr n in
        let () = mem := (i,!n)::!mem in
        !n
  in
  (*let ra = List.map (function (n,r) -> (n,String.capitalize (String.make 1 r.[0]))) ra in*)
  let f = open_out filename in
  let taille = if List.length g > 15 then "rankdir=TB; fontsize=20;" else "rankdir=LR; fontsize=20;" in
  output_string f ("digraph G {\n  "^taille^" margin = 0;\n\n") ;
  List.iter (function (i,m,j) ->
               let ii = name i in
               let jj = name j in
               output_string f 
                 (sprintf "  %i -> %i [label=\"%s\"];\n" ii jj m.label);
               
               if List.mem jj !printed then () 
               else begin printed := jj::!printed ;
                 output_string f (sprintf "  %i [label=\"%s\"];\n" jj
                                    (List.assoc j ra)) end;
               if List.mem ii !printed then () 
               else begin
                 printed := ii::!printed ;
                 if i = 0 then
                   output_string f (sprintf "  %i [label=\"%s\",peripheries=2];\n  {rank = min; %d;}\n" ii (List.assoc i ra) ii)
                 else 
                   output_string f (sprintf "  %i [label=\"%s\"];\n" ii
                                      (List.assoc i ra))
               end
                 (*
                   output_string f (sprintf "  %i -> %i [label=\"{%s}%s{%s}\"];\n" i j (String.concat "," m.write) m.label (String.concat "," m.read));
                   
                   output_string f (sprintf "  %i [label=\"%s\"];\n" j (List.assoc j ra)) ;
                   if i = 0 then
                   output_string f (sprintf "  %i [label=\"%s\",peripheries=2];\n  {rank = min; 0;}\n" i (List.assoc i ra))
                   else 
                   output_string f (sprintf "  %i [label=\"%s\"];\n" i (List.assoc i ra)) 
                 *)
            ) g ;
  output_string f ("}\n") ;
  close_out f




let gen_fullgraph (filename:string) (sg:stategraph) =
  let n = ref (-1) in
  let rec name acc = function
      [] -> acc
    | ((a,b),m,(aa,bb))::q ->
        let acc = if (List.mem_assoc a acc) then acc else ((a),(incr n;!n))::acc in
        let acc = if (List.mem_assoc (bb) acc) then acc else ((bb),(incr n;!n))::acc in
        name acc q in
  let num = name [] sg in
  let printed = ref [] in
  let f = open_out filename in
  (*  output_string f ("digraph G {\n  \nsize=\"40,40\";  fontsize=20;\n\n") ; *)
  output_string f ("digraph G {\n  \n  rankdir=LR; fontsize=20; margin = 0;\n\n") ;
  List.iter 
    (function ((a,b),m,(aa,bb)) ->
       (*let () = debug (Printf.sprintf "Generation of message %s " m.label) in*)
       output_string f (sprintf "  %i -> %i [label=\"%s\"];\n"
                          (List.assoc (a) num) (List.assoc (bb) num) m.label);
    
       if List.mem bb !printed then () 
       else begin printed := bb::!printed ;
         output_string f (sprintf "  %i [label=\"%s\"];\n" (List.assoc (bb) num)
                          (print_state_display bb)) end;
       
       if List.mem a !printed then () 
       else begin printed := a::!printed ;
         if snd a = [] then
           output_string f 
             (sprintf "  %i [label=\"%s\",peripheries=2];\n  {rank = min; %i;}\n" 
                (List.assoc (a) num) (print_state_display a)  (List.assoc (a) num))
         else 
           output_string f (sprintf "  %i [label=\"%s\"];\n"  (List.assoc (a) num)
                              (print_state_display a))
       end)
    sg ;
  output_string f ("}\n") ;
  close_out f

let gen_extgraph (filename:string) (sg:stategraph) =
  let n = ref (-1) in
  let rec name acc = function
      [] -> acc
    | ((a,b),m,(aa,bb))::q ->
        let acc = if (List.mem_assoc (a) acc) then acc else ((a),(incr n;!n))::acc in
        let acc = if (List.mem_assoc (b) acc) then acc else ((b),(incr n;!n))::acc in
        let acc = if (List.mem_assoc (aa) acc) then acc else ((aa),(incr n;!n))::acc in
        let acc = if (List.mem_assoc (bb) acc) then acc else ((bb),(incr n;!n))::acc in
        name acc q in
  let num = name [] sg in
  let f = open_out filename in
  output_string f ("digraph G {\n  rankdir=TB; fontsize=20; margin = 0; ranksep=0.2; nodesep = 0.2;\n\n") ;
  List.iter 
    (function ((a,b),m,(aa,bb)) ->
       (*let () = debug (Printf.sprintf "Generation of message %s " m.label) in*)
       output_string f (sprintf "  %i -> %i [label=\"%s\"];\n"
                          (List.assoc (a) num) (List.assoc (b) num) m.label);
       output_string f (sprintf "  %i -> %i [label=\"%s\"];\n"
                          (List.assoc (aa) num) (List.assoc (bb) num) m.label);
       output_string f (sprintf "  %i [label=\"%s\"];\n" (List.assoc (b) num)
                          (print_state_display b)) ;
       output_string f (sprintf "  %i [label=\"%s\",peripheries=2];\n" (List.assoc (bb) num)
                          (print_state_display bb)) ;
       if snd a = [] then
         output_string f (sprintf "  %i [label=\"%s\",peripheries=2];\n  {rank = min; %d;}\n"
                            (List.assoc (a) num) (print_state_display a)  (List.assoc (a) num) )
       else 
         output_string f (sprintf "  %i [label=\"%s\",peripheries=2];\n"  (List.assoc (a) num) (print_state_display a));
       if snd aa = [] then
         output_string f (sprintf "  %i [label=\"%s\"];\n  {rank = min; %d;}\n"
                            (List.assoc (aa) num) (print_state_display aa)  (List.assoc (aa) num))
       else  
         output_string f (sprintf "  %i [label=\"%s\"];\n"  (List.assoc (aa) num) (print_state_display aa)))
    sg ; 
  output_string f ("}\n") ;
  close_out f
