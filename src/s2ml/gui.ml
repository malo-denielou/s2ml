(* Backing pixmap for drawing area *)
let backing = ref (GDraw.pixmap ~width:800 ~height:600 ())

let debug_gui = true
let debug = Common.gen_debug debug_gui "gui"


(* Types *)
type node = {mutable role:string; mutable posx:int; mutable posy:int}
type edge = {mutable source:int; mutable dest:int; mutable label:string}

type state = 
    Idle
  | AddNode
  | RemoveNode
  | SelectNode of int
  | SelectEdge of int
  | AddEdge of int option


(* Values *)
let nodes = ref [
  (0,{role="";posx=1;posy=1});
  (1,{role="";posx=3;posy=2});
  (2,{role="";posx=2;posy=3});
]
let edges : (int*edge) list ref = ref [
  (0,{source=0;dest=1;label="Test0"});
  (1,{source=0;dest=2;label="Test1"});
  (2,{source=2;dest=1;label="Test2"});
(*  (3,{source=1;dest=0;label="Te3"});
  (4,{source=1;dest=2;label="Te4"});*)
]
let roles : (int*string) list ref = ref [(*0,"c"*)]
let session = ref "S"
let state : state ref = ref Idle




let freshnode () =
  let c = ref 0 in
  while (List.mem_assoc !c !nodes) do
    incr c
  done;
  !c

let freshedge () =
  let c = ref 0 in
  while (List.mem_assoc !c !edges) do
    incr c
  done;
  !c

let rec get_by_pos (x,y) = function
    [] -> None
  | (id,r)::q -> if r.posx = x && r.posy = y 
    then Some id 
    else get_by_pos (x,y) q


let main () =
  let width = 800 in
  let height = 600 in
  let dim = 80 in

  let window = GWindow.window ~width:800 ~height:600 ~title:"Session Editor" () in
  let _ = window#connect#destroy ~callback:GMain.Main.quit in

  let outvbox = GPack.vbox ~packing:window#add () in
  let buttonboxtop = GPack.button_box `HORIZONTAL ~layout:`START ~packing:(outvbox#pack ~expand:false) () in
  let outhbox = GPack.paned `HORIZONTAL ~packing:outvbox#add () in
  (*GPack.hbox ~packing:outvbox#add () in*)
  let invbox = GPack.vbox ~packing:outhbox#add1 () in
  let rightbox = GPack.vbox ~width:100 ~packing:outhbox#add2 () in
  let () = outhbox#set_position 600 in
  let scrolledw = 
    GBin.scrolled_window ~border_width:5
      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:invbox#add () in

  let area = GMisc.drawing_area ~width ~height ~packing:scrolledw#add_with_viewport () in
  let buttonboxbot = 
    GPack.button_box `HORIZONTAL ~layout:`START ~packing:(invbox#pack ~expand:false) () in

  let bnew  = GButton.button ~stock:`NEW ~packing:buttonboxtop#add () in
  let bopen = GButton.button ~stock:`OPEN ~packing:buttonboxtop#add () in
  let bsave = GButton.button ~stock:`SAVE ~packing:buttonboxtop#add () in
  let bquit = GButton.button ~stock:`QUIT ~packing:buttonboxtop#add () in
  let _ = bquit#connect#clicked ~callback:GMain.Main.quit in
  let badd = GButton.button ~stock:`ADD ~packing:buttonboxbot#add () in
  let brem = GButton.button ~stock:`REMOVE ~packing:buttonboxbot#add () in
  let bedge = GButton.button ~stock:`ADD ~packing:buttonboxbot#add () in

  let aspect = GBin.aspect_frame ~width:200 ~label:"Nodes" ~show:false
    ~packing:(rightbox#pack ~expand:false) () in 
  let aspectvb = GPack.vbox ~width:200 ~packing:aspect#add () in
  let nodelabel = GMisc.label ~text:"Nodes" ~width:200 ~packing:aspectvb#add () in
  let aspecthb = GPack.hbox ~packing:aspectvb#add () in
  let rolelabel = GMisc.label ~text:"Role: " ~packing:aspecthb#add () in
  let roleentry = GEdit.entry ~width:50 ~packing:aspecthb#add () in
  let rolebutton = GButton.button ~label:"Save" ~packing:aspecthb#add () in
  

  let aspect2 = GBin.aspect_frame ~width:200 ~label:"Edges" ~show:false
    ~packing:(rightbox#pack ~expand:false) () in 
  let aspectvb2 = GPack.vbox ~width:200 ~packing:aspect2#add () in
  let edgelabel = GMisc.label ~text:"Edges" ~width:200 ~packing:aspectvb2#add () in
  let aspecthb2 = GPack.hbox ~packing:aspectvb2#add () in
  let edgeelabel = GMisc.label ~text:"Label: " ~packing:aspecthb2#add () in
  let edgeentry = GEdit.entry ~width:50 ~packing:aspecthb2#add () in
  let edgebutton = GButton.button ~label:"Save" ~packing:aspecthb2#add () in
  


  let all_rect = Gdk.Rectangle.create ~x:0 ~y:0 ~width:width ~height:height in


  (* Drawing *)
  let draw_node id = 
    let pixmap = !backing in
    let width = 30 in
    let height = 30 in
    let w = 50 in
    let h = 50 in
    let node = List.assoc id !nodes in
    let (x,y)= (node.posx*dim+15,node.posy*dim+15) in
    let update_rect = Gdk.Rectangle.create ~x ~y ~width:(width+1) ~height:(height+1) in
    pixmap#set_foreground `WHITE;
    (*pixmap#rectangle ~x ~y ~width ~height ~filled:false ();*)
    pixmap#arc  ~x ~y ~width:w ~height:h ~filled:true ();
    pixmap#set_foreground `BLACK;
    (*pixmap#rectangle ~x ~y ~width ~height ~filled:false ();*)
    pixmap#arc  ~x ~y ~width:w ~height:h ~filled:false ();
    let font = Gdk.Font.load "-Adobe-Helvetica-medium-R-Normal--*-80-*-*-*-*-*-*" in
    !backing#string (string_of_int id) ~font:font ~x:(x) ~y:(y);
    let font = Gdk.Font.load "-Adobe-Helvetica-medium-R-Normal--*-120-*-*-*-*-*-*" in
    !backing#string (node.role) ~font:font ~x:(x+5) ~y:(y+30);
    (*area#misc#draw (Some update_rect)*)
  in

  let draw_edge id =
    let pixmap = !backing in
    let w = 50 in
    let h = 50 in
    let edge = List.assoc id !edges in
    let nsource = List.assoc edge.source !nodes in
    let ndest = List.assoc edge.dest !nodes in
    let (xs,ys)= (nsource.posx*dim+15,nsource.posy*dim+15) in
    let (xd,yd)= (ndest.posx*dim+15,ndest.posy*dim+15) in
    pixmap#set_foreground `BLACK;
    let () = match (xs<xd,ys<yd) with
      | true,true -> pixmap#polygon ~filled:true 
          [(xs+25,ys+25);(xs+30,ys+20);(xd+30,yd+20)]
      | true,false -> pixmap#polygon ~filled:true 
          [(xs+30,ys+30);(xs+25,ys+25);(xd+30,yd+30)]
      | false,true -> pixmap#polygon ~filled:true 
          [(xs+25,ys+25);(xs+20,ys+20);(xd+20,yd+20)]
      | false,false -> pixmap#polygon ~filled:true 
          [(xs+20,ys+30);(xs+25,ys+25);(xd+20,yd+30)]
    in
(*    let (xs,xd)= if xs>xd then (xs-25,xd+25) else (xs+25,xd-25) in
    let (ys,yd)= if ys>yd then (ys-25,yd+25) else (ys+25,yd-25) in*)
(*    pixmap#line ~x:(xs+25) ~y:(ys+25) ~x:(xd+25) ~y:(yd+25) ;*)
(*    let font = Gdk.Font.load "-Adobe-Helvetica-medium-R-Normal--*-80-*-*-*-*-*-*" in
    !backing#string (string_of_int id) ~font:font ~x:(xd) ~y:(yd);*)
    let font = Gdk.Font.load "-Adobe-Helvetica-medium-R-Normal--*-100-*-*-*-*-*-*" in
    !backing#string (edge.label) ~font:font ~x:((xs+xd)/2) ~y:((ys+yd)/2);
  in
    

  let draw_graph () =
    let pixmap = !backing in
    pixmap#set_foreground `WHITE;
    pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    List.iter (fun (id,_) -> draw_edge id) !edges; 
    List.iter (fun (id,_) -> draw_node id) !nodes; 
  in

  let up () = area#misc#draw (Some all_rect) 
  in

  (* Init *)
  (* Redraw the screen from the data *)
  let redraw () =
    debug "Redrawing all the graph." ;
    let () = draw_graph () in
    let () = up () in
    ()
  in

  

  (* Create a new backing pixmap of the appropriate size *)
  let configure window backing ev =
    debug "Configure event." ;
    let width = GdkEvent.Configure.width ev in
    let height = GdkEvent.Configure.height ev in
    let pixmap = GDraw.pixmap ~width ~height ~window () in
    pixmap#set_foreground `WHITE;
    pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    backing := pixmap; 
    let () = draw_graph () in
 (*   List.iter (fun (id,_) -> draw_node id) !nodes; *)
    area#misc#draw None;
    (*    redraw ();*)
    true 
  in

  (* Redraw the screen from the backing pixmap *)
  let expose (drawing_area:GMisc.drawing_area) (backing:GDraw.pixmap ref) ev =
    (* debug "Expose event."; *)
    let area = GdkEvent.Expose.area ev in
    let x = Gdk.Rectangle.x area in
    let y = Gdk.Rectangle.y area in
    let rwidth = Gdk.Rectangle.width area in
    let rheight = Gdk.Rectangle.width area in
    let drawing =
      drawing_area#misc#realize ();
      new GDraw.drawable (drawing_area#misc#window)
    in
    (*drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;*)
    drawing#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 ~width ~height !backing#pixmap;
    false
  in

  let really_add_node role x y =
    let _ = debug "Creating node" in
    let id = freshnode () in
    let () = nodes:= (id,{role=role ; posx=x ; posy=y})::!nodes in
    id
  in
  let really_remove_node id =
    let _ = debug "Removing node" in
    let () = nodes:= List.remove_assoc id !nodes in
    ()
  in
  let add_node () = 
    match !state with
      | SelectNode id -> 
          let _ = debug "AddNode" in 
          let () = state := AddNode in
          aspect#misc#hide ()
      | _ -> 
          let _ = debug "AddNode" in 
          let () = state := AddNode in
          ()
  in
  let add_edge () = 
    let _ = debug "AddEdge" in 
    match !state with
      | _ -> 
          let () = state := AddEdge None in
          aspect#misc#hide ();
          ()
  in
  let remove_node () = 
    match !state with
      | SelectNode id -> 
          let _ = debug "RemoveNode" in
          let () = state := RemoveNode in
          aspect#misc#hide ()
      | _ -> 
          let _ = debug "RemoveNode" in
          let () = state := RemoveNode in
          ()
  in
  let update_node () =
    match !state with
      | SelectNode id -> begin
          aspect#misc#hide ();
          let node = List.assoc id !nodes in
          let () = node.role <- roleentry#text in
          redraw ();
          state := Idle
        end
      | _ -> assert false
  in
  let update_edge () =
    match !state with
      | SelectEdge id -> begin
          aspect2#misc#hide ();
          let edge = List.assoc id !edges in
          let () = edge.label <- edgeentry#text in
          redraw ();
          state := Idle
        end
      | _ -> assert false
  in
  let select_node id =
    let node = List.assoc id !nodes in
    aspect#misc#show ();
    rolebutton#grab_default ();
    nodelabel#set_text ("Node number "^(string_of_int id)) ;
    roleentry#set_text node.role ;
    state := SelectNode id
  in
  let select_edge id =
    let edge = List.assoc id !edges in
    aspect2#misc#show ();
    edgebutton#grab_default ();
    edgelabel#set_text ("Edge number "^(string_of_int id)) ;
    edgeentry#set_text edge.label ;
    state := SelectEdge id
  in
  let clicked ev =
    let _ = debug "Clicked" in
    match !state with
      | Idle | SelectEdge _ -> begin
          if GdkEvent.Button.button ev = 1 then (
            let x = int_of_float (GdkEvent.Button.x ev) /dim in
            let y = int_of_float (GdkEvent.Button.y ev) /dim in
            match get_by_pos (x,y) !nodes with
                None -> debug "There isn't any node here."
              | Some id -> begin
                  debug "Selecting a node.";
                  select_node id;
                end);
          true
        end
      | AddEdge opt -> begin
          if GdkEvent.Button.button ev = 1 then (
            let x = int_of_float (GdkEvent.Button.x ev) /dim in
            let y = int_of_float (GdkEvent.Button.y ev) /dim in
            match opt,get_by_pos (x,y) !nodes with
              | _,None ->  debug "There isn't any node here."
              | None,Some id -> (state:= AddEdge (Some id))
              | Some source,Some dest ->
                  if source = dest then
                    debug "Source and Destination have to be different."
                  else
                    let id = freshedge() in
                    (edges:=(id,{source=source;dest=dest;label=""})::!edges;
                     let () = redraw () in
                     debug "Adding an edge.";
                     select_edge id;
                     ()
                    )
          )
          else if GdkEvent.Button.button ev = 3 then (
            let () = state := Idle in
            debug "Stop adding an edge."
          );
          true
        end
      | AddNode -> begin
          if GdkEvent.Button.button ev = 1 then (
            let x = int_of_float (GdkEvent.Button.x ev) /dim in
            let y = int_of_float (GdkEvent.Button.y ev) /dim in
            if get_by_pos (x,y) !nodes = None 
            then
              let nodeid = really_add_node "" x y in
              let () = redraw () in
              select_node nodeid;
              ()
            else
              let _ = debug "There is already a node here." in
              ());
          true
        end
      | RemoveNode -> begin
          if GdkEvent.Button.button ev = 1 then (
            let x = int_of_float (GdkEvent.Button.x ev) /dim in
            let y = int_of_float (GdkEvent.Button.y ev) /dim in
            match get_by_pos (x,y) !nodes 
            with None -> debug "There isn't any node here."
              | Some 0 -> debug "You cannot remove the initial node."
              | Some id ->
                  let nodeid = really_remove_node id in
                  let () = redraw () in
                  let () = state := Idle in
                  ())
          else if GdkEvent.Button.button ev = 3 then (
            let () = state := Idle in
            debug "Stop removing a node."
          );
          true
        end
      | SelectNode id -> begin
          if GdkEvent.Button.button ev = 1 then (
            let x = int_of_float (GdkEvent.Button.x ev) /dim in
            let y = int_of_float (GdkEvent.Button.y ev) /dim in
            match get_by_pos (x,y) !nodes 
            with 
              | None -> 
                  let node = List.assoc id !nodes in
                  let () = node.posx <- x in
                  let () = node.posy <- y in
                  let () = redraw () in
                  debug "Moving a node here."
              | Some id ->begin
                  debug "Selecting another node.";
                  select_node id;
                end)
          else if GdkEvent.Button.button ev = 3 then (
            let () = aspect#misc#hide () in
            let () = redraw () in
            let () = state := Idle in
            debug "Deselection of a node."
          );
          true
        end 
  in
  let _ = badd#connect#clicked ~callback:add_node in
  let _ = bedge#connect#clicked ~callback:add_edge in
  let _ = brem#connect#clicked ~callback:remove_node in
  let _ = rolebutton#connect#clicked ~callback:update_node in
  let _ = edgebutton#connect#clicked ~callback:update_edge in
  let _ = area#event#connect#button_press ~callback:clicked in
  let _ = area#event#connect#expose ~callback:(expose area backing) in
  let _ = area#event#connect#configure ~callback:(configure window backing) in
  let _ = area#event#add
    [`EXPOSURE;`LEAVE_NOTIFY;`BUTTON_PRESS;`POINTER_MOTION;`POINTER_MOTION_HINT]
  in 









  let _ = window#show () in
  GMain.Main.main ()

let _ = main ()
