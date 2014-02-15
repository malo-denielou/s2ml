open Data

let yield (n:int) = Thread.delay (float_of_int n)

let fork (c:unit->unit) = 
  let _ = Thread.create c () in
  ()

let fork_dep (c:unit->unit) (d:unit->unit) =
  let t1 = Thread.create c () in
  d ();
  Thread.join t1


type chan = str option ref

let chanDb : (string * chan) list ref = ref []

let create_chan s =
  let c : chan = ref None  in
  chanDb := (s,c)::!chanDb

let recv (prin:string) = 
  let c = List.assoc prin !chanDb in
  while !c = None do
    Thread.delay 1.0
  done ;
  match !c with
      Some v -> (c:= None ; v)
    | _ -> assert false
        
let send (prin:string) v = 
  let c = List.assoc prin !chanDb in
  match !c with
      Some u -> assert false
    | None -> c:= Some v


let assume = function e -> ()
let expect = function e -> ()
