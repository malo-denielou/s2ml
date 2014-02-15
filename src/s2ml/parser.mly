%{
  (********************************************************************)
  (* Session - Implementation                                         *)
  (*                                                                  *)
  (* parser.mly                                                       *)
  (********************************************************************)
  (* $Id: parser.mly 9310 2009-01-27 15:07:25Z denielou $ *)

  open Common
  open Syntax
  open Lexing


  let debug = gen_debug debug_parserlexer "parser"
    
  let error t info =
    debug ("Error generation: "^t) ;
    let (l,c1),(_,c2) = info in
    let s = Printf.sprintf "%d:%d-%d" l c1 c2 in
    if t = "" then raise (Common.Parse_error (s,info))
    else raise (Common.Parse_error ((*s^ ": " ^ *)t,info))

  let parse_error _ =
    let start_pos = Parsing.symbol_start_pos () in
    let end_pos = Parsing.symbol_end_pos () in
    let (l1,c1),(l2,c2) =
      (start_pos.pos_lnum,start_pos.pos_bol),(end_pos.pos_lnum,end_pos.pos_bol)
    in
(*    let s = Printf.sprintf "%d:%d-%d" l1 c1 c2 in*)
    raise (Common.Parse_error ("",((l1,c1),(l2,c2))))

    
    

%}

%token <Common.info> ROLE SESSION VAR
%token <Common.info> EOF
%token <Common.info> SEND RECV OR ARROW TRUSTS INF
%token <Common.info> LPA RPA LCB RCB LBR RBR
%token <Common.info> LANG
%token <Common.info * string> RANG
%token <Common.info> DOT SEMI EQUAL COMMA MUL COLON PLUS
%token <Common.info * string> IDENTUP IDENTLO
%token <Common.info * string> TYP
%left OR
%start sess
%type <Syntax.ast> sess

%%

sess:
| SESSION IDENTUP EQUAL ROLE roles      { (snd $2, $5) }
| SESSION IDENTUP EQUAL ROLE            { error "Missing role name" ($3) }
| SESSION IDENTUP EQUAL                 { error "Missing role declaration" ($3)}
| SESSION IDENTUP                       { error "Missing equal" (fst $2) }
| SESSION IDENTLO                       { error "Lowercase session name" (fst $2) }
| SESSION                               { error "Missing identifier" ($1) }
| IDENTLO                               { error "Sessions should start with keyword 'session'" (fst $1) }
| IDENTUP                               { error "Sessions should start with keyword 'session'" (fst $1) }
|                                       { error "Shouldn't happen" bogusInfo }
;


roles:
| IDENTLO COLON IDENTLO EQUAL session ROLE roles    { (snd $1,snd $3,$5)::$7 }
| IDENTLO COLON                               { error "Missing return type" $2 }
| IDENTLO EQUAL session ROLE roles            { (snd $1,"string",$3)::$5 }
| IDENTLO EQUAL session ROLE                  { error "Missing role name" $4 }
| IDENTLO COLON IDENTLO EQUAL session         { (snd $1,snd $3,$5)::[] }
| IDENTLO EQUAL session                       { (snd $1,"string",$3)::[] }
| IDENTLO EQUAL                               { error "Missing session description" ($2)}
| IDENTLO                                     { error "Missing equal" (fst $1)}
;


session:
| IDENTLO COLON session             { ASMu (merge_info (fst $1) $2,snd $1,$3) }
| IDENTLO COLON                     { error "Missing session after recursive point" $2 }
| IDENTLO                           { error "Missing colon after recursing point name" (fst $1) }
| SEND LPA sendmsg sendmsglist      { ASSend ($1,$3::$4) }
| SEND LPA sendmsg                  { error "Missing matching parenthesis" $2 }
| SEND LPA                          { error "Missing matching parenthesis" $2 }
| SEND sendmsg                      { ASSend ($1,[$2]) }
| SEND                              { error "Missing label" $1 }
//| RECV LBR OR recvmsg recvmsglist   { ASReceive ($1,$4::$5) }
//| RECV LBR OR                       { error "Missing label" $3 }
| RECV LBR OR recvmsg recvmsglist     { ASReceive ($1,$4::$5) }
| RECV LBR recvmsg recvmsglist        { ASReceive ($1,$3::$4) }
| RECV LBR recvmsg                    { error "Missing label" $2 }
| RECV LBR                            { error "Missing label" $2 }
| RECV recvmsg                        { ASReceive ($1,[$2]) }
| RECV                                { error "Missing label" $1 }


label:
| IDENTUP COLON IDENTLO               { (snd $1,snd $3) }
| IDENTUP                             { (snd $1,"unit") }
;

sendsession:
| SEND LPA sendmsg sendmsglist    { ASSend ($1,$3::$4) }
| SEND LPA sendmsg error          { error "Missing matching parenthesis" $2 }
| SEND LPA                        { error "Missing matching parenthesis" $2 }
| SEND sendmsg                    { ASSend ($1,[$2]) }
| SEND                            { error "Missing label" $1 }
| IDENTLO COLON sendsession       { ASMu (merge_info (fst $1) $2,snd $1,$3) }
| IDENTLO COLON                   { ASMu (merge_info (fst $1) $2,snd $1,ASEnd) }
| IDENTLO                         { ASGoto (fst $1,snd $1) }
;

sendmsg:
| label SEMI recvsession { ({label = fst $1; payload = snd $1},$3)}
| label SEMI             { ({label = fst $1; payload = snd $1},ASEnd)}
| label                  { ({label = fst $1; payload = snd $1},ASEnd)}
;

sendmsglist:
| PLUS label SEMI recvsession sendmsglist { [({label = fst $2; payload = snd $2},$4)]@$5}
| PLUS label sendmsglist                  { [({label = fst $2; payload = snd $2},ASEnd)]@$3}
| PLUS label error                        { error "Missing semi-colon" $1 }
| PLUS                                    { error "Missing label" $1 }
| OR                                      { error "Recv lists use '|' and not '+'" $1 }
| RPA                                     { [] }
| PLUS label SEMI sendmsglist             { [({label = fst $2; payload = snd $2},ASEnd)]@$4}
;



recvsession:
| RECV LBR OR recvmsg recvmsglist     { ASReceive ($1,$4::$5) }
| RECV LBR recvmsg recvmsglist        { ASReceive ($1,$3::$4) }
| RECV LBR recvmsg error              { error "Missing label" $2 }
| RECV recvmsg                        { ASReceive ($1,[$2]) }
//| RECV LBR                            { error "Missing label" $2 }
| RECV                                { error "Missing label" $1 }
| IDENTLO COLON recvsession           { ASMu (merge_info (fst $1) $2,snd $1,$3) }
| IDENTLO COLON                       { ASMu (merge_info (fst $1) $2,snd $1,ASEnd) }
| IDENTLO                             { ASGoto (fst $1,snd $1) }
//| RECV                                { error "Missing label" $1 }
//| RECV LBR                            { error "Missing label" $2 }
//| RECV LBR OR                         { error "Missing label" $3 }
//| RECV LBR recvmsg                    { error "Missing corresponding right bracket" $2}
;

recvmsg:
| label ARROW sendsession { ({label = fst $1; payload = snd $1},$3)}
| label ARROW             { error "Missing sending session" $2}
| label                   { ({label = fst $1; payload = snd $1},ASEnd)}
;

recvmsglist:
| OR label ARROW sendsession recvmsglist { [({label = fst $2; payload = snd $2},$4)]@$5}
//| OR label ARROW sendsession error       { error "Missing pipe" $3}
//| OR label ARROW error                   { error "Wrong session" $3}
| OR label ARROW                         { error "Missing sending session" $3}
| OR label recvmsglist                   { [({label = fst $2; payload = snd $2},ASEnd)]@$3} 
//| OR label                               { error "Missing right bracket" (fst $2)}
| OR                                     { error "Missing label" $1 }    
| RBR                                    { [] }
;

