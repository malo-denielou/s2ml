open Loi
open Loi_protocol
open Printf
open Prins

let _ = 
  let alice = {id="Alice"; cert="alice"; ip="127.0.0.1"; port=8001} in
  let bob = {id="Bob"; cert="bob"; ip="127.0.0.1"; port=8002} in
  let charlie = {id="Charlie"; cert="alice"; ip="127.0.0.1"; port=8003} in
  let don = {id="Don"; cert="bob"; ip="127.0.0.1"; port=8004} in
  let eddy = {id="Eddy"; cert="alice"; ip="127.0.0.1"; port=8005} in
  let frank = {id="Frank"; cert="bob"; ip="127.0.0.1"; port=8006} in
  register alice; register bob ;register charlie; register don ; register eddy;
  register frank

let start_ministre () =
  let rec handler = function (_,s)  ->
    Expose (s,
            {hQuestions = handler ;
             hAdopte = (function _ ->
                          PassageCommission("Texte","Fini"))}) in
  let res = 
    Loi.ministre {prins_ministre = "Alice";
                  prins_gouvernement = "Bob";
                  prins_commission = "Charlie";
                  prins_assemblee = "Don";
                  prins_depute = "Eddy";
                  prins_conseildetat = "Frank"}
    (ProjetLoi("Hadopi",
           {hConseilMinistre = handler})) in
  printf "Je suis %s\n" res


let start_gouvernement () =
  let rec handler_adopte =
    {hAudition = (function _ -> Reponse("Pfft",handler_adopte));
     hTexteFinal = (function _ -> "Ouf");
     hEchec = (function _ -> "Zut")} in
  let rec handler_expose =
    {hExpose = (function (_,s) ->
                  if s = "Texte"
                  then Questions("plus dur",handler_expose)
                  else Adopte((),handler_adopte)
    )} in
  let rec handler_soum texte = 
    {hAvisNegatif = (function _ -> 
                       let texte = texte^"moins dur" in
                       Soumission(texte,
                                  handler_soum texte));
     hAvisPositif = function _ -> 
       ConseilMinistre(texte,handler_expose)
    } in
  let res = 
    Loi.gouvernement "Bob"
      {hProjetLoi = function (_,texte) ->
         Soumission(texte,handler_soum texte)
      }
  in
  printf "Je suis %s\n" res

let start_commission () =
  let res = 
    Loi.commission "Charlie"
      {hPassageCommission = function (_,texte) ->
         TextePropose(texte,"c'est tout !")
      }
  in
  printf "La commission a fait son boulot et %s\n" res


let start_assemblee () =
  let rec handler_seance s =
    {hAuditionGouv = (function _ ->
                        Audition((),
                                 {hReponse = function _ -> 
                                    Parole((),handler_seance s)}));
     hException = (function _ ->
                     VoteException((),
                                   {hExceptionRejetee = 
                                       (function _ -> 
                                          DebutSeance(s,handler_seance s));
                                    hExceptionAcceptee =
                                       (function _ -> Echec((),"Rejet du texte"))
                                   }));
     hAmendement = (function _ ->
                     VoteAmendement((),
                                   {hAmendementRejete = 
                                       (function _ -> 
                                          DebutSeance(s,handler_seance s));
                                    hAmendementAccepte = 
                                       (function (_,a) ->
                                          DebutSeance(s^a,handler_seance s))}));
     hTexte = (function (_,s) ->
                 Vote((),{hApprouve = (function _ -> 
                                         TexteFinal((),"Texte adopte"));
                          hRejete = (function _ -> 
                                       Echec((),"Texte rejete"))
                         }))
    } in
  let res = 
    Loi.assemblee "Don"
      {hTextePropose = 
          function (_,s) ->
            DebutSeance(s,handler_seance s)} in
  printf "Resultat %s\n" res
    
let start_depute () =
  let rec choice texte = 
    if texte = "" 
    then AuditionGouv((),{hParole=function _ -> choice texte})
    else if texte = "" 
    then Exception((),{hVoteException=function _ -> 
                         if true 
                         then ExceptionRejetee((),handler_seance)
                         else ExceptionAcceptee((),"Rejet du texte")})
    else if texte = "" 
    then Amendement("article +1",{hVoteAmendement=function _ -> 
                         if true 
                         then AmendementAccepte("+1",handler_seance)
                         else AmendementRejete((),handler_seance)})
    else Texte(texte,{hVote=function _ -> 
                        if true 
                        then Approuve((),"Texte adopte")
                        else Rejete((),"Rejet du texte")})
  and handler_seance =
    {hDebutSeance = function (_,texte) -> choice texte
    }
  in
  let res = 
    Loi.depute "Eddy" handler_seance
  in
  printf "Resultat %s\n" res



let start_conseil () =
  let rec handler =
    {hSoumission = function (_,texte) ->
         if texte = "Texte"
         then AvisNegatif("Change!",handler)
         else AvisPositif((),"c'est tout !")
      }
  in
  let res = 
    Loi.conseildetat "Frank" handler
  in
  printf "La commission a fait son boulot et %s\n" res

let _ = Pi.fork start_conseil
let _ = Pi.fork start_commission
let _ = Pi.fork start_ministre
let _ = Pi.fork start_gouvernement
let _ = Pi.fork start_assemblee
let _ = start_depute ()
