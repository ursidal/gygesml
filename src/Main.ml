(* This line opens the Tea.App modules into the current scope for Program access functions and types *)
open Tea.App

(* This opens the Elm-style virtual-dom functions and types into the current scope *)
open Tea.Html

open Gyges_types
   
(* Let's create a new type here to be our main message type that is passed around *)
type msg =
  | ToggleDesc
  | Pions of string
  | ChangePions
  | Choix of case
  [@@bs.deriving {accessors}] (* This is a nice quality-of-life addon from Bucklescript, it will generate function names for each constructor name, optional, but nice to cut down on code, this is unused in this example but good to have regardless *)


let original = [|{j|●|j};{j|○|j};{j|◎|j};{j|🞋|j}|];;

let defaut = [|{j|٠|j};{j|١|j};{j|٢|j};{j|٣|j}|];;

let chiffre = [|".";"1";"2";"3"|];;

let init_jeu ()= [] ;;

let test = init_jeu();;

let ajout_cle f cle valeur =
  fun x -> if x = cle then valeur else f x;;

let cle_ajout cle valeur f = ajout_cle f cle valeur;;

let ajout f p = let (k,v) = p in ajout_cle f k v;;


let encours = (List.map (fun ((x,y),z) -> (Case(x,y),Val z)) [((1,1),3);((3,1),1);((4,1),3);((6,1),1);((4,2),2);((3,3),2);
                                                                                               ((5,3),3);((3,4),3);((1,6),1);((3,6),2);((4,6),1);((5,6),2)]);;


let pions config p =
  let parray = Js.String.split "" config in
  match p with
  |Vide -> parray.(0)
  |Val i -> parray.(i)
;;

type model =
  { partie: (case * pion) list;
    joueur: joueur;
    pions: string array;
    desc: bool;
    choixprem: case option;
    choixdeux: case option;
    message: string
  }
  
let string_vers_array str =
  let n = String.length str in
  let rep = Array.make n "" in
  for i=0 to n do
    rep.(i) <- String.make 1 (String.get str i);
  done;
  rep
  
    
           
(* This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values, the model for Counter is just an integer *)
let init() =
  { partie= encours
                ; joueur= Sud
                ; pions= original
                ; desc= false
                ; choixprem = None
                ; choixdeux = None
                ; message = ""
                }

  
let case_vers_string case =
  match case with
  |None -> {js|aucune case sélectionnée|js}
  |Some case -> match case with
                |Case (x,y) -> {j| case ($(x),$(y))|j}
                |CaseNord -> "case Nord"
                |CaseSud -> "case Sud"

(* This is the central message handler, it takes the model as the first argument *)
let update model = function (* These should be simple enough to be self-explanatory, mutate the model based on the message, easy to read and follow *)
  | ToggleDesc -> {model with desc=not model.desc}
  | Pions str ->  {model with pions=string_vers_array str}
  |ChangePions -> let nou = match model.pions with
                    |x when x = original -> defaut
                    |_ -> original
                  in
                  {model with pions=nou}
  |Choix case ->
    match model.choixprem with
    |None ->
      if List.exists (fun x -> x=case) (Gyges.departs_possibles model.partie model.joueur)
      then {model with choixprem = Some case;  message = "premier choix: "^(case_vers_string (Some case))}
      else {model with message="Choix invalide"}
    |Some prem ->
      if Some case = model.choixprem
      then {model with choixprem = None}
      else
        if List.exists (fun x -> x = case) (Gyges.depl_possibles model.partie prem)
        then
          match case with
          |CaseNord -> {model with partie = Gyges.init_jeu ();
                                   choixprem = None;
                                   message = {js|Sud a gagné|js}}
           |CaseSud -> {model with partie = Gyges.init_jeu ();
                                   choixprem = None;
                                   message = {js|Nord a gagné|js}}
           |_ ->  {model with partie = Gyges.mise_a_jour model.partie (prem,case)
                       ; joueur = if model.joueur=Sud then Nord else Sud
                       ; choixdeux = Some case
                       ; message = "deuxieme choix: "^(case_vers_string (Some case))
                       ;choixprem = None}
        else {model with message={js|Case inacessible|js} }
                  
(* This is just a helper function for the view, a simple function that returns a button based on some argument *)
let view_button title msg =
  button
    [ onClick msg
    ]
    [ text title
    ]


let ascii_titre = {|
         _    _        _          _              _           _        
        /\ \ /\ \     /\_\       /\ \           /\ \        / /\      
       /  \ \\ \ \   / / /      /  \ \         /  \ \      / /  \     
      / /\ \_\\ \ \_/ / /      / /\ \_\       / /\ \ \    / / /\ \__  
     / / /\/_/ \ \___/ /      / / /\/_/      / / /\ \_\  / / /\ \___\ 
    / / / ______\ \ \_/      / / / ______   / /_/_ \/_/  \ \ \ \/___/ 
   / / / /\_____\\ \ \      / / / /\_____\ / /____/\      \ \ \       
  / / /  \/____ / \ \ \    / / /  \/____ // /\____\/  _    \ \ \      
 / / /_____/ / /   \ \ \  / / /_____/ / // / /______ /_/\__/ / /      
/ / /______\/ /     \ \_\/ / /______\/ // / /_______\\ \/___/ /       
\/___________/       \/_/\/___________/ \/__________/ \_____\/        |}
;;

let style_item = styles [("padding","5px")];;

let menu =
  div
    [style "margin" "20px"]
    [span [style_item] [text "Description"]
    ;span [style_item] [text {js|Règles|js}]
    ;span [style_item] [text "Config"]
    ]
;;

let donne_position cote (x,y) =
  let intervalle = cote/8 in
  (intervalle*(x),intervalle*(7-y))
;;

external window_width : int = "window.innerWidth" [@@bs.val]
external window_height : int = "window.innerHeight" [@@bs.val]

let max_size = max window_width window_height
;;


let val_de_pion p =
  match p with
  |Vide -> 0
  |Val n -> n
;;
let vers_pix n = (string_of_int n)^"px";;
  
let grille_liste m n =
  let rec range i j =
    match j-i with
    |0 -> []
    |_ -> i::(range (i+1) j)
  in
  (range m (n+1)) |> List.map (fun y -> List.map (fun x -> (x,y)) (range m (n+1))) |> List.flatten
  ;;


let vue_plateau model =
  let jeu = model.partie and str = model.pions and prem = model.choixprem in
  let cote = min ((min window_width window_height)/2) 600 in
  let carre = grille_liste 1 6 in
  let intervalle = cote/8 in
  let fait_case x y case =
  span
    [onClick (Choix case)
    ; if prem = Some case  then class' "case-active" else class' "case-inactive"
    ; styles
       [("left",(string_of_int x)^"px")
       ;("top",(string_of_int y)^"px")
       ;"width",vers_pix intervalle
       ;"height",vers_pix intervalle
       ;"font-size",vers_pix (intervalle*3/4)]]
    [text str.((val_de_pion (Gyges.pion_dans_case jeu case)))] in
  let appel (x,y) =
    let (xp,yp) = donne_position cote (x,y) in
    fait_case xp yp (Case (x,y))
  in
  let nord = fait_case ((cote-intervalle)/2) 0 CaseNord and
      sud = fait_case ((cote-intervalle)/2) (cote-intervalle) CaseSud in
  div
    [class' "plateau"; styles ["width", vers_pix cote
                              ;"height", vers_pix cote
    ]]
    ((div [class' "bord-plateau"] [text ""])::nord::sud::(List.map appel carre))
;;


                
(* This is the main callback to generate the virtual-dom.
  This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing *)
let view model =
  div
    [styles [("max-width","800px");("margin","auto");("background-color","#ccc")]]
    [ h3
        [ onClick ToggleDesc;styles [ ("text-align", "center")  ]] 
        [ pre [styles [("margin","auto");("font-size","8px");("font-weight","bold")]] [text ascii_titre]
        ; menu
        ]
    ; if model.desc then p [] [text "Description de Gyges"] else noNode
    ; span [ style "font_size" "20px"] [text (String.concat " " (Array.to_list model.pions))]
    ; button [onClick ChangePions] [text "Change les pions"]
    ; vue_plateau model
    ; p [] [text model.message] 
    ]

(* This is the main function, it can be named anything you want but `main` is traditional.
  The Program returned here has a set of callbacks that can easily be called from
  Bucklescript or from javascript for running this main attached to an element,
  or even to pass a message into the event loop.  You can even expose the
  constructors to the messages to javascript via the above [@@bs.deriving {accessors}]
  attribute on the `msg` type or manually, that way even javascript can use it safely. *)
let main =
  beginnerProgram { (* The beginnerProgram just takes a set model state and the update and view functions *)
    model = init (); (* Since model is a set value here, we call our init function to generate that value *)
    update;
    view;
  }
