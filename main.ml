
type joueur =
  | Nord | Sud ;;

type pion =
  | Vide
  | Val of int
;;

type case =
  |Case of int * int
  |CaseNord
  |CaseSud
;;

let compare_case c1 c2 =
  match (c1,c2) with
  |(CaseSud,CaseSud) -> 0
  |(CaseSud,_) -> -1
  |(_,CaseSud) -> 1
  |(CaseNord,CaseNord)-> 0
  |(CaseNord,_) -> -1
  |(_,CaseNord) -> 1
  |(Case (m1,m2),Case (n1,n2))-> (m2-n2)+6*(m1-n1);; 

type deplacement =  case * case;;

let (|>) x f = f x;; 

let init_jeu ()= fun x -> Vide ;;

let ajout_cle f cle valeur =
  fun x -> if x = cle then valeur else f x;;

let cle_ajout cle valeur f = ajout_cle f cle valeur;;

let mise_a_jour jeu depl =
  let (c1,c2) = depl in
  fun x -> match x with
           |x when x = c1 -> Vide
           |x when x = c2 -> jeu c1
           |_ -> jeu x
;;


let meme_chemin d1 d2 =
  match (d1,d2) with
  |((x,y),(z,t)) when (x = z) && (y = t)-> true
  |((x,y),(z,t)) when (x = t) && (y = z)-> true
  |(_,_) -> false;;

let rec par_paire l =
  match l with
  |[]->[]
   |x::[]->[]
  |x::y::tl -> (x,y)::(par_paire (y::tl));;

let pas_recoupe l =
  match l with
  |[] -> true
  |x::[] -> true
  |_-> let lp = par_paire l in
       List.for_all (fun x -> not(meme_chemin x (List.hd lp))) (List.tl lp);;
  
let voisins_bords pos =
  let (xp,yp) = pos in
  List.filter
    (fun (x,y) -> (x>0)&&(x<7)&&(y>0)&&(y<7))
    [(xp-1,yp);(xp+1,yp);(xp,yp-1);(xp,yp+1)];;

let voisins pos =
  let lambd = List.map (fun (x,y) -> Case (x,y)) in
  match pos with
  |CaseNord -> List.map (fun i ->Case (i,6)) [1;2;3;4;5;6]
  |CaseSud -> List.map (fun i -> Case (i,1)) [1;2;3;4;5;6]
  |Case (x,1) -> CaseSud::lambd (voisins_bords (x,1))
  |Case (x,6) -> CaseNord::lambd (voisins_bords (x,6))
  |Case (x,y) -> lambd (voisins_bords (x,y))
;;

let int_de_pos p =
  match p with
  |Vide ->0
  |Val n ->n
;;


let rec chemins partie n lchemin =
  match n with
  |0 -> [lchemin]
  |1 ->  let dernier = List.hd lchemin in
        let lvoisins = voisins dernier in
        lvoisins |> List.map (fun x -> x::lchemin) |> List.filter pas_recoupe |> List.map (fun lch -> let dern = List.hd lch in chemins partie (int_de_pos (partie dern)) lch) |> List.concat  
  |n -> let dernier = List.hd lchemin in
        let lvoisins = List.filter (fun p -> ((partie p) = Vide && p<>CaseNord && p<>CaseSud)) (voisins dernier) in
        lvoisins |> List.map (fun x -> x::lchemin) |> List.filter pas_recoupe |> List.map (fun lch -> chemins partie (n-1) lch) |> List.concat
;;

let depl_possibles jeu (x,y)=
  match jeu (Case (x,y)) with
  |Vide -> []
  |Val n -> let liste_chemins = chemins jeu n [Case (x,y)] in
        List.sort_uniq compare_case (List.map List.hd liste_chemins)
;; 


let test = init_jeu();;

let ajout f p = let (k,v) = p in ajout_cle f k v;;


let encours = List.fold_left ajout test (List.map (fun ((x,y),z) -> (Case(x,y),Val z)) [((1,1),3);((3,1),1);((4,1),3);((6,1),1);((4,2),2);((3,3),2);
                                                                                               ((5,3),3);((3,4),3);((1,6),1);((3,6),2);((4,6),1);((5,6),2)]);;

let pions p =
  match p with
  |Vide ->"٠"
  |Val 1 ->"١"
  |Val 2 ->"٢"
  |Val 3 ->"٣";;

let affichage_plateau jeu =
  let sud = [(*"    \\ \\ \\ / / /";*)"S        ٠     ";"    a b c d e f"] and nord = ["N        ٠     " (*;"    / / / \\ \\ \\"*)] in
let interieur = (List.map (fun x -> String.concat " " ((string_of_int x)::" "::(List.map (fun y-> pions (jeu (Case (y,x)))) [1;2;3;4;5;6]))) [1;2;3;4;5;6]) |> List.rev in
  [ nord;interieur;sud] |> List.concat |> List.iter print_endline ;;


