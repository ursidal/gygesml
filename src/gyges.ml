
open Gyges_types
   
(*type joueur =
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

type deplacement =  
	| Init of joueur * (case list)
	| Depl of case * case;;

type jeu = case*pion list;;
*)

let compare_case c1 c2 =
  match (c1,c2) with
  |(CaseSud,CaseSud) -> 0
  |(CaseSud,_) -> -1
  |(_,CaseSud) -> 1
  |(CaseNord,CaseNord)-> 0
  |(CaseNord,_) -> -1
  |(_,CaseNord) -> 1
  |(Case (m1,m2),Case (n1,n2))-> (m2-n2)+6*(m1-n1);; 



let (|>) x f = f x;; 

let init_jeu ()= [] ;;

let suivant = function
	|Sud -> Nord
	|Nord -> Sud

let rec pion_dans_case partie case =
  match partie with
  |[] -> Vide
  |hd::_ when (fst hd) = case -> snd hd
  |_::tl -> pion_dans_case tl case
;;


let ajout_cle f cle valeur =
  fun x -> if x = cle then valeur else f x;;

let cle_ajout cle valeur f = ajout_cle f cle valeur;;

let mise_a_jour partie depl =
  match depl with 
  |Init (j,l) -> let ligne_init = match j with
  				|Sud -> 1
				|Nord -> 6 
		 in
		 let update_list = List.map2 (fun i v -> ((ligne_init,i),v)) [1;2;3;4;5;6] l
		 in List.append update_list partie
  |Depl (c1,c2) -> let p1 = pion_dans_case partie c1 in
  (c1,Vide)::(c2,p1):: partie
;;


let meme_chemin d1 d2 =
  match (d1,d2) with
  |((x,y),(z,t)) when (x = z) && (y = t)-> true
  |((x,y),(z,t)) when (x = t) && (y = z)-> true
  |(_,_) -> false;;

let rec par_paire l =
  match l with
  |[]->[]
   |_::[]->[]
  |x::y::tl -> (x,y)::(par_paire (y::tl));;

let pas_recoupe l =
  match l with
  |[] -> true
  |_::[] -> true
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
        lvoisins |> List.map (fun x -> x::lchemin) |> List.filter pas_recoupe |> List.map (fun lch -> let dern = List.hd lch in chemins partie (int_de_pos (pion_dans_case partie dern)) lch) |> List.concat  
  |n -> let dernier = List.hd lchemin in
        let lvoisins = List.filter (fun p -> ((pion_dans_case partie p) = Vide && p<>CaseNord && p<>CaseSud)) (voisins dernier) in
        lvoisins |> List.map (fun x -> x::lchemin) |> List.filter pas_recoupe |> List.map (fun lch -> chemins partie (n-1) lch) |> List.concat
;;

let arrivees_possibles jeu case=
  match pion_dans_case jeu case with
  |Vide -> []
  |Val n -> let liste_chemins = chemins jeu n [case] in
        List.sort_uniq compare_case (List.map List.hd liste_chemins)
;; 


let departs_possibles jeu joueur =
  let rec premiere_ligne i =
  let pion_de_ligne = List.filter (fun x -> x<> Vide) (List.map (fun x -> pion_dans_case jeu (Case (x,i))) [1;2;3;4;5;6]) in
  if pion_de_ligne = [] then
    let j = match joueur with
    |Sud -> i+1
    |Nord -> i-1
    in
    premiere_ligne j
  else
    i
  in
  let i =
    match joueur with
    |Sud -> 1
    |Nord -> 6
  in
  let ligne =  (premiere_ligne i) in
  List.filter (fun x -> (pion_dans_case jeu x)<>Vide) (List.map (fun x-> Case (x,ligne)) [1;2;3;4;5;6])
;;
  
let mouv_possibles jeu joueur = 
	let ld = departs_possibles jeu joueur 
	in
	(List.map (fun dep -> List.map (fun arr -> (dep,arr)) (arrivees_possibles jeu dep)) ld) |> List.concat

let jeu_suiv (pl,joueur) = 
	let lmouv = mouv_possibles pl joueur in
	List.map (fun m -> (mise_a_jour pl m, suivant joueur)) lmouv

let rec string_of_vlist l=
 	match l with
    |[] -> "."
    |Vide::q -> " ;"^(string_of_vlist q)
    |(Val k)::q -> (string_of_int k)^";"^(string_of_vlist q);;

print_string (string_of_vlist [Val 1;Val 2;Vide;Val 3]);;

(*let (|>) x f = f x;;*)
let supp = [Val 1;Val 2;Val 3];;

let compte_val k l = 
    	List.fold_left (fun acc x -> if x = Val k then acc+1 else acc) 0 l
        
let ajout_possible l = 
        	List.filter (fun x -> match x with 
            	|Vide -> false
                |Val k -> (compte_val k l)<2 ) supp;;
                       
let ligne_dep nb = 
    let rec fait_dep n ll =
    	if n = 0 then ll
        else 
        let ll_augment = List.fold_left 
        					(fun acc x -> 
                            		List.append  
                                    	(List.map 
                                        	(fun h -> h::x) 					
                                            (ajout_possible x) 
                                         ) 
                                     	 acc) 
                            [] 
                            ll 
        in fait_dep (n-1) ll_augment
        in
        fait_dep nb [[]];;
 


let test = init_jeu();;

let ajout f p = let (k,v) = p in ajout_cle f k v;;


let encours = (List.map (fun ((x,y),z) -> (Case(x,y),Val z)) [((1,1),3);((3,1),1);((4,1),3);((6,1),1);((4,2),2);((3,3),2);
                                                                                               ((5,3),3);((3,4),3);((1,6),1);((3,6),2);((4,6),1);((5,6),2)]);;
