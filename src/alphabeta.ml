open Gyges_types;;

let iswinner liste_coup joueur =
  let case_gagnante = match joueur with
    |Sud -> CaseNord
    |Nord -> CaseSud
  in
    List.exists (fun c -> c = case_gagnante) liste_coup  

let winscore = function
  |Sud -> -100
  |Nord -> 100;;
  
  
let estMaximisant = function 
	|Sud -> false 
	|Nord -> true
	
let rec fold_until fonction_et_test acc liste =
	match liste with 
	|[] -> acc
	|t::q -> let (acc2,booleen) = fonction_et_test acc t in
			 if booleen 
			 then acc2
			 else fold_until fonction_et_test acc2 q;;		 

let rec alphabeta (jeu,joueur) alpha beta n =
  let liste_coup = Gyges.mouv_possibles jeu joueur in
  if iswinner liste_coup joueur 
  then 
    winscore joueur
  else
    if n = 0 
	then 
	  (0, List.hd liste_coup)
	else
	  let coups_suivants = List.map (fun m -> (m,Gyges.suivant joueur)) liste_coup in 
	  let alphab = 
		fun (a,b,score,coup) partie -> 
			let (nou_score,coup_suiv) = alphabeta coup a b (n-1) in
			if estMaximisant joueur 
			then 
				if nou_score > a 
				then ((nou_score,b,nou_score,coup_suiv),false)
				else ((a,b,nou_score,coup_suiv),true)
			else 
				if nou_score < b 
				then ((a,nou_score,nou_score,coup_suiv),false)
				else ((a,b,nou_score,coup_suiv),true)
	  in
	  fold_until alphabeta (fun 
