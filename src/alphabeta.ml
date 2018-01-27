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
	|(_,Sud) -> false 
	|(_,Nord) -> true
	
let rec fold_until fonction_et_test acc liste =
	match liste with 
	|[] -> acc
	|t::q -> let (acc2,booleen) = fonction_et_test acc t in
			 if booleen 
			 then acc2
			 else fold_until fonction_et_test acc2 q;;		 
(*
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
	  fold_until alphabeta (fun *)
	  
	  
let compar jeu j1 j2 = 
  match jeu with 
  |(_,Sud) -> compare (snd j1) (snd j2)
  |(_,Nord) -> compare (snd j2) (snd j1)
             
let fst3 (a,b,c) = a
    
let rec alphabeta n a b j =
  if n = 0 || Gyges.estTerminal j 
  then Gyges.eval_jeu j
  else 
    let lsuiv = Gyges.jeu_suiv j in
    let res = 
      if estMaximisant j
      then
	let v = Gyges.minScore in
	fold_until (maximin (n-1)) (v,a,b) lsuiv
      else
	let v = Gyges.maxScore in
	fold_until (minimax (n-1)) (v,a,b) lsuiv
    in fst3 res
and maximin n (v,a,b) j =
  let v2 = max v (alphabeta n a b j) in
  let a2 = max a v2 in
  ((v,a,b),a2>=b)
and minimax n (v,a,b) j =
  let v2 = min v (alphabeta n a b j) in
  let b2 = min b v2 in
  ((v,a,b),b2<=a)

let recherche_ab jeu n =
  if n=0 || Gyges.estTerminal jeu 
  then jeu
  else
    let lsuiv = Gyges.jeu_suiv jeu in
    let lalphab = List.map (fun j-> (j,alphabeta n Gyges.minScore Gyges.maxScore j)) lsuiv in
    List.sort_uniq (compar jeu) lalphab |> List.hd |> fst
