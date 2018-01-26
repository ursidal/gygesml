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
	|(_,Sud) -> (snd j2) - (snd j1)
	|(_,Nord) -> (snd j1) - (snd j2)

	  
let recherche_ab jeu n =
	if n=0 || estTerminal jeu 
	then eval_jeu jeu
	else
		let lsuiv = jeu_suiv jeu in
		let lalphab = List.map (fun j-> (j,alphabeta n minScore maxScore j)) lsuiv in
		List.sort (compar jeu) lalphab |> List.hd |> fst

let rec alphabeta n a b j =
	if n = 0 || estTerminal j 
	then eval_jeu j
	else 
		let lsuiv = jeu_suiv j in
		let res = 
			if estMaximisant j
			then
				let v = minScore in
				fold_until maximin (v,a,b) lsuiv
			else
				let v = maxScore in
				fold_until minimax (v,a,b) lsuiv
		in fst3 res
and maximin (v,a,b) j =
	let v2 = max v (alphabeta (n-1) a b j) in
	let a2 = max a v2 in
	((v,a,b),a2>=b)
and minimax (v,a,b) j =
	let v2 = min v (alphabeta (n-1) a b j) in
	let b2 = min b v2 in
	((v,a,b),b2<=a)
