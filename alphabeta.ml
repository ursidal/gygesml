let iswinner liste_coup joueur =
  let case_gagnante = match joueur with
    |Sud -> CaseNord
    |Nord -> CaseSud
  in
    List.exists (fun c -> c = case_gagnante) liste_coup  

let winscore = function
  |Sud -> -100
  |Nord -> 100

let rec alphabeta (jeu,joueur) alpha beta =
  let liste_coup = depl_possibles jeu joueur in
  if iswinner liste_coup joueur then winscore joueur
  else 
