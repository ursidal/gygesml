let iswinner jeu joueur =
  let liste_coup = depl_possibles jeu joueur in
  let case_gagnante = match joueur with
    |Sud -> CaseNord
    |Nord -> CaseSud
  in
    List.exists (fun c -> c = case_gagnante) liste_coup  

let rec alphabeta (jeu,joueur) alpha beta =
  if iswinner jeu joueur then winscore joueur
  else 
