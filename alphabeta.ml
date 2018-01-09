let rec alphabeta (jeu,joueur) alpha beta =
  if iswinner jeu joueur then winscore joueur
  else 
