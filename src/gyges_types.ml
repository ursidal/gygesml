
type joueur =
  | Nord | Sud
;;

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
  |Init of joueur * (pion list)
  |Depl of case * case
;;


type jeu = case*pion list;;
