#use "IA.ml";;
module PlayerVsPlayer : 
sig 

(**Mode PVP
*Cette fonction est le mode PVP : humain contre humain
*@param nom du premier joueur
*@param nom du deuxieme joueur
*@param nombres de tours choisis par les joueurs
*@param nombres de parties choisies par les joueurs
*@param point du premier joueur
*@param point du deuxieme joueur
*@param liste de codes secrets
*@return lance le mode pvp
*)    

val pvp : string -> string -> int -> int -> int -> int -> Code.t list -> unit

end = 
struct

let rec pvp nomJoueur1 nomJoueur2 nbTentatives nbParties pointsJoueur1 pointsJoueur2 listeCodeVraiConst =
	Unix.sleep 2;
	let codeSecret = List.nth listeCodeVraiConst nbParties in 
    if (nbParties <> 0) then 
    print_endline "Nouvelle manche ! " 
	else 
	print_endline "Partie terminée !";
    Unix.sleep 1;
    match nbParties with 
    |0 					  -> (match (pointsJoueur1,pointsJoueur2) with 
    						|(a,b) when a < b -> print_string nomJoueur2 ; print_string " a gagné la partie ! Félicitations \n"; scoreGraph nomJoueur2 nomJoueur1 pointsJoueur2 pointsJoueur1;
    						|(a,b) when a > b -> print_string nomJoueur1 ; print_string " a gagné la partie ! Félicitations. \n"; scoreGraph nomJoueur1 nomJoueur2 pointsJoueur1 pointsJoueur2;
    						|_				  -> print_endline "Egalité parfaite ! Personne n'a gagné, ni perdu.  ")
    |x when (x mod 2) = 0 -> print_string "\nC'est au tour de " ; print_string nomJoueur1 ; print_string " de jouer. " ;
                            let p = (humainDevine nbTentatives true true codeSecret nbParties) in  
    						pvp nomJoueur1 nomJoueur2 nbTentatives (nbParties-1) (pointsJoueur1+p) pointsJoueur2 listeCodeVraiConst
    |x when (x mod 2) <> 0 -> print_string "\nC'est au tour de " ; print_string nomJoueur2 ; print_string " de jouer. " ;
                            let m = (humainDevine nbTentatives true true codeSecret nbParties) in 
    						pvp nomJoueur1 nomJoueur2 nbTentatives (nbParties-1) pointsJoueur1 (pointsJoueur2+m) listeCodeVraiConst
    |_ 						-> print_string "Erreur pvp.";;

end;;
open PlayerVsPlayer;;