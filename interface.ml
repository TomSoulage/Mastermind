#load "unix.cma" ;;

module Interface : 
sig 

(**Traduction liste
*Cette fonction traduit une liste d'entiers en liste de pions
*@param liste d'entiers
*@return liste de pion
*)

val traduireListeEnPion : int list -> string list

(**Affichage forme et couleur des pions
*Cette fonction affiche les pions en couleur et avec la forme "●"
*@param couleur
*@return un pion avec une forme et une couleur
*)

val affichageUneCouleur : string -> unit

(**Affichage résultat
*Cette fonction affiche le resultat avec le nombre de pion bien placé et mal placé (dans le cas du mode saisie automatique) ou cache le résultat (mode saisie manuelle)
*@param reponse, est la résulat de nombre biens placés et mal placés
*@param saisieAuto vrai si saisie auto, faux si saisie manuel
*@param humainBool, vrai si c'est au tour de l'humain, faux sinon
*@return le résulat du joueur
*)


val afficherResultat : (int*int) option -> bool -> bool -> unit

(**Affichage plateau
*Cette fonction affiche le plateau de jeu
*@param stockage liste de codes stockés
*@param listeDeResultat: liste des résultats
*@param nb , nombre de tours
*@param humainBool, vrai si c'est le tour de l'humain faux sinon
*@return affiche le plateau
*) 

val afficherPlateau : Code.t list -> (int*int) option list -> int -> bool -> bool -> unit

(**Affichage réponse
*Cette fonction affiche la réponse
*@param stockagePion, liste de pions stockés
*@param stockageReponse, liste de réponses stockés
*@param nb , nombre de tours
*@param saisieAuto , vrai si c'est le mode automatique, faux pour le mode manuel
*@param humainBool, vrai si c'est le tour de l'humain faux sinon
*@return affiche le plateau
*)   

val afficherReponse : Code.t list -> (int*int) option list -> int -> bool -> bool -> unit


(**Affichage de "Mastermind"
*Cette fonction affiche le graphique "Mastermind" au lancement du jeu
*@param ()
*@return affiche "Mastermind"
*)

val masterGraph : 'a -> int 

(**Affichage du mode "PVP" et du mode "PVE"
*Cette fonction affiche les graphiques du mode "PVP" et du mode "PVE" dans le menu du jeu
*@param ()
*@return affiche "PVP" et "PVE"
*)

val pvpPveGraph : 'a -> unit

(**Affichage du mode "KNUTH" et du mode "NAIF"
*Cette fonction affiche les graphiques du mode "KNUTH" et du mode "NAIF" lorsque le joueur joue en mode PVE(contre l'IA)
*@param ()
*@return affiche "KNUTH" et "NAIF"
*)

val knuthNaifGraph : 'a -> unit 

(**Affichage score finale
*Cette fonction affiche  le score des joueurs (que ce soit le mode PVE ou PVP) à la fin d'une partie 
*@param nom_gagnant est le nom de joueur gagnant
*@param nom_perdant est le nom du joueur perdant
*@param score_g est le score de joueur gagnant la partie
*@param score_p est le score de joueur perdant la partie
*@return
*)

val scoreGraph : string -> string -> int -> int -> unit

(**Affichage du code vrai (le code à deviner pour gagner)
*Cette fonction affiche le code vrai
*@param code vrai, le code à deviner
*@return affiche le code vrai
*)

val afficherCodeVrai : Code.t -> unit 

end = 
struct

let traduireListeEnPion liste = List.map (fun x -> traduireNbToPion x) liste ;;

let affichageUneCouleur couleur = 
	match couleur with 
	|"vert"   -> print_string "\027[0;32m\027[40m ● " 
	|"jaune"  -> print_string "\027[1;33m\027[40m ● "
	|"bleu"   -> print_string "\027[0;34m\027[40m ● "	
	|"rouge"  -> print_string "\027[0;31m\027[40m ● "
	|"blanc"  -> print_string "\027[1;37m\027[40m ● "	
	|"rose"   -> print_string "\027[1;35m\027[40m ● "
	| _ 	  -> print_string " " ;;	

let afficherResultat reponse saisieAuto humainBool =  
	match (reponse,saisieAuto, humainBool) with 
	|(_,false,false)    	-> print_string "\027[40m\027[1;34m BP :■" ; print_string "\027[1;37m ┃\027[1;31m MP :■" ; print_string "\027[1;37m┃\027[0m\027[1;37m";
	|(Some(x,y),_,_) 	    -> print_string "\027[40m\027[1;34m BP : " ; print_int x; print_string "\027[1;37m┃\027[1;31m MP :" ; print_int y ; print_string "\027[1;37m┃\027[0m\027[1;37m";
	|(None,_,_) 			-> print_string "Erreur sur l'affichage réponse.";;

let rec afficherPlateauBis stockage listeDeResultat nb saisieAuto humainBool =
    match (stockage,listeDeResultat) with 
    |([],_)       -> ()
    |(t::q,x::h)    -> 
    		let code_vers_string = traduireListeEnPion t in
            print_string "\027[1;37m ";
            print_string "  \027[40m┃ Tour  " ;
            print_int nb ;
            if (nb < 10 ) then print_string "  ┃  " else print_string " ┃  " ;
            affichageUneCouleur (List.nth code_vers_string 0);
            affichageUneCouleur (List.nth code_vers_string 1);
            affichageUneCouleur (List.nth code_vers_string 2);
            affichageUneCouleur (List.nth code_vers_string 3);
            print_string "\027[1;37m ";
            print_string "  ┃";
            afficherResultat x saisieAuto humainBool;
            print_endline" ";
            afficherPlateauBis q h (nb+1) saisieAuto humainBool;
    |_             -> print_string "Erreur sur l'affichage plateau";;	


let rec afficherPlateau stockage listeDeResultat nb saisieAuto humainBool = 
	print_endline"   \027[40m┏━━━━━━━━━━┳━━━━━━━━━━━━━━━━━┳━━━━━━━┳━━━━━━┓\027[0m\027[1;37m";
	afficherPlateauBis stockage listeDeResultat nb saisieAuto humainBool ;
    print_endline"   \027[40m┗━━━━━━━━━━┻━━━━━━━━━━━━━━━━━┻━━━━━━━┻━━━━━━┛\027[0m\027[1;37m";;


let afficherReponse stockagePion stockageReponse nb saisieAuto humainBool = 
	Sys.command "clear";
	afficherPlateau stockagePion stockageReponse 1 saisieAuto humainBool ;
	print_endline " ";
	print_string "Il vous reste ";
	print_int nb;
	print_string " tour(s).";;

let afficherCodeVrai code = 
	let code_vers_string = traduireListeEnPion code in 
	print_string "\027[40m┏━━━━━━━━━━━━┓\027[0m \n";
	print_string "\027[40m┃";
	affichageUneCouleur (List.nth code_vers_string 0);
	affichageUneCouleur (List.nth code_vers_string 1);
	affichageUneCouleur (List.nth code_vers_string 2);
	affichageUneCouleur (List.nth code_vers_string 3);
	print_string "\027[40m\027[1;37m┃\027[0m \n";
	print_string "\027[40m┗━━━━━━━━━━━━┛\027[0m \n";;

let masterGraph t  = 
Sys.command "clear";
print_endline " ";
print_endline"\027[0;31m  ▄▄▄▄███▄▄▄▄      ▄████████    ▄████████     ███        ▄████████    ▄████████   ▄▄▄▄███▄▄▄▄    ▄█  ███▄▄▄▄   ████████▄  ";
print_endline"\027[0;31m ▄██▀▀▀███▀▀▀██▄   ███    ███   ███    ███ ▀█████████▄   ███    ███   ███    ███ ▄██▀▀▀███▀▀▀██▄ ███  ███▀▀▀██▄ ███   ▀███ "; 
print_endline"\027[0;31m ███   ███   ███   ███    ███   ███    █▀     ▀███▀▀██   ███    █▀    ███    ███ ███   ███   ███ ███▌ ███   ███ ███    ███ ";
print_endline"\027[0;31m ███   ███   ███   ███    ███   ███            ███   ▀  ▄███▄▄▄      ▄███▄▄▄▄██▀ ███   ███   ███ ███▌ ███   ███ ███    ███ ";
print_endline"\027[0;31m ███   ███   ███ ▀███████████ ▀███████████     ███     ▀▀███▀▀▀     ▀▀███▀▀▀▀▀   ███   ███   ███ ███▌ ███   ███ ███    ███ ";
print_endline"\027[0;31m ███   ███   ███   ███    ███          ███     ███       ███    █▄  ▀███████████ ███   ███   ███ ███  ███   ███ ███    ███ ";
print_endline"\027[0;31m ███   ███   ███   ███    ███    ▄█    ███     ███       ███    ███   ███    ███ ███   ███   ███ ███  ███   ███ ███   ▄███ ";
print_endline"\027[0;31m  ▀█   ███   █▀    ███    █▀   ▄████████▀     ▄████▀     ██████████   ███    ███  ▀█   ███   █▀  █▀    ▀█   █▀  ████████▀  ";
print_endline"\027[0;31m                                                                      ███    ███                                           ";
print_endline"\027[0m 		       by PETITJEAN Gauthier, SOULAGE Tom, TOURNIER Etienne ";							
print_endline" ";
Unix.sleep(4) ;
Sys.command "clear";;

let pvpPveGraph t = 
print_endline " ";
print_string "\027[0;31m     ██████╗ \027[1;33m ██╗   ██╗ \027[0;31m ██████╗ \027[0m "; print_endline"\027[0;31m       ██████╗ \027[1;33m ██╗   ██╗\027[0;31m ███████╗\027[0m";
print_string "\027[0;31m     ██╔══██╗\027[1;33m ██║   ██║ \027[0;31m ██╔══██╗\027[0m "; print_endline"\027[0;31m       ██╔══██╗\027[1;33m ██║   ██║\027[0;31m ██╔════╝\027[0m";
print_string "\027[0;31m     ██████╔╝\027[1;33m ██║   ██║ \027[0;31m ██████╔╝\027[0m "; print_endline"\027[0;31m       ██████╔╝\027[1;33m ██║   ██║\027[0;31m █████╗  \027[0m";
print_string "\027[0;31m     ██╔═══╝ \027[1;33m ╚██╗ ██╔╝ \027[0;31m ██╔═══╝ \027[0m "; print_endline"\027[0;31m       ██╔═══╝ \027[1;33m ╚██╗ ██╔╝\027[0;31m ██╔══╝  \027[0m";
print_string "\027[0;31m     ██║     \027[1;33m  ╚████╔╝  \027[0;31m ██║     \027[0m "; print_endline"\027[0;31m       ██║     \027[1;33m  ╚████╔╝ \027[0;31m ███████╗\027[0m";
print_string "\027[0;31m     ╚═╝     \027[1;33m   ╚═══╝   \027[0;31m ╚═╝ \027[1;33m Mode \027[0m "; print_endline"\027[0;31m     ╚═╝     \027[1;33m   ╚═══╝  \027[0;31m ╚══════╝\027[1;33m Mode \027[0m ";
print_endline " ";;

let knuthNaifGraph t =
print_endline " ";
print_string" \027[0;34m██╗  ██╗\027[0;33m███╗   ██╗\027[0;34m██╗   ██╗\027[0;33m████████╗\027[0;34m██╗  ██╗\027[0m"; print_endline"\027[0;33m          ███╗   ██╗\027[0;34m █████╗ ██╗\027[0;33m███████╗\027[0m ";
print_string" \027[0;34m██║ ██╔╝\027[0;33m████╗  ██║\027[0;34m██║   ██║\027[0;33m╚══██╔══╝\027[0;34m██║  ██║\027[0m"; print_endline"\027[0;33m          ████╗  ██║\027[0;34m██╔══██╗██║\027[0;33m██╔════╝\027[0m ";
print_string" \027[0;34m█████╔╝ \027[0;33m██╔██╗ ██║\027[0;34m██║   ██║\027[0;33m   ██║   \027[0;34m███████║\027[0m"; print_endline"\027[0;33m          ██╔██╗ ██║\027[0;34m███████║██║\027[0;33m█████╗  \027[0m ";
print_string" \027[0;34m██╔═██╗ \027[0;33m██║╚██╗██║\027[0;34m██║   ██║\027[0;33m   ██║   \027[0;34m██╔══██║\027[0m"; print_endline"\027[0;33m          ██║╚██╗██║\027[0;34m██╔══██║██║\027[0;33m██╔══╝  \027[0m ";
print_string" \027[0;34m██║  ██╗\027[0;33m██║ ╚████║\027[0;34m╚██████╔╝\027[0;33m   ██║   \027[0;34m██║  ██║\027[0m"; print_endline"\027[0;33m          ██║ ╚████║\027[0;34m██║  ██║██║\027[0;33m██║     \027[0m ";
print_string" \027[0;34m╚═╝  ╚═╝\027[0;33m╚═╝  ╚═══╝\027[0;34m ╚═════╝ \027[0;33m   ╚═╝   \027[0;34m╚═╝  ╚═╝\027[1;33m Mode \027[0m "; print_endline"\027[0;33m   ╚═╝  ╚═══╝\027[0;34m╚═╝  ╚═╝╚═╝\027[0;33m╚═╝\027[0;34m Mode \027[0m  ";
print_endline " ";;

let scoreGraph nom_gagnant nom_perdant score_g score_p =
print_endline "\027[0;32m┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓" ;	
print_endline "                             " ;
print_string  "        " ; print_string nom_gagnant; print_string " : "; print_int score_g;
print_endline " \n";
print_string  "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"	;
print_endline" \n" ;
print_endline "\027[0;31m┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓" ;	
print_endline "                             " ;
print_string  "        " ; print_string nom_perdant; print_string " : "; print_int score_p; 
print_endline " \n";
print_string  "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\027[0m\n"	;;

end ;;

open Interface;;

