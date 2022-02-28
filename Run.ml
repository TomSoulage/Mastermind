#use "code.ml";;
#use "interface.ml";;
#use "IA.ml";;
#use "pvp.ml";;

module Run : 
sig 

	(** mastermind
	Lance le jeux mastermind
  *@param nomJoueur string le nom du joueur
  *@param nbTentatives entier le nombre de tentatives par parties
  *@param nbParties entier le nombre de parties
  *@param saisieAuto bool la saisie automatique ou manuelle
  *@return lance le jeux en mode pvp ou pve 
  *)

val mastermind : string -> int -> int -> bool -> unit

end =
struct  

Random.init;;

	(** ListeCodeVrai
	Créer une liste de code aléatoire 
  *@param nb entier qui sera le nombre de partie
  *@return une liste de codeVrai
  *)

let rec listeCodeVrai nb =
	match (nb+2) with 
	|0  -> []
	|_  -> (List.nth tous (Random.int (List.length tous))) :: listeCodeVrai (nb-1);; 

	(** Methode
	Definir une méthode d'algorithme pour l'IA
  *@param nbTentatives entier le nombre de tentatives par parties
  *@param saisieAuto bool la saisie automatique ou manuelle
  *@param nbParties entier le nombre de parties 
  *@param codeSecret Code le code secret de la partie courante
  *@return le lancement de l'IA
  *)

let rec methode nbTentatives saisieAuto nbParties codeSecret = 
	knuthNaifGraph 0 ;
	print_endline " Tapez 1 pour la méthode d'algorithme Knuth.       Tapez 2 pour la méthode d'algorithme naïf. ";
	let reponseI = read_int () in  
	match reponseI with 
	|1 -> knuth    [1;1;2;2] (reponse [1;1;2;2] codeSecret) codeSecret (filtre 1 ([1;1;2;2],reponse [1;1;2;2] codeSecret) tous) [] [] saisieAuto false (nbTentatives-1)
	|2 -> algoNaif [1;1;1;1] (reponse [1;1;1;1] codeSecret) codeSecret (filtre 0 ([1;1;1;1],reponse [1;1;1;1] codeSecret) tous) [] [] saisieAuto false (nbTentatives-1) 
	|_ -> methode nbTentatives saisieAuto nbParties codeSecret;;

	(** runGame
	Lancer la partie contre une IA
  *@param nbTentatives entier le nombre de tentatives par parties
  *@param nbParties entier le nombre de parties
  *@param pointsHumain entier le nombre de point du joueur humain
  *@param pointsIA entier le nombre de points du joueur IA
  *@param saisieAuto bool la saisie automatique ou manuelle
  *@param listeCodeVraiConst Code list la liste de code secret
  *@param nomJoueur string le nom du joueur
  *@return lance une manche joueur ou une manche IA, ou affiche le tableau des scores si la partie est terminée
  *)

let rec runGame nbTentatives nbParties pointsHumain pointsIA saisieAuto listeCodeVraiConst nomJoueur = 
	Unix.sleep(1);
	let codeSecret = List.nth listeCodeVraiConst nbParties in
	if nbParties <> 0 then print_endline "Nouvelle manche ! " else print_endline "La partie est terminée ! ";
	Unix.sleep(1);
	match nbParties with 
	|0  						-> (match (pointsHumain,pointsIA) with 
								|(x,y) when (x > y)  -> print_endline"Vous avez gagné ! Félicitations. " ; scoreGraph nomJoueur "IA" pointsHumain pointsIA;
								|(x,y) when (x < y)  -> print_endline "L'IA a gagné ! Dommage. " ; scoreGraph "IA" nomJoueur pointsIA pointsHumain;
								|(x,y) when (x=y)    -> print_endline "Egalié parfaite ! Vous n'avez ni gagné, ni perdu. " ;
								|_ 					 -> failwith "Erreur calcul de points.");

	|z when ((z mod 2) = 0) 	->  let p = (humainDevine nbTentatives saisieAuto true codeSecret nbParties) in 
									runGame nbTentatives (nbParties-1) (pointsHumain+p) pointsIA saisieAuto listeCodeVraiConst nomJoueur 

	|z when ((z mod 2) <> 0)    -> 	let m = (methode nbTentatives saisieAuto nbParties codeSecret) in
									runGame nbTentatives (nbParties-1) pointsHumain (pointsIA+m) saisieAuto listeCodeVraiConst nomJoueur 
	|_  						-> failwith "Erreur run. ";;  

let rec mastermind nomJoueur nbTentatives nbParties saisieAuto = 
	let listeCodeVraiConst = listeCodeVrai nbParties in
	masterGraph 0 ;
	let nbParties1 = if (nbParties mod 2) <> 0 then nbParties +1 else nbParties in
	print_string "\nBienvenue dans le mastermind "; print_string nomJoueur ; print_string " !\n";
	Unix.sleep 1;
	pvpPveGraph 0 ;
	print_string "\n1 : Jouer avec contre un ami (PVP) "; print_string "    2 : Mode de jeux contre une IA (PVE)\n\n\n"; print_endline "\n0 : Exit";
	let pvpOuPvIA = read_int () in 
	match pvpOuPvIA with 
	|1 -> print_endline "Quel est le nom de l'autre joueur ?" ; let nomJoueur2 = read_line() in
		  print_endline "Le joueur ayant lancé la partie commence. Bonne chance ! "; pvp nomJoueur nomJoueur2 nbTentatives nbParties1 0 0 listeCodeVraiConst
	|2 -> print_string nomJoueur ; print_string " commence la partie. "; runGame nbTentatives nbParties1 0 0 saisieAuto listeCodeVraiConst nomJoueur
	|0 -> print_endline "Aurevoir. ";
	|_ -> mastermind nomJoueur nbTentatives nbParties1 saisieAuto ;;

end;; 

open Run;;