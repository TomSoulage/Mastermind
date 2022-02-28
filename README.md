(2018) Mastermind ( OCaml ) : two game modes: against a friend or against an AI  (  using the Knuth algorithm or the Naive algorithm ) with differents levels of difficulty.

Pour lancer le jeu, il suffit d'effectuer les manipulations suivantes : 

-> #use "Run.ml";;
-> mastermind "Nom_du_joueur" nombre_de_tentatives nombres_de_parties saisieAuto_ou_saisieManuelle en respectant : string -> int -> int -> bool 

Afin de profiter pleinement de l'interface graphique, il est préfèrable de lancer le jeu en mode plein écran.
Le nombre de points est calculé en fonction du nombre de tours nécessaires au joueur afin de trouver le vrai code. Exemple, si le joueur trouve le code en 3 coups et qu'il 
disposait de 10 tentatives, il remportera 7 points !

Les paramètres nombre_de_couleur et nombre_de_pion sont fixés à l'avance et ne peuvent être modifiés,
comme dans le jeu Mastermind classique c'est à dire 4 couleurs et 6 pions.

Il est possible de jouer contre un amis, ou contre un IA avec différents niveaux de difficulté. 


		
