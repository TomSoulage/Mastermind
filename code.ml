#load "str.cma";;	

	(** Module de definition d'un code dans le jeu Mastermind *)
	module Code :
	sig
	(** Le type d'un pion *)
	type pion = int 
	(** Le type d'un code *)
	type t = pion list 

	(** Nombre de pions par code *)
	val nombre_pions : int

	(** Liste des couleurs possibles *)
	val couleurs_possibles : pion list

	(** Compare deux codes
	* @param code1 premier code a comparer
	* @param code2 second code a comparer
	* @return 0 si les deux codes sont identiques,
	un entier positif si [code1] est strictement plus grand que [code2]
	un entier negatif si [code1] est strictement plus petit que [code2]
	*)
	val compare : t -> t -> int


	(** Conversion code vers chaine de caracteres (pour affichage)
	* @param code code a convertir
	* @return la representation en chaine de caracteres de [code]
	*)
	val string_of_code : t -> string


	(** Conversion chaine de caracteres vers code (pour saisie)
	* @param string chaine de caractere saisie
	* @return le code correspondant a la saisie si la conversion est possible
	[None] si la conversion n'est pas possible
	*)

	val code_of_string : string -> t option 

	
	(** La liste de tous les codes permis *)
	val tous : t list


	(** La liste de toutes les reponses possibles *)
	val toutes_reponses : (int * int) list 

	(** Calcule la reponse d'un code par rapport au code cache
	* @param code le code propose
	* @param vrai_code le code cache
	* @return un couple (nombre de pions bien places, nombre de pions mal places)
	[None] si la reponse ne peut etre calculee
	*) 

	val reponse : t -> t -> (int * int) option 

	  (** traduireNbToPion
	  Traduire un entier en string
    *@param nb entier le pion
    *@return la couleur associée au pion
    *)

	val traduireNbToPion : int -> string  

	(** traduirePionToNb
	  Traduire un string en entier
    *@param pion string le pion à convertir en entier
    *@return le nombre associé à la couleur
    *)

	val traduirePionToNb : string -> int 

	(** saisieValide
	  Vérifier si la saisie joueur est valide
    *@param code Code le code à vérifier
    *@return true si le code est correct
    *)

	val saisieValide : t -> bool

end = 
struct

type pion = int ;;

type t = pion list;;

(** listNthOpt
	  List.nth option
    *@param liste la contenant dont l'élèment est recherchée
    *@param n entier le nième élèment à rechercher
    *@return true si le code est correct
    *)

let rec listNthOpt liste n = 
	match (liste,n) with 
	
	|([],_)   			 -> None  
	|(t::q,0) 			 -> Some t
	|(t::q,n) 			 -> listNthOpt q (n-1);;

let nombre_pions = 4 ;;

let couleurs_possibles = [1;2;3;4;5;6];;

(** compare
	  comparer deux code
    *@param  code1 code à comparer
    *@param  code2 code à comparer
    *@return 1 si les codes sont différents 0 sinon
    *)

let compare code1 code2 = if code1 <> code2 then 1 else 0;;  

let traduireNbToPion nb = 
	match nb with 
	|1 -> "vert"
	|2 -> "jaune"
	|3 -> "bleu"
	|4 -> "rouge"
	|5 -> "blanc"
	|6 -> "rose"
	|_ -> failwith "Erreur sur la traduction couleur" ;;

let traduirePionToNb pion = 
	match pion with 
	|"vert" 	-> 1
	|"jaune"	-> 2 
	|"bleu" 	-> 3
	|"rouge" 	-> 4
	|"blanc" 	-> 5
	|"rose" 	-> 6
	|_ 			-> 0;;	

let rec string_of_code code = 
	match code with 
	|[]      -> ""
	|t::q    -> (traduireNbToPion t) ^ " " ^ (string_of_code q);;

(** tousBis
	  créer la liste de tous les code possibles
    *@param  nb le nombre de pions différents
    *@param  liste la liste des couleurs possibles
    *@return la liste de tous les codes possibles
    *)

let rec tousBis nb liste = 
	match nb with 
	|0  -> [[]]
	|_  -> List.fold_left(fun acc x -> acc@(List.map(fun listeListe -> x::listeListe) (tousBis (nb-1) liste))) [] liste;;

let tous = tousBis nombre_pions couleurs_possibles;;

(** toutes_reponsesBis
	  créer la liste de toutes les réponses possibles
    *@param x entier
    *@param y entier
    *@return la liste de tous les codes (différents des codes possibles)
    *)

let rec toutes_reponsesBis x y =
	match (x,y) with 
	|(4,5) 					  -> []
	|(x,4) when x<>4 		  -> (x,4) :: toutes_reponsesBis (x+1) 0
	|(x,y) when x <=4 && y<=4 -> (x,y) :: toutes_reponsesBis x (y+1)
	|_                        -> [];;

let toutes_reponses = List.filter (fun (x,y) -> x+y <=4) (toutes_reponsesBis 0 0);;

(** positionBis
	  donne la position de l'élèment voulu dans la liste
    *@param liste la liste contenant l'élèment
    *@param el l'élèment dont la position est recherchée
    *@param acc l'accumulateur de la fonction (1 en l'occurence)
    *@return la poisition de l'élèment
    *)

let rec positionBis liste el acc =
	match liste with
	|[] -> (-1)
	|t::q -> if t = el then acc else (positionBis q el (acc+1));;

(** position
	  donne la position de l'élèment voulu dans la liste
    *@param liste la liste contenant l'élèment
    *@param el l'élèment dont la position est recherchée
    *@param acc l'accumulateur de la fonction (1 en l'occurence)
    *)

let position liste el = positionBis liste el 1;;

(** changeListeElPos 
	  change l'élèment à la position demandé en (-1) afin de ne pas avoir de conflit lors du traitements des deux listes
    *@param liste la liste 
    *@param pos la position dont de l'élèment qui doit être modifié
    *@return la liste avec l'élèment étant à la poisition pos modifié en (-1)
    *)

let rec changeListeElPos liste pos = List.map (fun x -> if (position liste x) = pos then (-1) else x) liste;;   

(** nombreBonnePlace
	  renvoie le nombre d'élèment a la bonne place
    *@param codeJoueur le code du joueur
    *@param codeVrai le code secret
    *@param x le nombre d'élèment à la bonne place
    *@return le nombre d'élèment qui sont à la bonne place dans le codeJoueur par rapport au codeVrai
    *)

let rec nombreBonnePlace codeJoueur codeVrai x =
	match (codeJoueur,codeVrai) with 
	|(a::b,c::d) when a = c  -> nombreBonnePlace b d (x+1) 
	|(a::b,c::d) when a <> c -> nombreBonnePlace b d x 
	|(_,_) -> x;;

(** supprimeElBonnePlace
	  supprime les élèments qui sont à la bonne place (bien placé)
    *@param codeJoueur le code du joueur
    *@param codeVrai le code secret
    *@param acc l'accumulateur initialisé à 0
    *@return le nombre d'élèment qui sont à la bonne place dans le codeJoueur par rapport au codeVrai
    *)

let rec supprimeElBonnePlace codeJoueur codeVrai acc = 
	match (listNthOpt codeJoueur acc,listNthOpt codeVrai acc) with
	| (Some(el0),Some(el)) when el0=el  		-> supprimeElBonnePlace codeJoueur codeVrai (acc+1) 
	| (Some(el0),Some(el)) when not (el0=el)    -> el0 :: (supprimeElBonnePlace codeJoueur codeVrai (acc+1))
	| (_,_)							 	        -> [];;

(** nombreBonneCouleurMauvaisePlace
	  renvoie le nombre d'élèment qui sont mal placés
    *@param codeJoueur le code du joueur
    *@param codeVrai le code secret
    *@param x accumulateur initialisé a 0 qui représente le nombre d'élèment à la mauvaise place
    *@param acc l'accumulateur initialisé à 0
    *@return le nombre d'élèment qui sont à la bonne place dans le codeJoueur par rapport au codeVrai
    *)

let rec nombreBonneCouleurMauvaisePlace codeJoueur codeVrai x acc = 
	match listNthOpt codeVrai acc with 
	|None         								  -> x
	|Some(el) when not (List.mem el codeJoueur)   -> nombreBonneCouleurMauvaisePlace codeJoueur codeVrai x (acc+1)
	|Some(el) when (List.mem el codeJoueur)       -> nombreBonneCouleurMauvaisePlace (changeListeElPos codeJoueur (position codeJoueur el)) codeVrai (x+1) (acc+1)
	|_                                            -> x;;  

let reponse codeJoueur codeVrai = Some((nombreBonnePlace codeJoueur codeVrai 0) , nombreBonneCouleurMauvaisePlace (supprimeElBonnePlace codeJoueur codeVrai 0) (supprimeElBonnePlace codeVrai codeJoueur 0) 0 0);;

let rec saisieValide code = List.fold_left (fun acc x -> x <> 0 && acc) true code && (List.exists(fun codeAppartenantATous -> code = codeAppartenantATous) tous);;

let code_of_string code = 
	if saisieValide (List.map(traduirePionToNb)(Str.split(Str.regexp " ") code)) then 
		Some (List.map(traduirePionToNb)(Str.split(Str.regexp " ") code)) 
	else 
		None ;;

end ;;
open Code;;




















