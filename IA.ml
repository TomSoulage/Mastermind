#use "code.ml";;
 #load "unix.cma";;
 #use "interface.ml";;

(** Algorithmes de recherche de code *) 
module IA :
sig

(** Nombre d'algorithmes developpes *)

val nombre_methodes : int

(** Choisit un code a proposer
	* @param methode 0 pour l ' algorithme naif,
					 1 pour l ' algorithme de KNUTH
	*
					... et ainsi de suite
	*
	* @param  essais		la	liste des codes deja proposes
	* @param  possibles 	la liste des codes possibles
	* @return le prochain code a essayer
*)

val choix : int -> Code.t list -> Code.t list -> Code.t

(** Filtre les codes possibles
	* @param methode 0 pour l ' algorithme naif,
					 1 pour l ' algorithme de KNUTH
	*
	... et ainsi de suite
	*
	@param (code,rep)  le code essaye et la reponse correspondante
	@param possibles   la liste courante de codes possibles
 	@return la nouvelle liste de codes possibles
*)

val filtre : int -> (Code.t * (int * int) option) -> Code.t list -> Code.t list

(**Humain doit deviner le code 
*Cette fonction utilise la fonction humainDevineBis permettant à l'utilisateur de deviner le code secret
*@param nombre de tentatives pour deviner le code vrai
*@param saisieAuto mode de saisie ( vrai si saisie auto, faux si saisie manuelle)
*@param humainBool vrai si c'est au tour de l'humain, faux sinon
*@param codeSecret code à deviner
*@param nbParties le nombre de parties
*@return le resultat de l'utilisateur (s'il a gagné ou non )
*)	

val humainDevine : int -> bool -> bool -> Code.t -> int -> int

(**Algo naif
*Cette fonction réalise l'algorithme naif 
*@param code rentré par l'utilisateur
*@param rep est le nombre de pion bien placé et mal placé 
*@param codeVrai est le code secret à deviner 
*@param ensembleCode est l'ensemble des codes dans lesquels sont contenus les codes à tester
*@param listeCodeJoue est la liste des codes joués 
*@param listeDeResultat est la liste des résultats obtenus à chaque tour
*@param saisieAuto mode de saisie ( vrai si saisie auto, faux si saisie manuelle)
*@param humainBool vrai si c'est au tour de l'humain, faux sinon
*@param nbTentatives est le nombre de tours réaliser par l'algo pour trouver le code secret
*@return lance le mode naif
*)	

val algoNaif : Code.t -> (int*int) option -> Code.t -> Code.t list -> Code.t list -> ((int*int) option) list -> bool -> bool -> int -> int  

(**Algo de Knuth 
*Cette fonction réalise l'algorithme de KNUTH
*@param code rentré par l'utilisateur
*@param rep est le nombre de pion bien placé et mal placé 
*@param codeVrai est le code secret à deviner 
*@param ensembleCode est l'ensemble des codes dans lesquels sont contenus les codes à tester
*@param listeCodeJoue est la liste des codes joués 
*@param listeDeResultat est la liste des résultats obtenus à chaque tour
*@param saisieAuto mode de saisie ( vrai si saisie auto, faux si saisie manuelle)
*@param humainBool vrai si c'est au tour de l'humain, faux sinon
*@param nbTentatives est le nombre de tours réaliser par l'algo pour trouver le code secret
*@return lance le mode knuth
*)	

val knuth : Code.t -> (int*int) option -> Code.t -> Code.t list -> Code.t list -> ((int*int) option) list -> bool -> bool -> int -> int 

end =
struct

let nombre_methodes = 2 ;;

let traduireListeListe listeofliste = List.map (fun liste -> traduireListeEnPion liste) listeofliste;;

let saisieValide_2  liste = List.exists (fun x -> x = liste) (traduireListeListe tous);;

(************************************* HUMAIN DEVINE ****************************************)

(** Stocker des réponses
*Cette fonction stocke les réponses en queue
*@param élémént à stocker
*@param liste
*@return la liste avec les éléments en queue
*)	
let rec stockerReponse el liste  =
	match liste with 
	|[]   -> [el]
	|t::q -> t :: stockerReponse el q ;;


(**Demande le code 
*Cette fonction demande le code à l'utilisateur de rentrer un code
*@param () unit
*@return récupère le code rentré par l'utilisateur s'il est valide, sinon renvoie un message d'erreur  
*)	
let rec demanderCode () = 
	print_endline "Veuillez entrer un code : " ;
	let code =   code_of_string (String.lowercase (read_line())) in 
	match code with 
	|Some(el)      -> el
	|None          -> print_endline "Ce code n'est pas correct... Entrez en un nouveau : " ; demanderCode ();; 

(**Demande le nombre de pion bien et mals placé 
*Cette fonction demande à l'utilisateur qui doit faire deviner le code secret, le nombre de pions bien placé et mals placé
*@param () unit
*@return recupère les données rentrées par l'utilisateur, si l'utilisateur s'est trompé renvoie un message d'erreur  
*)	

let rec demanderPionMPBP () = 
	print_endline "Veuillez entrer le nombre de pion(s) bien placé(s).";
	let bienPlace = read_int() in 
	print_endline "Veuillez entrer le nombre de pion(s) mal placé(s)." ;
	let malPlace = read_int () in 
	match (bienPlace,malPlace) with 
	|(x,y)  when (List.exists(fun rep -> (x,y) = rep) toutes_reponses) -> (x,y) 
	|_ 									 							   -> print_string "Ceci n'est pas possible...Recommencez. \n" ; demanderPionMPBP () ;;

(**Demande les pions bien et mals placé (utilisateur qui doit deviner le code secret)
*Cette fonction demande à l'utilisateur qui doit deviner le code secret de saisir combien il y a de pion bien placé et mal placé
*@param reponseParam est un type de réponse Some(x,y) 
*@param saisieAuto mode de saisie ( vrai si saisie auto, faux si saisie manuelle)
*@param codeSecret est le code à deviner
*@return recupère les données rentrées par l'utilisateur, si l'utilisateur s'est trompé renvoie un message d'erreur  
*)	

let rec demanderReponse reponseParam saisieAuto codeSecret =
	print_endline " " ;
	match saisieAuto with 
	|false -> print_endline "Voici le code secret : ";
			print_endline " ";
			afficherCodeVrai codeSecret ; 
			let bpmp = demanderPionMPBP () in 
			reponseParam = Some(fst bpmp, snd bpmp);
	|true -> true ;;   

	
(**Humain doit deviner le code
*Cette fonction permet à un utilisateur de deviner le code secret
*@param nombre de tentatives pour deviner le code vrai
*@param codeVrai est le code secret à deviner 
*@param listeCode est la liste de codes possibles
*@param listeReponse est la liste de réponses possibles
*@param saisieAuto mode de saisie ( vrai si saisie auto, faux si saisie manuelle)
*@param humainBool vrai si c'est au tour de l'humain, faux sinon
*@return le resultat de l'utilisateur (s'il a gagné ou non )
*)	
let rec humainDevineBis nb codeVrai listeCode listeReponse saisieAuto humainBool  =
	print_endline " ";
	let codeJoueurNb = demanderCode () in 
	let stockagePion = stockerReponse codeJoueurNb listeCode in
	let reponseVar = reponse codeJoueurNb codeVrai in
	let stockageReponse = stockerReponse reponseVar listeReponse in
	afficherReponse stockagePion stockageReponse (nb-1) saisieAuto humainBool;
	let winLoose = reponseVar = Some(4,0) in 
		print_endline " ";
		match (winLoose,nb) with 
		|(true,_)   -> print_endline "Vous avez gagné !"; (nb-1)
		|(_,1)      -> print_endline "Vous avez perdu !"; print_endline "La bonne réponse était : " ; afficherCodeVrai codeVrai;  0
		|(false,_) 	-> humainDevineBis (nb-1) codeVrai stockagePion stockageReponse saisieAuto humainBool  ;;

let humainDevine nbTentatives saisieAuto humainBool codeSecret nbParties  = 
	humainDevineBis nbTentatives codeSecret [] [] saisieAuto humainBool ;;

(*********************************** IMPLEMENTATION **************************************)

(** AlgoFiltre
	Algorithme de filtre  
  *@param codeTest le code à tester
  *@param reponseParam une réponse type Some(x,y)
  *@param ensembleCode l'ensemble dans le quel sont stockés les code qui reste
  *@return un nouvel ensemble restreint
  *)

let algoFiltre codeTest reponseParam ensembleCode = List.filter(fun codeTous -> reponseParam = reponse codeTous codeTest) ensembleCode ;; 

(** listNormalToListOpt
	Transforme une liste en liste option
  *@param liste
  *@return une liste option
  *)

let listNormalToListOpt liste = List.fold_left(fun acc x -> Some(x) :: acc) [] liste ;;	

(** knuthAlgo
	Pour un code donné, concatène la taille des nouvels ensembles issus de l'algoritme de filtre  
  *@param code le code à tester
  *@param listeReponse la liste de toutes les réponses possibles
  *@param ensembleCode l'ensemble dans les quels sont contenus les codes à tester
  *@return renvoie une liste de taille de liste
  *)

let knuthAlgo code listeReponse ensembleCode = List.fold_left (fun acc x -> (List.length (algoFiltre code x ensembleCode)) :: acc) [] listeReponse;;

(** supprimerCodeQueue
	supprimer le dernier code contenu dans une liste, dans une liste
  *@param listeCodeAEnlever la liste dont la tête est l'élèment à supprimer dans l'autre liste
  *@param ensembleCode la liste dont un code doit être enlever
  *@return renvoie une liste sans l'élèment décris en premier paramètre
  *)

let supprimerCodeQueue listeCodeAEnlever ensembleCode = List.fold_left (fun acc x -> if x = (List.nth listeCodeAEnlever ((List.length listeCodeAEnlever)-1)) then acc else acc@[x]) [] ensembleCode;;

(** tailleMaxListe
	déterminer le maximum d'une liste d'entier
  *@param liste la liste de taille 
  *@return renvoie le plus grand élèment entier de la liste
  *)

let tailleMaxListe liste = List.fold_left (fun acc el -> if el > acc then el else acc) 0 liste ;;

(** tailleMinCouple
	déterminer l'élèment dont la taille est la plus petite
  *@param listeCouple la liste de tous les couples (code,tailleMax)
  *@return renvoie l'élèment dont le score est minimal
  *)

let tailleMinCouple listeCouple = List.fold_left (fun acc el -> if (snd el) < (snd acc) then el else acc) ([],50000)  listeCouple ;;

(** scoreKnuth
	faire la liste (code,taillemax)
  *@param ensembleCode l'ensemble des codes restants
  *@return renvoie la liste (code,tailleMax)
  *)

let scoreKnuth ensembleCode = List.fold_left (fun acc el -> (el,tailleMaxListe (knuthAlgo el (listNormalToListOpt toutes_reponses) ensembleCode)) :: acc ) [] ensembleCode ;;

let choix methode essais possibles = 
	match methode with 
	|0        -> List.hd possibles
	|1        -> fst (tailleMinCouple (scoreKnuth (supprimerCodeQueue essais possibles)))
	|_        -> failwith "Erreur sur la prise du nouveau code.";;

let filtre methode codeRep possibles =
	match methode with 
	|0        -> algoFiltre (fst codeRep) (snd codeRep) possibles
	|1		  -> algoFiltre (fst codeRep) (snd codeRep) possibles
	|_        -> failwith "Erreur sur l'algorithme de réduction d'ensemble.";;

(*************************************** KNUTH ******************************************)


let rec knuth code rep codeVrai ensembleCode listeCodeJoue listeDeResultat saisieAuto humainBool nbTentatives  =
	Sys.command "clear";
	let stockageCode = stockerReponse code listeCodeJoue in 
	let stockageRes = stockerReponse rep listeDeResultat in 
	afficherPlateau stockageCode stockageRes 1 saisieAuto humainBool ;
	let demanderRep = demanderReponse rep saisieAuto codeVrai in 
	match (nbTentatives,rep,demanderRep) with
	|(_,_,false) 		 -> print_string "Raté !\n"; nbTentatives
	|(_,Some(4,0),_)     -> nbTentatives
	|(0,_,_)     		 -> 0
	|_		   		     -> let x = (choix 1 stockageCode ensembleCode) in 
								 knuth x (reponse x codeVrai) codeVrai (filtre 1 (x,reponse x codeVrai) ensembleCode) stockageCode stockageRes saisieAuto humainBool (nbTentatives-1) ;;

(************************************* NAIF ****************************************)



let rec algoNaif code rep codeVrai ensembleCode listeCodeJoue listeDeResultat saisieAuto humainBool nbTentatives = 
	Sys.command "clear";
	let stockageCode = stockerReponse code listeCodeJoue in 
	let stockageRes = stockerReponse rep listeDeResultat in 
	afficherPlateau stockageCode stockageRes 1 saisieAuto humainBool;
	let demanderRep = demanderReponse rep saisieAuto codeVrai in 
	match (nbTentatives,rep,demanderRep) with
	|(_,_,false) 		 -> print_string "Raté ! \n"; nbTentatives
	|(_,Some(4,0),_)     -> nbTentatives
	|(0,_,_)     		 -> 0
	|_		   			 -> let x = (choix 0 [[]] ensembleCode) in 
								algoNaif x (reponse x codeVrai) codeVrai (filtre 0 (x,reponse x codeVrai) ensembleCode) stockageCode stockageRes saisieAuto humainBool (nbTentatives-1) ;;



end;;
open Code;;
open IA;;
