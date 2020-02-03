
module Dictionnaire(Dictionnaire,creer,rechercherCle,inserer,insererM,tailleReelle,taille,insererIntervalle,insererIntervalleM,insererListe,insererListeM,supprimer,supprimerM,rechercherParCle,mapDico,filter) where
 
-- Arbre Binaire de Recherche par Intervalles
data Dictionnaire cle val  = Feuille |  Branche (Dictionnaire cle val) ((cle,cle),val) (Dictionnaire cle val) deriving (Read,Eq)




-- Creer un dictionnaire vide
creer :: Dictionnaire cle val
creer = Feuille

-- Rechercher une clef dans les intervalles de dictionnaire
rechercherCle :: (Ord cle,Enum cle)=> Dictionnaire cle val -> cle -> Bool
rechercherCle Feuille cle = False 
rechercherCle (Branche gauche ((p,q),val) droite)  cle
                | ((p <= cle) && (cle <= q)) = True
                | p > cle = rechercherCle gauche cle
                | q < cle = rechercherCle droite cle

-- Fonctions secondaires

-- renvoie le couple cle val maximal de fils gauche
coupleMaxG :: (Enum cle,Eq val,Ord cle)=> Dictionnaire cle val -> ((cle,cle),val) 
coupleMaxG (Branche gauche ((p,q),val) droite) = if (droite == Feuille) then ((p,q),val)
                                     else coupleMaxG droite

-- renvoie le couple cle val minimal de fils droite
coupleMaxD :: (Enum cle,Eq val,Ord cle)=> Dictionnaire cle val -> ((cle,cle),val)
coupleMaxD (Branche gauche ((p,q),val) droite) = if (gauche == Feuille) then ((p,q),val)
                                                  else coupleMaxD gauche
												 
-- focntion qui renvoie la clé droite de couple ((cle,cle),val)
coupleQ::(Enum cle,Eq val,Ord cle) => ((cle,cle),val) -> cle 
coupleQ ((p,q),val) = q

-- focntion qui renvoie la clé gauche de couple ((cle,cle),val)
coupleP::(Enum cle,Eq val,Ord cle) => ((cle,cle),val) -> cle 
coupleP ((p,q),val) = p
										   
-- focntion qui renvoie la valeur de couple ((cle,cle),val)
coupleVal::(Enum cle,Eq val,Ord cle) => ((cle,cle),val) -> val 
coupleVal ((p,q),val) = val
						
-- focntion qui renvoie le couple ((cle,cle),val)
couple::(Enum cle,Eq val,Ord cle) => Dictionnaire cle val -> ((cle,cle),val) 
couple (Branche gauche ((p,q),val) droite) = ((p,q),val) 

-- suppression une liste des clés dans un dictionnaire
supprimerListe :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> [cle] -> Dictionnaire cle val
supprimerListe dic [] = dic 
supprimerListe dic (cle:l) = supprimerListe (supprimer dic cle) l


-- Inserer une clef avec son contexte en modifiant potentielement les intervalles
-- Non definit si conflit de valeur						
inserer :: (Ord cle, Enum cle, Eq val) =>
        Dictionnaire cle val -> cle -> val -> Dictionnaire cle val
inserer Feuille cle val = (Branche Feuille ((cle,cle),val) Feuille)
inserer (Branche gauche ((p,q),valeur) droite) cle val
						|(rechercherCle(Branche gauche ((p,q),valeur) droite) cle  == True) = (Branche gauche ((p,q),valeur) droite)
						|(pred p == cle)&&(gauche == Feuille) = if(valeur == val) then Branche gauche ((cle,q),val) droite
						                                        else Branche (inserer gauche cle val) ((p,q),valeur) droite
						|(pred p == cle) = if(coupleQ(coupleMaxG gauche) == pred cle)&&(coupleVal(coupleMaxG gauche) == val)&&(valeur == val) then Branche (supprimerListe gauche [coupleP(coupleMaxG gauche)..coupleQ(coupleMaxG gauche)]) ((coupleP(coupleMaxG gauche),q),val) droite
										   else if(valeur == val) then Branche gauche ((cle,q),val) droite
						                   else Branche (inserer gauche cle val) ((p,q),valeur) droite
						|(succ q == cle)&&(droite == Feuille) = if(valeur == val) then Branche gauche ((p,cle),val) droite
						                                        else Branche gauche ((p,q),valeur) (inserer droite cle val) 
						|(succ p == cle) = if (coupleP(coupleMaxD droite) == succ cle)&&(coupleVal(coupleMaxD droite) == val)&&(valeur == val) then Branche gauche ((p,coupleQ(coupleMaxD droite)),val) (supprimerListe droite [coupleP(coupleMaxD droite)..coupleQ(coupleMaxD droite)]) 
										   else if (valeur == val) then Branche gauche ((p,cle),val) droite
						                   else Branche gauche ((p,q),valeur) (inserer droite cle val)
						| pred p > cle = (Branche (inserer gauche cle val) ((p,q),valeur) droite)
						| succ q < cle = (Branche gauche ((p,q),valeur) (inserer droite cle val))

-- Insersion qui retourne Nothing si conflit de valeur
insererM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> Maybe (Dictionnaire cle val)
insererM dic cle val
			|(rechercherCle dic cle) = if((rechercherParCle dic cle) == Just val) then Just (dic) 
                                       else Nothing
			|otherwise = Just(inserer dic cle val)



-- Imprimer mon dictionnaire 
instance (Show cle, Show val) => Show (Dictionnaire cle val) where
  show Feuille = "Feuille"
  show (Branche gauche ((p,q),val) droite) = "(Branche "++show gauche ++" "++show ((p,q),val)++" "++show droite++")"

-- Somme des longueurs de tous les intervallesval de dictionnaire 
tailleReelle :: (Enum cle) =>
	        Dictionnaire cle val -> Int
tailleReelle Feuille = 0
tailleReelle (Branche gauche ((p,q),valeur) droite) = tailleReelle gauche + tailleReelle droite + succ (fromEnum(q) - fromEnum(p)) 

-- Nombre de noeuds de dictionnaire
taille :: Dictionnaire cle val -> Int
taille Feuille = 0
taille (Branche gauche ((p,q),val) droite) = succ (taille gauche + taille droite)


-- Inserer tout un interval de clefs pour un context, peut renvoyer Nothing en cas de conflits sur les contenus
insererIntervalle :: (Ord cle, Enum cle, Eq val) =>
	             Dictionnaire cle val -> cle -> cle -> val -> Dictionnaire cle val
insererIntervalle Feuille cle1 cle2 val = Branche Feuille ((cle1,cle2),val) Feuille
insererIntervalle dic cle1 cle2 val = insererListe dic (associeVal val [cle1..cle2]) 

-- une fonction qui associe la valeur val à toutes les clés de la liste 
associeVal:: b -> [a] -> [(a,b)]
associeVal val [] = []
associeVal val (x:l) = (x,val):(associeVal val l) 

-- Version insertion un intervalle de clés sur un dictionnaire avec Maybe
insererIntervalleM :: (Ord cle, Enum cle, Eq val) =>
	             Dictionnaire cle val -> cle -> cle -> val -> Maybe (Dictionnaire cle val)
insererIntervalleM dic cle1 cle2 val = insererListeM dic (associeVal val [cle1..cle2])
					   

-- Insertion d'une liste de clefs et contexts
insererListe :: (Ord cle, Enum cle, Eq val) =>
	        Dictionnaire cle val -> [(cle,val)] -> Dictionnaire cle val
insererListe dic [] = dic 
insererListe dic ((cle,val):l) = insererListe (inserer dic cle val) l
									   
-- Version inserestion une listes de clés sur un dictionnaire avec Maybe
insererListeM :: (Ord cle, Enum cle, Eq val) =>
	        Dictionnaire cle val -> [(cle,val)] -> Maybe (Dictionnaire cle val)
insererListeM dic [] = Just dic
insererListeM dic ((cle,val):l)  
				|(rechercherCle dic cle) = if(rechercherParCle dic cle == Just val) then insererListeM dic l
                                                                                       else Nothing
                |otherwise = insererListeM (inserer dic cle val) l 

-- Suppression d'une clef dans un dictionnaire 
supprimer :: (Ord cle, Enum cle, Eq val) =>
	     Dictionnaire cle val -> cle -> Dictionnaire cle val
supprimer Feuille cle = Feuille
supprimer (Branche gauche ((p,q),valeur) droite) cle
						 |(p == cle)&&(q == cle) = if (gauche /= Feuille) then Branche (supprimerListe gauche [(coupleP (coupleMaxG gauche))..(coupleQ (coupleMaxG gauche))]) (coupleMaxG gauche) droite
												   else droite
						 |(p == cle) = (Branche gauche ((succ p,q),valeur) droite)
						 |(q == cle) = (Branche gauche ((p,pred q),valeur) droite)
						 |(p < cle) && (cle < q) = (Branche gauche ((p,pred cle),valeur) (Branche Feuille ((succ cle,q),valeur) droite)) 
						 |pred p >= cle = (Branche (supprimer gauche cle) ((p,q),valeur) droite)
						 |succ q <= cle = (Branche gauche ((p,q),valeur) (supprimer droite cle))


-- les fonctions auxiliaires utiliser dans la fonction supprimerM
suprAux ::Maybe (Dictionnaire cle val) -> ((cle,cle),val) -> Dictionnaire cle val -> Maybe (Dictionnaire cle val)
suprAux Nothing _ _ = Nothing              
suprAux (Just a) (p,q) d = Just (Branche a (p,q) d)


supr :: (Dictionnaire cle val) -> ((cle,cle),val) -> Maybe (Dictionnaire cle val) -> Maybe (Dictionnaire cle val)
supr  _ _ Nothing = Nothing
supr a (p,q) (Just d) = Just (Branche a (p,q) d)


-- Suppression d'une clef avec resultat Nothing si clef non presente
supprimerM :: (Ord cle, Enum cle, Eq val) =>
	      Dictionnaire cle val -> cle -> Maybe (Dictionnaire cle val)
supprimerM Feuille val = Nothing
supprimerM (Branche gauche ((p,q),valeur) droite) cle
                          |(p == cle)&&(q == cle) = if (gauche /= Feuille) then Just (Branche (supprimerListe gauche [coupleP(coupleMaxG gauche) .. coupleQ(coupleMaxG gauche)]) (coupleMaxG gauche) droite)
                                                     else Just droite
                          |(p == cle) = Just (Branche gauche ((succ p,q),valeur) droite)
                          |(q == cle) = Just (Branche gauche ((p,pred q),valeur) droite)
                          |(p < cle) && (cle < q) = Just ((Branche gauche ((p,pred cle),valeur) (Branche Feuille ((succ cle,q),valeur) droite))) 
                          |pred p >= cle =  suprAux (supprimerM gauche cle) ((p,q),valeur) droite
                          |succ q <= cle = supr gauche ((p,q),valeur) (supprimerM droite cle)



-- Rechercher une clef et rendre le contenu trouve, rend Nothing si non trouve
rechercherParCle :: Ord cle =>
	            Dictionnaire cle val -> cle -> Maybe val 
rechercherParCle Feuille cle = Nothing
rechercherParCle (Branche gauche ((p,q),val) droite)  cle
                 | ((p <= cle) && (cle <= q)) = Just val
                 | p > cle = (rechercherParCle gauche cle) 
                 | q < cle = (rechercherParCle droite cle)

-- modifier tous les contenus d'un dictionnaire
mapDico :: (Ord cle, Enum cle, Eq a, Eq b) =>
	   (a->b) ->  Dictionnaire cle a -> Dictionnaire cle b
mapDico f Feuille = Feuille
mapDico f (Branche gauche ((p,q),val) droite) = (Branche (mapDico f gauche) ((p,q),f val) (mapDico f droite))  
-- filtrer les contenus d'un dictionnaire
filtrer :: (Ord cle, Enum cle, Eq val) =>
	   (val->Bool) ->  Dictionnaire cle val -> Dictionnaire cle val
filtrer f Feuille = Feuille
filtrer f (Branche gauche ((p,q),val) droite) = if (f val == False) then  supprimerListe (Branche (filtrer f gauche) ((p,q),val) (filtrer f droite)) [p..q]
                                                else (Branche (filtrer f gauche) ((p,q),val) (filtrer f droite))