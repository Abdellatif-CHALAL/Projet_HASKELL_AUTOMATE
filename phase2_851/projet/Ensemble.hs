module Ensemble(Ensemble,ensembleVide,(€),inserer,tailleReelle,taille,insererIntervalle,insererListe,supprimer,supprimerM,union,intersection,foldSet) where
import Data.Foldable

-- Arbre Binaire de Recherche par Intervalles
data Ensemble a = Feuille | Branche (Ensemble a) (a,a) (Ensemble a) deriving (Read,Eq) 

-- Creer un ensemble vide
ensembleVide :: Ensemble a
ensembleVide = Feuille

-- Rechercher une clef dans les intervalles de l'ensemble
(€) :: (Enum a, Ord a) => a -> Ensemble a -> Bool
val €  Feuille = False
val € (Branche gauche (p,q) droite) 
                |((p <= val) && (val <= q)) = True
                | p > val = val € gauche
                | q < val = val € droite
                 
-- Fonctions secondaires

-- renvoie le couple maximal de fils gauche
coupleMaxG ::(Enum a, Ord a) => Ensemble a -> (a,a)
coupleMaxG (Branche gauche (p,q) droite) = if (droite == Feuille) then (p,q)
                                     else coupleMaxG droite

-- renvoie le couple minimal de fils droite
coupleMaxD ::(Enum a, Ord a) => Ensemble a -> (a,a)
coupleMaxD (Branche gauche (p,q) droite) = if (gauche == Feuille) then (p,q)
                                           else coupleMaxD gauche

-- Inseerer une clef en modifiant potentielement les intervalles d'un ensemble
inserer :: (Enum a, Ord a) => Ensemble a -> a -> Ensemble a 
inserer Feuille val = (Branche Feuille (val,val) Feuille)
inserer(Branche gauche (p,q) droite) val
                |(val € (Branche gauche (p,q) droite)  == True) = (Branche gauche (p,q) droite)
                |(pred p == val)&&(gauche == Feuille) = Branche gauche (val,q) droite
                |(pred p == val) = if (coupleQ(coupleMaxG gauche) == pred val) then Branche (supprimerListe gauche [coupleP (coupleMaxG gauche)..coupleQ (coupleMaxG gauche)]) (coupleP (coupleMaxG gauche),q) droite
                                   else Branche gauche (val,q) droite
                |(succ q == val)&&(droite == Feuille) = Branche gauche (p,val) droite
                |(succ q == val) = if (coupleP(coupleMaxD droite) == succ val) then Branche gauche (p,coupleQ (coupleMaxD droite)) ((supprimerListe droite [coupleP (coupleMaxD droite)..coupleQ (coupleMaxD droite)]))
                                    else Branche gauche (p,val) droite
                | pred p > val = (Branche (inserer gauche val) (p,q) droite)
                | succ q < val = (Branche gauche (p,q) (inserer droite val))


-- Imprimer mon ensemble
instance Show a => Show (Ensemble a) where
  show Feuille = "Feuille"
  show (Branche gauche (p,q) droite) = "(Branche "++show gauche ++" "++show (p,q)++" "++show droite++")"

  -- Somme des longueurs de tous les intervalles d'un ensemble
tailleReelle :: Enum a => Ensemble a -> Int
tailleReelle Feuille = 0
tailleReelle (Branche gauche (p,q) droite) = (tailleReelle gauche) + (tailleReelle droite) + succ (fromEnum(q) - fromEnum(p)) 

-- Nombre de noeuds d'un ensemble 
taille :: Ensemble a -> Int
taille Feuille = 0
taille (Branche gauche (p,q) droite) = succ (taille gauche + taille droite)

-- Inserer tout un interval de clefs dans un ensemble 
insererIntervalle :: (Enum a, Ord a) =>
                     Ensemble a -> a -> a -> Ensemble a
insererIntervalle arbi p q = insererListe arbi [p..q]  

-- Insertion d'une liste de clefs dans un ensemble 
insererListe :: (Enum a, Ord a) => Ensemble a -> [a] -> Ensemble a
insererListe arbi [] = arbi 
insererListe abri (x:l) = insererListe (inserer abri x) l

-- renvoie le couple maximal de fils gauche  
maxArb ::(Enum a, Ord a) => Ensemble a -> (a,a)
maxArb (Branche gauche (p,q) droite) = if (droite == Feuille) then (p,q)
                                     else maxArb droite

-- suppression une liste des clés dans un ensemble                                      
supprimerListe :: (Enum a, Ord a) => Ensemble a -> [a] -> Ensemble a
supprimerListe arbi [] = arbi 
supprimerListe arbi (x:l) = supprimerListe (supprimer arbi x) l
                                     
-- focntion qui renvoie la clé gauche de couple (p,q)
coupleP::(Enum a, Ord a) => (a,a) -> a 
coupleP (p,q) = p

-- focntion qui renvoie la clé droite de couple (p,q)
coupleQ::(Enum a, Ord a) => (a,a) -> a 
coupleQ (p,q) = q 

-- suppression d'une clé  
supprimer :: (Enum a, Ord a) => Ensemble a -> a -> Ensemble a
supprimer Feuille val = Feuille
supprimer (Branche gauche (p,q) droite) val
                |(p == val)&&(q == val) = if (gauche /= Feuille) then Branche (supprimerListe gauche [(coupleP (maxArb gauche))..(coupleQ (maxArb gauche))]) (maxArb gauche) droite
                                          else droite
                |(p == val) = (Branche gauche (succ p,q) droite)
                |(q == val) = (Branche gauche (p,pred q) droite)
                |(p < val) && (val < q) = (Branche gauche (p,pred val) (Branche Feuille (succ val,q) droite)) 
                |pred p >= val = (Branche (supprimer gauche val) (p,q) droite)
                |succ q <= val = (Branche gauche (p,q) (supprimer droite val))


--les fonctions auxiliaires

--utilise dans supprimerM 
suprAux ::Maybe (Ensemble a) -> (a,a) -> Ensemble a -> Maybe (Ensemble a)
suprAux Nothing _ _ = Nothing              
suprAux (Just a) (p,q) d = Just (Branche a (p,q) d)

-- utilise dans supprimerM  
supr :: Ensemble a -> (a,a) -> Maybe (Ensemble a) -> Maybe (Ensemble a)
supr  _ _ Nothing = Nothing
supr a (p,q) (Just d) = Just (Branche a (p,q) d)

-- Suppression d'une clef avec resultat Nothing si clef non presente
-- renvoie Nothing si l'élément n'existe pas dans l'ensemble, sinon renvoie Just l'ensemble avec l'élément supprimer   

supprimerM :: (Enum a, Ord a) =>Ensemble a -> a -> Maybe (Ensemble a)
supprimerM Feuille val = Nothing
supprimerM (Branche gauche (p,q) droite) val
                          |(p == val)&&(q == val) = if (gauche /= Feuille) then Just (Branche (supprimerListe gauche [coupleP (maxArb gauche) .. coupleQ (maxArb gauche)]) (maxArb gauche) droite)
                                                     else Just droite
                          |(p == val) = Just (Branche gauche (succ p,q) droite)
                          |(q == val) = Just (Branche gauche (p,pred q) droite)
                          |(p < val) && (val < q) = Just ((Branche gauche (p,pred val) (Branche Feuille (succ val,q) droite))) 
                          |pred p >= val =  suprAux (supprimerM gauche val) (p,q) droite
                          |succ q <= val = supr gauche (p,q) (supprimerM droite val)


-- transfère l'ensemble vers une liste   
abrVersListe:: (Enum a, Ord a) => Ensemble a -> [a]
abrVersListe Feuille = []
abrVersListe (Branche gauche (p,q) droite) = (abrVersListe gauche)++[p..q]++(abrVersListe droite) 

-- union des deux ensembles avec l'aide des listes, en transférant l'ensemble à en liste,
-- et puis on applique insertion la liste a dans l'ensemble b      
union :: (Enum a, Ord a) => Ensemble a -> Ensemble a -> Ensemble a
union a b = insererListe b (abrVersListe a)


-- fonction qui renvoie true si l'élément est appartient dans la liste spécifié sinon false
appartient::(Eq a, Ord a,Enum a)=> a -> [a] -> Bool
appartient x [] = False
appartient x (y:l) = if(x == y) then True else appartient x l

-- intersection de deux listes  
intersectListe :: (Eq a, Ord a,Enum a)=> [a] -> [a] -> [a]
intersectListe [] l = [] 
intersectListe (x:ll) l = if appartient x l then x:(intersectListe ll l)
                          else intersectListe ll l
-- intersection des deux ensembles
intersection :: (Enum a, Ord a) =>
                Ensemble a -> Ensemble a -> Ensemble a
intersection a b = insererListe Feuille (intersectListe (abrVersListe a) (abrVersListe b))


-- parcourt la structure en profondeur comme le foldl parcourt une liste
foldSet ::(Enum a,Ord a) => (b -> a -> b) -> b -> Ensemble a -> b 
foldSet f n Feuille = n
foldSet f n ens = foldl f n (abrVersListe ens)