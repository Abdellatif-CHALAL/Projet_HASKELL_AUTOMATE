module Dictionnaire (Dictionnaire,creer,toSet,rechercherCle,inserer,insererM,tailleReelle,taille,insererIntervalle,insererIntervalleM,insererListe,insererListeM,supprimer,supprimerM,rechercherParCle,mapDico,filter,toListCleVal,toListVal,unionCons,compresser,insererIntervalleCons) where

 
-- Arbre Binaire de Recherche par Intervalles
data Dictionnaire cle val  = Noeud [cle] val (Dictionnaire cle val) (Dictionnaire cle val) | Vide 
-- Creer un arbre vide
creer :: Dictionnaire cle val
creer = Vide

-- Rechercher une clef dans les intervalles de l'arbre
rechercherCle :: Ord cle => Dictionnaire cle val -> cle -> Bool
rechercherCle Vide x = False
rechercherCle (Noeud c v a b) x = if (x >= head c) && (x <= last c) then True 
                                  else if x < head c then rechercherCle a x
                                  else rechercherCle b x

-- Inserer une clef avec son contexte en modifiant potentielement les intervalles
-- Non definit si conflit de valeur
trouver ::(Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> [cle]
trouver Vide c v = []
trouver (Noeud c1 v1 a b) c v = if (head c1 == (succ c)) || (last c1 == (pred c)) then if v1 == v then c1 
                                                                                       else []
                                else if c < (pred (head c1)) then trouver a c v
                                else if c > (succ (last c1)) then trouver b c v
                                else [] 

estVide :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val -> Dictionnaire cle val
estVide Vide a2 = a2
estVide a1 _ = a1

maintien :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> Dictionnaire cle val
maintien Vide c v = Vide
maintien (Noeud c1 v1 a b) c v = if (head c1 == (succ c)) || (last c1 == (pred c)) then if v1 == v then (estVide a b)
                                                                                        else (Noeud c1 v1 a b)
                                 else if c < (pred (head c1))then (Noeud c1 v1 (maintien a c v) b)
                                 else if c > (succ (last c1))then (Noeud c1 v1 a (maintien b c v))
                                 else (Noeud c1 v1 a b)                                 

inserer :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> Dictionnaire cle val
inserer Vide c v = Noeud ([c,c]) v Vide Vide
inserer (Noeud c1 v1 a b) c v = if c == (pred (head c1)) then if v1 == v then (Noeud [(head ((trouver a c v)++[c])),(last c1)] v1 (maintien a c v) b)
                                                              else (Noeud c1 v1 (inserer a c v) b)
                                else if c == (succ (last c1)) then if v1 == v then (Noeud [(head c1),(last (c:(trouver b c v)))] v1 a (maintien b c v))
                                                                   else (Noeud c1 v1 a (inserer b c v))
                                else if c < (pred (head c1)) then (Noeud c1 v1 (inserer a c v) b)
                                else if c > (succ (last c1))then (Noeud c1 v1 a (inserer b c v))
                                else (Noeud c1 v1 a b) 

-- Insersion qui retourne Nothing si conflit de valeur
insererM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> Maybe (Dictionnaire cle val)
insererM d c v = if (rechercherCle d c) && ((rechercherParCle d c) /= Just (v)) then Nothing
                 else Just (inserer d c v)



-- Imprimer mon arbre
instance (Show cle, Show val) => Show (Dictionnaire cle val) where 
  show Vide = " "
  show (Noeud s val Vide Vide) = "(Noeud " ++ (" ["++show (head s)++","++show (last s)++"] " ++show val ++" Vide " ++ " Vide") ++ ")"
  show (Noeud s val l Vide) = "(Noeud " ++ (" ["++show (head s)++","++show (last s)++"] " ++ show val ++ ( show l) ++ " Vide") ++ ")"
  show (Noeud s val Vide r) = "(Noeud " ++ (" ["++show (head s)++","++show (last s)++"] "++ show val ++ " Vide " ++ ( show r)) ++ ")"
  show (Noeud s val l r) = "(Noeud " ++ (" ["++show (head s)++","++show (last s)++"] "++ show val ++ ( show l ) ++ ( show r)) ++ ")"




-- Somme des longueurs de tous les intervalles
tailleReelle :: (Enum cle) => Dictionnaire cle val -> Int
tailleReelle Vide = 0
tailleReelle (Noeud [] v a b) = 0
tailleReelle (Noeud c v a b) = ((fromEnum (last c)) - (fromEnum (head c)) + 1) + tailleReelle a + tailleReelle b  


-- Nombre de noeuds
taille :: Dictionnaire cle val -> Int
taille Vide = 0 
taille (Noeud [] v a b) = 0 + taille a + taille b
taille (Noeud c v a b) = 1 + taille a + taille b 

-- Inserer tout un interval de clefs pour un context, peut renvoyer Nothing en cas de conflits sur les contenus
trouverL :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> cle
trouverL Vide c v = c
trouverL (Noeud c1 v1 a1 a2) c v = if v1 == v then if c < (head c1) then max (trouverL a1 c v) (trouverL a2 c v)
                                                   else if c > (succ (last c1)) then trouverL a2 c v
                                                   else head c1
                                   else max (succ (last c1)) (trouverL a2 c v)
                                                   
trouverR :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> cle
trouverR Vide c v = c
trouverR (Noeud c1 v1 a1 a2) c v = if v1 == v then if c < (pred (head c1)) then trouverR a1 c v
                                                   else if c > (last c1) then min (trouverR a1 c v) (trouverR a2 c v)
                                                   else last c1
                                   else min (pred (head c1)) (trouverR a1 c v)

fusionL :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> Dictionnaire cle val
fusionL Vide l = Vide
fusionL (Noeud c1 v1 a1 a2) l = if (last c1) < l then (Noeud c1 v1 a1 (fusionL a2 l))
                                else fusionL a1 l

fusionR :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> Dictionnaire cle val
fusionR Vide r = Vide
fusionR (Noeud c1 v1 a1 a2) r = if (head c1) > r then (Noeud c1 v1 (fusionR a1 r) a2)
                                else fusionR a2 r 

insererIntervalle :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> cle -> val -> Dictionnaire cle val
insererIntervalle Vide l r v = Noeud [l,r] v Vide Vide
insererIntervalle (Noeud c1 v1 a1 a2) l r v = if (r < l) then (Noeud c1 v1 a1 a2)
                                              else if r < (pred (head c1)) then Noeud c1 v1 (insererIntervalle a1 l r v) a2
                                                   else if l > (succ (last c1)) then Noeud c1 v1 a1 (insererIntervalle a2 l r v)
                                                   else if (l < (head c1))&&((last c1) < r) then if (v == v1) then let ll = (trouverL a1 l v) 
                                                                                                                       rr = (trouverR a2 r v) 
                                                                                                                   in Noeud [ll,rr] v1 (fusionL (insererIntervalle a1 l (pred ll) v) ll) (fusionR (insererIntervalle a2 (succ rr) r v) rr)
                                                                                                              else Noeud c1 v1 (insererIntervalle a1 l (pred (head c1)) v) (insererIntervalle a2 (succ (last c1)) r v)
                                                       else if l < (head c1) then if (v == v1) then let ll = (trouverL a1 l v) in Noeud [ll,(last c1)] v1 (fusionL (insererIntervalle a1 l (pred ll) v) ll) a2
                                                                                  else Noeud c1 v1 (insererIntervalle a1 l (pred (head c1)) v) a2
                                                       else if (last c1) < r then if (v == v1) then let rr = (trouverR a2 r v) in Noeud [(head c1),rr] v1 a1 (fusionR (insererIntervalle a2 (succ rr) r v) rr)
                                                                                  else Noeud c1 v1 a1 (insererIntervalle a2 (succ (last c1)) r v)
                                                       else (Noeud c1 v1 a1 a2)

-- Version sur avec Maybe
existeConfI :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> cle -> val -> Bool
existeConfI Vide l r v = False
existeConfI d l r v1 = if l <= r then if(rechercherCle d l) && (rechercherParCle d l)/= Just (v1) then False
                                      else existeConfI d (succ l) r v1
                       else True


insererIntervalleM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> cle -> val -> Maybe (Dictionnaire cle val)
insererIntervalleM d l r v = if (existeConfI d l r v)then Just (insererIntervalle d l r v)
                             else Nothing


-- Insertion d'une liste de clefs et contexts
insererListe :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> [(cle,val)] -> Dictionnaire cle val
insererListe d n = if n == [] then d
                   else insererListe (inserer d (fst (head n)) (snd (head n))) (tail n)

-- Version sur avec Maybe
existeConfL ::  (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> [(cle,val)] -> Bool
existeConfL Vide n = False
existeConfL d n =  if (rechercherCle d (fst(head n))) then if (rechercherParCle d (fst(head n))) /= Just (snd(head n))then False
                                                           else existeConfL d (tail n) 
                  else True

insererListeM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> [(cle,val)] -> Maybe (Dictionnaire cle val)
insererListeM d n = if (existeConfL d n)then Just (insererListe d n)
                    else Nothing
                    
-- Suppression d'une clef
insert ::  (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val-> Dictionnaire cle val
insert Vide n = n
insert (Noeud x v a1 a2) n = Noeud x v a1 (insert a2 n) 

supprimer :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> Dictionnaire cle val
supprimer Vide n = Vide
supprimer (Noeud x v a1 a2) n = if n < head x then Noeud x v (supprimer a1 n) a2
                                else if n > last x then Noeud x v a1 (supprimer a2 n)
                                else if (n == (head x))&&(n == (last x)) then (insert a1 a2) 
                                else if n == (head x) then Noeud [(succ n),(last x)] v a1 a2
                                else if n == (last x) then Noeud [(head x),(pred n)] v a1 a2
                                else Noeud [(head x),(pred n)] v a1 (Noeud [(succ n),(last x)] v Vide a2)

-- Suppression d'une clef avec resultat Nothing si clef non presente
supprimerM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> Maybe (Dictionnaire cle val)
supprimerM a n = if (rechercherCle a n) == False then Nothing
                 else Just (supprimer a n) 




-- Rechercher une clef et rendre le contenu trouve, rend Nothing si non trouve
rechercherParCle :: Ord cle => Dictionnaire cle val -> cle -> Maybe val
rechercherParCle Vide c = Nothing
rechercherParCle (Noeud c1 v a b) c = if c >=head c1 && c <= last c1 then Just (v)
                                      else if c < head c1 then rechercherParCle a c
                                      else rechercherParCle b c 


-- suppression une liste des clés dans un dictionnaire
supprimerListe :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> [cle] -> Dictionnaire cle val
supprimerListe dic [] = dic 
supprimerListe dic (cle:l) = supprimerListe (supprimer dic cle) l

toSet::(Ord cle) => Dictionnaire cle val -> [([cle],val)] 
toSet Vide = []
toSet (Noeud l v g d) = (l,v):(toSet g) ++ (toSet d) 
-- modifier tous les contenus d'un dictionnaire
mapDico :: (Ord cle, Enum cle, Eq a, Eq b) =>
           (a->b) ->  Dictionnaire cle a -> Dictionnaire cle b
mapDico f Vide = Vide
mapDico f (Noeud l val gauche droite) = (Noeud  l (f val) (mapDico f gauche) (mapDico f droite))  
-- filtrer les contenus d'un dictionnaire
filtrer :: (Ord cle, Enum cle, Eq val) =>
           (val->Bool) ->  Dictionnaire cle val -> Dictionnaire cle val
filtrer f Vide = Vide
filtrer f (Noeud l val gauche droite) = if (f val == False) then  supprimerListe (Noeud l val (filtrer f gauche) (filtrer f droite)) [head l..last l]
                                                else (Noeud l val (filtrer f gauche) (filtrer f droite))




a = Noeud [50,75] 'A' (Noeud [8,8] 'B' (Noeud [5,6] 'C' (Noeud [0,2] 'D' (Vide) (Vide)) (Vide)) (Noeud [12,36] 'E' (Vide) (Noeud [42,45] 'F' (Vide) (Vide)))) (Noeud [85,89] 'G' (Noeud [80,83] 'H' (Vide) (Vide)) (Vide))

b = Noeud [60,82] 'A' (Noeud [10,11] 'B' (Noeud [2,4] 'C' (Vide) (Vide)) (Noeud [25,40] 'D' (Vide) (Vide))) (Noeud [90,91] 'E' (Vide) (Noeud [97,99] 'F' (Vide) (Vide)))


x = Noeud ['m','o'] 1 (Noeud ['a','b'] 2 (Vide) (Vide)) (Noeud ['x','y'] 3 (Vide) (Vide))
y = Noeud ['e','h'] 1 (Noeud ['d','d'] 2 (Vide) (Vide)) (Noeud ['k','o'] 3  (Vide) (Vide))


toListVal :: Dictionnaire cle val -> [val]
toListVal Vide = []
toListVal (Noeud s v a b) = (toListVal a) ++ [v] ++ (toListVal b)

toListCleVal :: Dictionnaire cle val -> [(cle,val)]
toListCleVal Vide = []
toListCleVal (Noeud s v a b) = (toListCleVal a) ++ l ++ (toListCleVal b) where l = do
                                                                                   r <- s
                                                                                   [(r,v)]

-- un fold qui peut utiliser les intervalles de clefs
foldIntervalle :: (Ord cle, Enum cle) => (a -> cle -> cle -> b -> a) -> a -> Dictionnaire cle b -> a
foldIntervalle _ acc _ = acc
foldIntervalle f acc (Noeud s v g d) = foldIntervalle f (foldIntervalle f (f acc (head s) (last s) v) g) d




-- Cette fonction n’est pas safe : le dictionnaire resultant n’est pas compresse
insererIntervalleCons :: (Ord cle, Enum cle) =>
                         Dictionnaire cle [val] ->
                         (cle,cle)   ->   val   ->   Dictionnaire cle [val]

insererIntervalleCons Vide (c1,c2) v = Noeud [c1..c2] [v] Vide  Vide
insererIntervalleCons (Noeud l v g d) (x,y) w 
     | y < head l = Noeud l v (insererIntervalleCons g (x,y) w) d 
     | x > last l = Noeud l v g (insererIntervalleCons d (x,y) w)
     | x > (head l) && y < last l = Noeud  [x..y] (w:v) (Noeud ((init l)++[pred x]) v g Vide) (Noeud ([succ y]++(tail l)) v Vide d)
     | x == (head l) && y < last l = Noeud [x..y] (w:v) g (Noeud ([succ y]++(tail l)) v Vide d)
     | x > (head l) && y == (last l) = Noeud [x..(last l)] (w:v) (Noeud ((init l)++[pred x]) v g Vide) d
     | x == (head l) && y == (last l) = Noeud l (w:v) g d
     | x == (head l) && y > last l = Noeud l (w:v) g (insererIntervalleCons d (succ (last l),y) w)
     | x < (head l) && y == (last l) = Noeud l (w:v) (insererIntervalleCons g (x,pred (head l)) w) d
     | x < (head l) && y > last l = Noeud l (w:v) (insererIntervalleCons g (x,pred (head l)) w) (insererIntervalleCons d (succ (last l),y) w)
     | x < (head l) && y < last l = Noeud [(head l)..y] (w:v) (insererIntervalleCons g (x,pred (head l)) w) (Noeud [(succ y)..(last l)] v Vide d)
     | x > (head l) && y > (last l) = Noeud [x..last l] (w:v) (Noeud [(head l)..pred x] v g Vide) (insererIntervalleCons d (succ (last l),y) w)



-- recompresse un dictionnaire mal compresse
compresser :: (Ord cle, Enum cle, Eq a) => Dictionnaire cle a -> Dictionnaire cle a
compresser = (foldIntervalle insererIntervalle creer)


-- fait l’union de dictionnaire en recuperant -- la liste des valeurs en cas de conflit unionCons :: (Ord cle, Enum cle, Eq val) =>
unionCons :: (Ord cle, Enum cle, Eq val) =>[Dictionnaire cle val] -> Dictionnaire cle [val]
unionCons l = compresser (Prelude.foldl unionConsBin creer l)
     where
     unionConsBin d1 Vide = d1
     unionConsBin d1 (Noeud l v g d) = unionConsBin (unionConsBin (insererIntervalleCons d1 (head l,last l) v) g) d
