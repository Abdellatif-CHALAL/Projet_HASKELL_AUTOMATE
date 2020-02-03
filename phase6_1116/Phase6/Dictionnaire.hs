module Dictionnaire(Dictionnaire(..),toListVal,valDeCle,insererIntervalleCons,creer,unionCons,firstValue,rechercherVal,rechercherCle,inserer,insererM,tailleReelle,taille,insererIntervalle,insererIntervalleM,insererListe,insererListeM,supprimer,supprimerM,rechercherParCle,mapDico,filter,insert,insertionPossible,rechercheAll,verification,remonter,supprimerNoeud,filsLePlusADroite,fP,filsLePlusAGauche,fusion) where
 
-- Arbre Binaire de Recherche par Intervalles
data Dictionnaire cle val  = Vide | Noeud (Dictionnaire cle val) cle cle (Dictionnaire cle val) val  deriving (Eq,Ord)


-- Creer un arbre vide
creer :: Dictionnaire cle val
creer = Vide


firstValue :: Dictionnaire cle val -> val
--firstValue (Noeud ag a b ad v) = v
firstValue (Noeud ag a b ad v) = v


--Cette fonction prend une cle et renvoie la valeur associée 
valDeCle :: (Enum cle , Ord cle, Eq cle) => Dictionnaire cle val-> cle -> Maybe val
valDeCle Vide c = Nothing
valDeCle (Noeud a x y b v) c  
 | c < x = valDeCle a c 
 | c > y = valDeCle b c 
 |otherwise = Just v 


rechercherVal :: Ord cle => Dictionnaire cle val -> cle -> val
--rechercherVal Vide _ = error
rechercherVal (Noeud ag a b ad v) element
 | element >= a && element <= b = v
 | element < a = rechercherVal ag element
 | element > b = rechercherVal ad element

-- un fold qui peut utiliser les intervalles de clefs
foldIntervalle :: (Ord cle, Enum cle) =>(a -> cle -> cle -> b -> a) -> a -> Dictionnaire cle b -> a
foldIntervalle _ acc Vide = acc
foldIntervalle f acc (Noeud g l u d v) = foldIntervalle f (foldIntervalle f (f acc l u v) g) d


-- recompresse un dictionnaire mal compresse
compresser :: (Ord cle, Enum cle, Eq a) => Dictionnaire cle a -> Dictionnaire cle a
compresser = (foldIntervalle insererIntervalle creer)


-- Cette fonction n’est pas safe : le dictionnaire resultant n’est pas compresse
insererIntervalleCons :: (Ord cle, Enum cle) => Dictionnaire cle [val] -> cle -> cle -> val -> Dictionnaire cle [val]
insererIntervalleCons Vide x y v = (Noeud Vide x y Vide [v])
insererIntervalleCons (Noeud g l u d v) x y w
 | y < l = Noeud (insererIntervalleCons g x y w) l u d v
 | x > u = Noeud g l u (insererIntervalleCons d x y w) v
 | x > l && y < u = Noeud (Noeud g l (pred x) Vide v) x y (Noeud Vide (succ y) u d v) (w:v)
 | x == l && y < u = (Noeud g x y (Noeud Vide (succ y) u d v) (w:v))
 | x > l && y == u = Noeud (Noeud g l (pred x) Vide v) x u d (w:v)
 | x == l && y == u = Noeud g l u d (w:v)
 | x == l && y > u = (Noeud g l u (insererIntervalleCons d (succ u) y w) (w:v))
 | x < l && y == u = Noeud (insererIntervalleCons g x (pred l) w) l u d (w:v) 
 | x < l && y > u = Noeud (insererIntervalleCons g x (pred l) w) l u (insererIntervalleCons d (succ u) y w) (w:v)
 | x < l && y < u = Noeud (insererIntervalleCons g x (pred l) w) l y (Noeud Vide (succ y) u d v) (w:v)
 | x > l && y > u = Noeud (Noeud g l (pred x) Vide v) x u (insererIntervalleCons d (succ u) y w) (w:v)



toListVal :: Dictionnaire cle val -> [val]
toListVal Vide = []
toListVal (Noeud ag a b ad v) = [v]++(toListVal ag)++(toListVal ad)


unionCons :: (Ord cle, Enum cle, Eq val) => [Dictionnaire cle val] -> Dictionnaire cle [val]
unionCons l = compresser (Prelude.foldl unionConsBin creer l)
 where
 unionConsBin d1 Vide = d1
 unionConsBin d1 (Noeud g l u d v) = (unionConsBin (unionConsBin (insererIntervalleCons d1 l u v) g) d)



-- Rechercher une clef dans les intervalles de l'arbre
rechercherCle :: Ord cle => Dictionnaire cle val -> cle -> Bool
rechercherCle Vide _ = False
rechercherCle (Noeud ag a b ad _) element
 | element >= a && element <= b = True
 | element < a = rechercherCle ag element
 | element > b = rechercherCle ad element

inserer :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> Dictionnaire cle val
inserer dico c v
 | rechercherCle dico c == False = insert dico c v
 | otherwise = dico

-- Inserer une clef avec son contexte en modifiant potentielement les intervalles
-- Non definit si conflit de valeur
insert :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> Dictionnaire cle val
insert Vide element v = Noeud Vide element element Vide v
insert (Noeud ag a b ad val) element v
--Le seul qui pourra engendrer une fusion "potentielle" avec l'intervalle modifié est le fils le plus à droite du fils gauche 
 | element == (pred a) && v == val = fusion (Noeud ag element b ad val) (filsLePlusADroite ag)
-- Même chose mais avec le fils le plus a gauche du fils droit
 | element == (succ b) && v == val = fusion (Noeud ag a element ad val) (filsLePlusAGauche ad)
--Sinon on cherche un intervalle potentiel à droite ou à gauche
 | element <= (pred a) = Noeud (inserer ag element v) a b ad val
 | element >= (succ b) = Noeud ag a b (inserer ad element v) val
--Cas où l'élément est dans l'intervalle donc déjà présent ou v et val sont différents lors d'un eventuelle insertion
 | otherwise = Noeud ag a b ad val

-- Insersion qui retourne Nothing si conflit de valeur
insererM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> val -> Maybe (Dictionnaire cle val)
insererM dico cle val
-- Si la clef n'est pas presente ou si la cle est presente avec la meme valeur dans ce dernier cas il n'y aura pas d'insertion mais ce n'est pas pour autant un conflit de valeur
 | ((rechercherCle dico cle) == False) || ((rechercherParCle dico cle)==Just(val)) = Just(insert dico cle val)
 | otherwise = Nothing

-- Imprimer mon arbre
instance (Show cle, Show val) => Show (Dictionnaire cle val) where
 show Vide = ""
 show (Noeud a p q b val) = show a ++ "[" ++ show p ++ ".." ++ show q ++ "] -> " ++ show val ++ " " ++ show b

-- Somme des longueurs de tous les intervalles
tailleReelle :: (Enum cle) => Dictionnaire cle val -> Int
tailleReelle Vide = 0
tailleReelle (Noeud ag p q ad _) = (tailleReelle ag) + (tailleReelle ad) + succ (fromEnum q - fromEnum p)

-- Nombre de noeuds
taille :: Dictionnaire cle val -> Int
taille Vide = 0
taille (Noeud a _ _ b _) = 1 + (taille a) + (taille b) 

-- Inserer tout un interval de clefs pour un context, peut renvoyer Nothing en cas de conflits sur les contenus
insererIntervalle :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> cle -> val -> Dictionnaire cle val
insererIntervalle Vide i1 i2 v = Noeud Vide i1 i2 Vide v
insererIntervalle (Noeud ag a b ad val) i1 i2 v
 | i1>=a && i1<=b && i2>=succ b = fusion (insererIntervalle (Noeud ag a b ad val) (succ b) i2 v) (filsLePlusAGauche (insererIntervalle ad (succ b) i2 v))
 | i2<=b && i2>=a && i1<=pred a = fusion (insererIntervalle (Noeud ag a b ad val) i1 (pred a) v) (filsLePlusADroite (insererIntervalle ag i1 (pred a) v))
 | i1<=a && i2>=b = fusion (fusion (Noeud (insererIntervalle ag i1 (pred a) v) a b (insererIntervalle ad (succ b) i2 v) val) (filsLePlusAGauche (insererIntervalle ad (succ b) i2 v))) (filsLePlusADroite (insererIntervalle ag i1 (pred a) v))
 | i1>=a && i2<=b = fusion (fusion (Noeud ag a b ad val) (filsLePlusADroite ag)) (filsLePlusAGauche ad)
 | i2<=pred a = fusion (Noeud (insererIntervalle ag i1 i2 v) a b ad val) (filsLePlusADroite (insererIntervalle ag i1 i2 v))
 | i1>=succ b = fusion (Noeud ag a b (insererIntervalle ad i1 i2 v) val) (filsLePlusAGauche  (insererIntervalle ad i1 i2 v))
-- Sinon impossible d'ajouter l'intervalle
 | otherwise = Noeud ag a b ad val

-- Version sur avec Maybe
insererIntervalleM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> cle -> val -> Maybe (Dictionnaire cle val)
insererIntervalleM dico i1 i2 v
 | (insertionPossible dico i1 i2 v) == True = Just(insererIntervalle dico i1 i2 v)
 | otherwise = Nothing

insertionPossible :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> cle -> val -> Bool
insertionPossible Vide _ _ _ = True
insertionPossible (Noeud ag a b ad val) i1 i2 v
 | i1>=a && i1<=b && i2>=succ b && v==val = insertionPossible ad (succ b) i2 v
 | i2<=b && i2>=a && i1<=pred a && v==val = insertionPossible ag i1 (pred a) v
 | i1<=a && i2>=b && v==val = (insertionPossible ag i1 (pred a) v) && (insertionPossible ad (succ b) i2 v) 
 | i1>=a && i2<=b && val==v = True
 | i2<=pred a = insertionPossible ag i1 i2 v
 | i1>=succ b = insertionPossible ad i1 i2 v
 | otherwise = False

-- Insertion d'une liste de clefs et contexts
insererListe :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> [(cle,val)] -> Dictionnaire cle val
insererListe  dico [] = dico
insererListe dico ((c,v):l)=inserer (insererListe dico l) c v

-- Version sur avec Maybe
insererListeM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> [(cle,val)] -> Maybe (Dictionnaire cle val)
insererListeM dico l 
 | rechercheAll dico l= Just (insererListe dico l)
 | otherwise = Nothing

rechercheAll :: (Enum cle, Eq val,Ord cle) => Dictionnaire cle val -> [(cle,val)] -> Bool
rechercheAll dico [] = True
rechercheAll dico ((c,v):l) 
-- L'insertion se fait si l'element n'est pas present ou si l'element est present, on verifie si sa valeur est bien la bonne dans se cas on le garde meme si il est deja present
 | (rechercherCle dico c) == True =  ((rechercherParCle dico c)==Just(v)) && (rechercheAll dico l)
 | otherwise = (rechercheAll dico l)

-- Suppression d'une clef
supprimer :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> Dictionnaire cle val
supprimer Vide _ = Vide
supprimer (Noeud ag a b ad val) element 
--Cas où l'élément à supprimer ne constitut pas un noeud à lui tout seul
 | element == a && b > a = Noeud ag ((succ a)) b ad val
 | element == b && a < b = Noeud ag a ((pred b)) ad val
--Cas où l'intervalle n'est qu'un point  on supprimer l'intervalle en n'oubliant pas de remonter ses fils potentiels
 | element == a && a == b = remonter(Noeud ag a b ad val)
--Cas où l'element est dans un intervalle, on decoupe donc l'intervalle en deux
 | element > a && element < b = Noeud ag a ((pred element)) (Noeud Vide ((succ element)) b ad val) val
--Cas où l'élément est inférieur à a, on va donc essayer de le supprimer à gauche
 | element < a = Noeud (supprimer ag element) a b ad val
--Cas où l'élément est supérieur à b, on va donc essayer de le supprimer à droite
 | element > b = Noeud ag a b (supprimer ad element) val

-- Suppression d'une clef avec resultat Nothing si clef non presente
supprimerM :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> cle -> Maybe (Dictionnaire cle val)
supprimerM a element 
 | rechercherCle a element == True = Just (supprimer a element)
 | otherwise = Nothing

-- Rechercher une clef et rendre le contenu trouve, rend Nothing si non trouve
rechercherParCle :: Ord cle => Dictionnaire cle val -> cle -> Maybe val
rechercherParCle Vide _ = Nothing
rechercherParCle (Noeud ag a b ad val) element
 | element >= a && element <= b = Just(val)
 | element < a = rechercherParCle ag element
 | element > b = rechercherParCle ad element

-- modifier tous les contenus d'un dictionnaire
mapDico :: (Ord cle, Enum cle, Eq a, Eq b) => (a->b) ->  Dictionnaire cle a -> Dictionnaire cle b
mapDico _ Vide = Vide
mapDico f (Noeud ag a b ad val) = insererIntervalle (mapDico f (supprimerNoeud (Noeud ag a b ad val) (Noeud ag a b ad val))) a b (f val)

-- filtrer les contenus d'un dictionnaire
filtrer :: (Ord cle, Enum cle, Eq val) => (val->Bool) ->  Dictionnaire cle val -> Dictionnaire cle val
filtrer _ Vide = Vide
filtrer f (Noeud ag a b ad val) 
 | (f val) == True = Noeud (filtrer f ag) a b (filtrer f ad) val
 | otherwise = filtrer f (supprimerNoeud (Noeud ag a b ad val) (Noeud ag a b ad val))
 -- | otherwise = supprimerNoeud (Noeud (filtrer f ag) a b (filtrer f ad) val) (Noeud (filtrer f ag) a b (filtrer f ad) val)

--Remonter est une fonction qui permet au fils d'un noeud qui va etre supprimer de ne pas être supprimés eux aussi
--Pour cela le fils gauche (resp droit (selon les cas)) devient père du fils droit (resp gauche) recursivement
remonter :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val
remonter Vide = Vide 
remonter (Noeud Vide _ _ Vide _) = Vide
remonter (Noeud (Noeud ag2 c d ad2 val2) a b ad1 _) = let (Noeud al i1 i2 ar v) = (filsLePlusADroite (Noeud ag2 c d ad2 val2)) in Noeud (supprimerNoeud (Noeud ag2 c d ad2 val2) (Noeud al i1 i2 ar v)) i1 i2 ad1 v
remonter (Noeud ag1 a b (Noeud ag2 c d ad2 val2) _) = let (Noeud al i1 i2 ar v) = (filsLePlusAGauche (Noeud ag2 c d ad2 val2)) in Noeud ag1 i1 i2 (supprimerNoeud (Noeud ag2 c d ad2 val2) (Noeud al i1 i2 ar v)) v

--fusion permettre de fusionner les intervalles de deux noeuds, si les bornes coïncides
fusion :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val -> Dictionnaire cle val
fusion a Vide = a 
fusion Vide a = a
fusion (Noeud ag1 a b ad1 val1) (Noeud ag2 c d ad2 val2)
-- Les bornes du deuxième intervalles coupent le premier, on modifie l'intervalle et on supprime l'autre
 | a<=c && d>=b && c<=succ b && val1==val2 = supprimerNoeud (Noeud ag1 a d ad1 val1) (Noeud ag2 c d ad2 val2)
 | a>=c && d<=b && d>=pred a && val1==val2 = supprimerNoeud (Noeud ag1 c b ad1 val1) (Noeud ag2 c d ad2 val2)
--Les bornes ne coincident pas, on a donc aucune fusion a faire 
--En effet, on a le cas ou l'intervalle 2 est inclu dans le 1 et ou les intervalles sont totalement disjoints : dans ces 2 cas on ne change rien 
 | otherwise = (Noeud ag1 a b ad1 val1)


--La fonction verification prend 3 arguments, 3 arbres Ensemble, il faut comprendre que ces 3 arbres correspondent au noeud, à son arbre gauche et son arbre droit qui seront regrouper par la suite
verification :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val -> Dictionnaire cle val -> Dictionnaire cle val
verification Vide _ _ = Vide
verification (Noeud ag1 a b ad1 val1) Vide Vide = Noeud Vide a b Vide val1
verification (Noeud ag1 a b ad1 val1) Vide (Noeud ag3 e f ad3 val3)
 | e>=a && f<=b && val1==val3 = verification (Noeud ag1 a b ad1 val1) Vide ad3
 | b>=pred e && b<f && val1==val3 = verification (Noeud ag1 a f ad1 val1) Vide ad3
 | e>succ b && (fP (Noeud ag1 a b ad1 val1) (filsLePlusAGauche(Noeud ag3 e f ad3 val3))) == True = verification (fusion (Noeud ag1 a b ad1 val1) (filsLePlusAGauche(Noeud ag3 e f ad3 val3))) Vide (supprimerNoeud (Noeud ag3 e f ad3 val3) (filsLePlusAGauche(Noeud ag3 e f ad3 val3)))
 | otherwise =  Noeud Vide a b (Noeud ag3 e f ad3 val3) val1
verification (Noeud ag1 a b ad1 val1) (Noeud ag2 c d ad2 val2) Vide
 | c>=a && d<=b && val1==val2 = verification (Noeud ag1 a b ad1 val1) ag2 Vide
 | a<=succ d && a>c && val1==val2 = verification (Noeud ag1 c b ad1 val1) ag2 Vide
 | d<pred a && (fP (Noeud ag1 a b ad1 val1) (filsLePlusADroite (Noeud ag2 c d ad2 val2)))==True = verification (fusion (Noeud ag1 a b ad1 val1) (filsLePlusADroite(Noeud ag2 c d ad2 val2))) (supprimerNoeud (Noeud ag2 c d ad2 val2) (filsLePlusADroite(Noeud ag2 c d ad2 val2))) Vide
 | otherwise =  Noeud (Noeud ag2 c d ad2 val2) a b Vide val1
verification (Noeud ag1 a b ad1 val1) (Noeud ag2 c d ad2 val2) (Noeud ag3 e f ad3 val3)
-- Cas où les intervalles 2 et 3 sont inclus dans le 1, dans ce cas tout ce qui est a droite du noeud 2 et a gauche du noeud 3 sont forcement inclus dans le 1
 | c>=a && d<=b && e>=a && f<=b && val1==val2 && val1==val3 = verification (Noeud ag1 a b ad1 val1) ag2 ad3
-- Cas où seulement le 2 est inclus dans le 1
 | c>=a && d<=b && val1==val2 = verification (Noeud ag1 a b ad1 val1) ag2 (Noeud ag3 e f ad3 val3)
-- Cas où seulement le 3 est inclus dans le 1
 | e>=a && f<=b && val1==val3 = verification (Noeud ag1 a b ad1 val1) (Noeud ag2 c d ad2 val2) ad3
-- Cas où les intervalles 2 et 3 sont partiellement inclus dans 1 ou sont à une distance de + ou - 1, dans ce cas là, on prolonge le 1
 | a<=succ d && a>c && b>=pred e && b<f && val1==val2 && val1==val3 = verification (Noeud ag1 c f ad1 val1) ag2 ad3
-- Cas où seulement le noeud 2 est partiellement inclus dans 1 ou à une distance de + ou - 1
 | a<=succ d && val1==val2 = verification (Noeud ag1 c b ad1 val1) ag2 (Noeud ag3 e f ad3 val3)
-- Cas où seulement le noeud 3 est partiellement inclus dans 1 ou à une distance de + ou - 1
 | b>=pred e && val1==val3= verification (Noeud ag1 a f ad1 val1) (Noeud ag2 c d ad2 val2) ad3
-- Cas où les intervalles 2 et 3 ne sont pas inclus dans 1 mais que les fusions entre le noeud le plus à droite de 2 et le noeud le plus à gauche de 3 avec 1 sont possibles
 | d<pred a && e>succ b && fP (Noeud ag1 a b ad1 val1) (filsLePlusADroite (Noeud ag2 c d ad2 val2))==True && (fP (Noeud ag1 a b ad1 val1) (filsLePlusAGauche(Noeud ag3 e f ad3 val3))) == True = verification (fusion(fusion(Noeud ag1 a b ad1 val1) (filsLePlusADroite(Noeud ag2 c d ad2 val2))) (filsLePlusAGauche(Noeud ag3 e f ad3 val3))) (supprimerNoeud (Noeud ag2 c d ad2 val2) (filsLePlusADroite(Noeud ag2 c d ad2 val2))) (supprimerNoeud (Noeud ag3 e f ad3 val3) (filsLePlusAGauche(Noeud ag3 e f ad3 val3)))
-- Cas où les intervalles 2 et 3 ne sont pas inclus dans 1 mais que la fusion entre le noeud le plus a droite de 2 avec 1 est possible
 | d<pred a && e>succ b && fP (Noeud ag1 a b ad1 val1) (filsLePlusADroite (Noeud ag2 c d ad2 val2))==True = verification (fusion (Noeud ag1 a b ad1 val1) (filsLePlusADroite(Noeud ag2 c d ad2 val2))) (supprimerNoeud (Noeud ag2 c d ad2 val2) (filsLePlusADroite (Noeud ag2 c d ad2 val2))) (Noeud ag3 e f ad3 val3)
-- Cas où les intervalles 2 et 3 ne sont pas inclus dans 1 mais que la fusion entre le noeud le plus a gauche de 2 avec 1 est possible
 | d<pred a && e>succ b && fP (Noeud ag1 a b ad1 val1) (filsLePlusAGauche (Noeud ag3 e f ad3 val3))== True = verification (fusion (Noeud ag1 a b ad1 val1) (filsLePlusAGauche(Noeud ag3 e f ad3 val3))) (Noeud ag2 c d ad2 val2) (supprimerNoeud (Noeud ag3 e f ad3 val3) (filsLePlusAGauche(Noeud ag3 e f ad3 val3)))
-- Autres cas : l'intervalle ajouté n'entraîne pas de modification de noeud
 | otherwise = Noeud (Noeud ag2 c d ad2 val2) a b (Noeud ag3 e f ad3 val3) val1

--Cette fonction permet de savoir si la fusion entre un noeud et un autre noeud est possible
fP :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val -> Bool
fP Vide _ = False
fP _ Vide = True
fP (Noeud ag1 a b ad1 val1) (Noeud ag2 c d ad2 val2)
 | c<=succ b && d>=b && val1==val2 = True
 | a<=c && b>=d && val1==val2 = True
 | c<=a && d>=pred a && val1==val2 = True
 | otherwise = False

-- Supprime un noeud dans un arbre, puis on remonte ses fils
supprimerNoeud :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val -> Dictionnaire cle val
supprimerNoeud Vide _ = Vide 
supprimerNoeud abr Vide = abr
supprimerNoeud (Noeud ag1 a b ad1 val1) (Noeud ag2 c d ad2 val2) 
 |c==a && b==d = remonter (Noeud ag1 a b ad1 val1)
 |a>=c && b>d = Noeud (supprimerNoeud ag1 (Noeud ag2 c d ad2 val2)) a b ad1 val1
 |a<c && b<=d = Noeud ag1 a b (supprimerNoeud ad1 (Noeud ag2 c d ad2 val2)) val1
 --Sinon c'est que l'intervalle exacte n'existe pas on renvoie donc l'arbre de départ
 |otherwise = (Noeud ag1 a b ad1 val1) 

--Récupère le fils le plus à droite d'un noeud, si ce fils est vide on renvoie lui même
filsLePlusADroite :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val
filsLePlusADroite Vide = Vide
filsLePlusADroite (Noeud ag a b ad val)
 | ad /= Vide = filsLePlusADroite ad
 | otherwise = Noeud ag a b ad val

--Récupère le fils le plus à gauche d'un noeud, si ce fils est vide on renvoie lui même
filsLePlusAGauche :: (Ord cle, Enum cle, Eq val) => Dictionnaire cle val -> Dictionnaire cle val
filsLePlusAGauche Vide = Vide
filsLePlusAGauche (Noeud ag a b ad val)
 | ag /= Vide = filsLePlusAGauche ag
 | otherwise = Noeud ag a b ad val

a = Noeud (Noeud (Noeud Vide 1 2 Vide "a") 5 6 (Noeud Vide 7 8 Vide "s") "d") 10 24 (Noeud (Noeud Vide 25 35 Vide "t") 48 60 (Noeud Vide 70 83 Vide "s") "v") "s"
b = Noeud (Noeud (Noeud Vide 0 0 Vide "n") 1 2 Vide "a") 9 10 (Noeud Vide 15 20 Vide "b") "c"
