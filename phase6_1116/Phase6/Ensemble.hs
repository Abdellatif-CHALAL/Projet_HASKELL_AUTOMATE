
module Ensemble(Ensemble,ensembleVide,(€),inserer,tailleReelle,taille,insererIntervalle,insererListe,supprimer,supprimerM,union,intersection,verification,remonter,supprimerNoeud,filsLePlusADroite,fP,filsLePlusAGauche,fusion) where



-- Arbre Binaire de Recherche par Intervalles
data Ensemble a = Vide | Noeud (Ensemble a) a a (Ensemble a)  deriving (Eq,Ord)


-- Creer un arbre vide
ensembleVide :: Ensemble a
ensembleVide = Vide

-- Rechercher une clef dans les intervalles de l'arbre
(€) :: (Enum a, Ord a) =>
       a -> Ensemble a -> Bool
_ € Vide = False
x € (Noeud a p q b) = if (x>=p && x<=q) then True else if(x<p) then x € a else x € b 

-- Inserer une clef en modifiant potentielement les intervalles
inserer :: (Enum a, Ord a) =>
           Ensemble a -> a -> Ensemble a
inserer Vide element = Noeud Vide element element Vide 
inserer (Noeud ag a b ad) element 
--Le seul qui pourra engendrer une fusion "potentielle" avec l'intervalle modifié est le fils le plus à droite du fils gauche 
 | element == (pred a) = fusion (Noeud ag element b ad) (filsLePlusADroite ag)
-- Même chose mais avec le fils le plus a gauche du fils droit
 | element == (succ b) = fusion (Noeud ag a element ad) (filsLePlusAGauche ad)
--Sinon on cherche un intervalle potentiel à droite ou à gauche
 | element < (pred a) = Noeud (inserer ag element) a b ad
 | element > (succ b) = Noeud ag a b (inserer ad element)
--Cas où l'élément est dans l'intervalle donc déjà présent 
 | otherwise = (Noeud ag a b ad)

-- Imprimer mon arbre
instance Show a => Show (Ensemble a) where
 show Vide = ""
 show (Noeud a p q b) = show a ++ "[" ++ show p ++ ".." ++ show q ++ "]" ++ show b

-- Somme des longueurs de tous les intervalles
tailleReelle :: (Enum a,Ord a) =>
                Ensemble a -> Int
tailleReelle Vide = 0
tailleReelle (Noeud ag p q ad) = (tailleReelle ag) + (tailleReelle ad) + succ (fromEnum q - fromEnum p)

-- Nombre de noeuds
taille :: Ensemble a -> Int
taille Vide = 0
taille (Noeud a _ _ b) = 1 + (taille a) + (taille b) 

--Remonter est une fonction qui permet au fils d'un noeud qui va etre supprimer de ne pas être supprimés eux aussi
--Pour cela le fils gauche (resp droit (selon les cas)) devient père du fils droit (resp gauche) recursivement
remonter :: (Ord a, Enum a) => Ensemble a -> Ensemble a
remonter Vide = Vide 
remonter (Noeud Vide a b Vide) = Vide
remonter (Noeud (Noeud ag2 c d ad2) a b ad1) =Noeud (remonter(Noeud ag2 c d ad2)) c d ad1
remonter (Noeud ag1 a b (Noeud ag2 c d ad2)) = Noeud ag1 c d (remonter (Noeud ag2 c d ad2))

-- Supprime un noeud dans un arbre, puis on remonte ses fils
supprimerNoeud :: (Ord a, Enum a) => Ensemble a -> Ensemble a -> Ensemble a
supprimerNoeud Vide _ = Vide 
supprimerNoeud abr Vide = abr
supprimerNoeud (Noeud ag1 a b ad1) (Noeud ag2 c d ad2) 
 |c==a && b==d = remonter (Noeud ag2 c d ad2)
 |a>=c && b>d = Noeud (supprimerNoeud ag1 (Noeud ag2 c d ad2)) a b ad1
 |a<c && b<=d = Noeud ag1 a b (supprimerNoeud ad1 (Noeud ag2 c d ad2))
 --Sinon c'est que l'intervalle exacte n'existe pas on renvoie donc l'arbre de départ
 |otherwise = (Noeud ag1 a b ad1) 

--Récupère le fils le plus à droite d'un noeud, si ce fils est vide on renvoie lui même
filsLePlusADroite :: (Ord a, Enum a) => Ensemble a -> Ensemble a
filsLePlusADroite Vide = Vide
filsLePlusADroite (Noeud ag a b ad)
 | ad /= Vide = filsLePlusADroite ad
 | otherwise = Noeud ag a b ad

--Récupère le fils le plus à gauche d'un noeud, si ce fils est vide on renvoie lui même
filsLePlusAGauche :: (Ord a, Enum a) => Ensemble a -> Ensemble a
filsLePlusAGauche Vide = Vide
filsLePlusAGauche (Noeud ag a b ad)
 | ag /= Vide = filsLePlusAGauche ag
 | otherwise = Noeud ag a b ad

--fusion permettre de fusionner les intervalles de deux noeuds, si les bornes coïncides
fusion :: (Ord a, Enum a) => Ensemble a -> Ensemble a -> Ensemble a
fusion a Vide = a 
fusion Vide a = a
fusion (Noeud ag1 a b ad1) (Noeud ag2 c d ad2)
-- Les bornes du deuxième intervalles coupent le premier, on modifie l'intervalle et on supprime l'autre
 | a<=c && d>=b && c<=succ b = supprimerNoeud (Noeud ag1 a d ad1) (Noeud ag2 c d ad2)
 | a>=c && d<=b && d>=pred a = supprimerNoeud (Noeud ag1 c b ad1) (Noeud ag2 c d ad2)
--Les bornes ne coincident pas, on a donc aucune fusion a faire 
--En effet, on a le cas ou l'intervalle 2 est inclu dans le 1 et ou les intervalles sont totalement disjoints : dans ces 2 cas on ne change rien 
 | otherwise = (Noeud ag1 a b ad1)

--La fonction verification prend 3 arguments, 3 arbres Ensemble, il faut comprendre que ces 3 arbres correspondent au noeud, à son arbre gauche et son arbre droit qui seront regrouper par la suite
verification :: (Ord a, Enum a) => Ensemble a -> Ensemble a -> Ensemble a -> Ensemble a
verification Vide _ _ = Vide
verification (Noeud ag1 a b ag2) Vide Vide = Noeud Vide a b Vide
verification (Noeud ag1 a b ad1) Vide  (Noeud ag3 e f ad3)
 | e>=a && f<=b = verification (Noeud ag1 a b ad1) Vide ad3
 | b>=pred e && b<f = verification (Noeud ag1 a f ad1) Vide ad3
 | e>succ b && (fP (Noeud ag1 a b ad1) (filsLePlusAGauche(Noeud ag3 e f ad3))) == True = verification (fusion (Noeud ag1 a b ad1) (filsLePlusAGauche(Noeud ag3 e f ad3))) Vide (supprimerNoeud (Noeud ag3 e f ad3) (filsLePlusAGauche(Noeud ag3 e f ad3)))
 | otherwise =  Noeud Vide a b (Noeud ag3 e f ad3)
verification (Noeud ag1 a b ad1) (Noeud ag2 c d ad2) Vide
 | c>=a && d<=b= verification (Noeud ag1 a b ad1) ag2 Vide
 | a<=succ d && a>c = verification (Noeud ag1 c b ad1) ag2 Vide
 | d<pred a && (fP (Noeud ag1 a b ad1) (filsLePlusADroite (Noeud ag2 c d ad2)))==True = verification (fusion (Noeud ag1 a b ad1) (filsLePlusADroite(Noeud ag2 c d ad2))) (supprimerNoeud (Noeud ag2 c d ad2) (filsLePlusADroite(Noeud ag2 c d ad2))) Vide
 | otherwise =  Noeud (Noeud ag2 c d ad2) a b Vide
verification (Noeud ag1 a b ad1) (Noeud ag2 c d ad2) (Noeud ag3 e f ad3)
-- Cas où les intervalles 2 et 3 sont inclus dans le 1, dans ce cas tout ce qui est a droite du noeud 2 et a gauche du noeud 3 sont forcement inclus dans le 1
 | c>=a && d<=b && e>=a && f<=b = verification (Noeud ag1 a b ad1) ag2 ad3
-- Cas où seulement le 2 est inclus dans le 1
 | c>=a && d<=b = verification (Noeud ag1 a b ad1) ag2 (Noeud ag3 e f ad3)
-- Cas où seulement le 3 est inclus dans le 1
 | e>=a && f<=b = verification (Noeud ag1 a b ad1) (Noeud ag2 c d ad2) ad3
-- Cas où les intervalles 2 et 3 sont partiellement inclus dans 1 ou sont à une distance de + ou - 1, dans ce cas là, on prolonge le 1
 | a<=succ d && a>c && b>=pred e && b<f = verification (Noeud ag1 c f ad1) ag2 ad3
-- Cas où seulement le noeud 2 est partiellement inclus dans 1 ou à une distance de + ou - 1
 | a<=succ d = verification (Noeud ag1 c b ad1) ag2 (Noeud ag3 e f ad3)
-- Cas où seulement le noeud 3 est partiellement inclus dans 1 ou à une distance de + ou - 1
 | b>=pred e = verification (Noeud ag1 a f ad1) (Noeud ag2 c d ad2) ad3
-- Cas où les intervalles 2 et 3 ne sont pas inclus dans 1 mais que les fusions entre le noeud le plus à droite de 2 et le noeud le plus à gauche de 3 avec 1 sont possibles
 | d<pred a && e>succ b && fP (Noeud ag1 a b ad1) (filsLePlusADroite (Noeud ag2 c d ad2))==True && (fP (Noeud ag1 a b ad1) (filsLePlusAGauche(Noeud ag3 e f ad3))) == True = verification (fusion(fusion(Noeud ag1 a b ad1) (filsLePlusADroite(Noeud ag2 c d ad2))) (filsLePlusAGauche(Noeud ag3 e f ad3))) (supprimerNoeud (Noeud ag2 c d ad2) (filsLePlusADroite(Noeud ag2 c d ad2))) (supprimerNoeud (Noeud ag3 e f ad3) (filsLePlusAGauche(Noeud ag3 e f ad3)))
-- Cas où les intervalles 2 et 3 ne sont pas inclus dans 1 mais que la fusion entre le noeud le plus a droite de 2 avec 1 est possible
 | d<pred a && e>succ b && fP (Noeud ag1 a b ad1) (filsLePlusADroite (Noeud ag2 c d ad2))==True = verification (fusion (Noeud ag1 a b ad1) (filsLePlusADroite(Noeud ag2 c d ad2))) (supprimerNoeud (Noeud ag2 c d ad2) (filsLePlusADroite (Noeud ag2 c d ad2))) (Noeud ag3 e f ad3)
-- Cas où les intervalles 2 et 3 ne sont pas inclus dans 1 mais que la fusion entre le noeud le plus a gauche de 2 avec 1 est possible
 | d<pred a && e>succ b && fP (Noeud ag1 a b ad1) (filsLePlusAGauche(Noeud ag3 e f ad3))== True = verification (fusion (Noeud ag1 a b ad1) (filsLePlusAGauche(Noeud ag3 e f ad3))) (Noeud ag2 c d ad2) (supprimerNoeud (Noeud ag3 e f ad3) (filsLePlusAGauche(Noeud ag3 e f ad3)))
-- Autres cas : l'intervalle ajouté n'entraîne pas de modification de noeud
 | otherwise = Noeud (Noeud ag2 c d ad2) a b (Noeud ag3 e f ad3)

--Cette fonction permet de savoir si la fusion entre un noeud et un autre noeud est possible
fP :: (Ord a, Enum a) => Ensemble a -> Ensemble a -> Bool
fP Vide _ = False
fP _ Vide = True
fP (Noeud ag1 a b ad1) (Noeud ag2 c d ad2)
 | c<=succ b && d>=b = True
 | a<=c && b>=d = True
 | c<=a && d>=pred a = True
 | otherwise = False

-- Inseerer tout un interval de clefs
insererIntervalle :: (Enum a, Ord a) =>
                     Ensemble a -> a -> a -> Ensemble a
insererIntervalle Vide i1 i2 = Noeud Vide i1 i2 Vide
insererIntervalle (Noeud ag a b ad) i1 i2 
--Si l'intervalle à inclure est inclu dans l'intervalle déjà présent, on ne fait rien
 | i1>=a && i2<=b = (Noeud ag a b ad) 
--Si la borne supérieur de l'intervalle que l'on veut rajouter est inférieur à (pred a) on va donc regarder à gauche
 | i2<pred a = Noeud (insererIntervalle ag i1 i2) a b ad
--Si la borne inférieur de l'intervalle que l'on veut rajouter est supérieur à (succ b) on va donc regarder à droite
 | i1>succ b = Noeud ag a b (insererIntervalle ad i1 i2)
--Si l'intervalle est à cheval entre l'intervalle du noeud déjà présent on va potentiellement fusionner des noeuds grâce à la fonction de vérification
 | i1>=a && i1<=succ b && i2>=succ b =verification (Noeud ag a i2 ad) ag ad
 | i1<=pred a && i2 <=b && i2>=pred a = verification (Noeud ag i1 b ad) ag ad
--Si l'intervalle englobe tout l'intervalle du noeud alors on change les bornes et une fusion avec d'autre noeud plus bas est eventuellement possible
 | otherwise = verification (Noeud ag i1 i2 ad) ag ad

-- Insertion d'une liste de clefs
insererListe :: (Enum a, Ord a) =>
                Ensemble a -> [a] -> Ensemble a
insererListe a [] = a
insererListe a (x:l)=inserer (insererListe a l) x


-- Suppression d'une clef
supprimer :: (Enum a, Ord a) =>
             Ensemble a -> a -> Ensemble a
supprimer Vide _ = Vide
supprimer (Noeud ag a b ad) element 
--Cas où l'élément à supprimer ne constitut pas un noeud à lui tout seul
 | element == a && b > a = Noeud ag ((succ a)) b ad
 | element == b && a < b = Noeud ag a ((pred b)) ad 
--Cas où l'intervalle n'est qu'un point  on supprimer l'intervalle en n'oubliant pas de remonter ses fils potentiels
 | element == a && a == b = remonter(Noeud ag a b ad)
--Cas où l'element est dans un intervalle, on decoupe donc l'intervalle en deux
 | element > a && element < b = Noeud ag a ((pred element)) (Noeud Vide ((succ element)) b ad)
--Cas où l'élément est inférieur à a, on va donc essayer de le supprimer à gauche
 | element < a = Noeud (supprimer ag element) a b ad
--Cas où l'élément est supérieur à b, on va donc essayer de le supprimer à droite
 | element > b = Noeud ag a b (supprimer ad element)

-- Suppression d'une clef avec resultat Nothing si clef non presente
supprimerM :: (Enum a, Ord a) =>
              Ensemble a -> a -> Maybe (Ensemble a)
supprimerM a element 
 | element € a == True = Just (supprimer a element)
 | otherwise = Nothing




-- union des deux ensembles
union :: (Enum a, Ord a) =>
         Ensemble a -> Ensemble a -> Ensemble a
union Vide Vide = Vide
union Vide abr = abr
union abr Vide = abr
union (Noeud ag1 a b ad1) (Noeud ag2 c d ad2) = insererIntervalle(insererIntervalle (union (supprimerNoeud (Noeud ag1 a b ad1) (Noeud ag1 a b ad1)) (supprimerNoeud (Noeud ag2 c d ad2) (Noeud ag2 c d ad2))) a b) c d

-- intersection des deux ensembles
intersection :: (Enum a, Ord a) =>
                Ensemble a -> Ensemble a -> Ensemble a
intersection Vide Vide = Vide
intersection Vide abr = Vide
intersection abr Vide = Vide
intersection (Noeud ag1 a b ad1) (Noeud ag2 c d ad2)
 | a € (Noeud ag2 c d ad2) = inserer (intersection (supprimer (Noeud ag1 a b ad1) a) (Noeud ag2 c d ad2)) a
 | otherwise = intersection (supprimer (Noeud ag1 a b ad1) a) (Noeud ag2 c d ad2)

-- parcourt la structure en profondeur comme le foldl parcourt une liste
foldSet :: (Enum a, Ord a) => (a -> b -> b) -> b -> Ensemble a -> b 
foldSet _ i Vide = i
foldSet f i (Noeud ag a b ad) = f a (foldSet f i (supprimer (Noeud ag a b ad) a ))

a = Noeud(Noeud(Noeud(Noeud Vide 0 2 Vide) 5 6 Vide ) 8 8 (Noeud Vide 12 36 (Noeud Vide 42 45 Vide ))) 50 75 (Noeud(Noeud Vide 80 83 Vide) 85 89 Vide)
b = Noeud(Noeud(Noeud(Noeud Vide 1 2 Vide) 5 5 Vide ) 8 9 (Noeud Vide 11 36 (Noeud Vide 42 45 Vide ))) 50 75 (Noeud(Noeud Vide 80 83 Vide) 85 89 Vide)
c = Noeud Vide 5 50 Vide  