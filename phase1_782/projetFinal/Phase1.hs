module Phase1(ABRI,creer,rechercher,inserer,imprimer,tailleReelle,taille,insererIntervalle,insererListe,supprimer) where
-- Arbre Binaire de Recherche par Intervalles
{-
Cette déclaration veut dire qu'une valeur de type ABRI est soit représentée par le constructeur Vide(on'est au bout de l'arbre) ou un Noeud( Un Arbre). Le deriving Show à la fin de la déclaration fait que ghc génère automatiquement une instance de Show pour notre type, 
 -}
data ABRI = Vide | Noeud ABRI (Int,Int) ABRI deriving (Show,Eq,Read)


{-Creer un arbre vide
 -ette fonction permet la création d'un arbre vide
 -}

creer :: ABRI
creer = Vide

-- Rechercher une clef dans les intervalles de l'arbre
{- Cette fonction prend deux paramétres un ABRI et une valeur,renvoie False si la valeur n'appartient à l'arbre sinon True ,
 - le principe consiste à rechercher la valeur  dans l' ABR tout en parcourant les branches en partant de la racine,
 - en descendant chaque fois sur le fils gauche ou sur le fils droit suivant la valeur(clé) ́portée sur le noeud est plus grande 
 - ou plus petite que la valeur cherchée. La recherche s’arrête dès que la valeur est rencontrée ou que l’on a parcourus tout l'arbre 
 -}
rechercher :: ABRI -> Int -> Bool
rechercher Vide val = False
rechercher (Noeud gauche (p,q) droite) val 
                | p-1 > val = rechercher gauche val
                | q+1 < val = rechercher droite val
                | otherwise = if ((p <= val) && (val <= q)) then True 
                              else False

-- Insérer une clef en modifiant potentielement les intervalles
-- Fonctions secondaires
--Cette fonction prend en paramétre un ABRI et renvoie l'élément maximum de couple
renvoiQ :: ABRI -> Int
renvoiQ (Noeud gauche (p,q) droite) = q
          
--Cette fonction prend en paramétre un ABRI et renvoie l'élément minimum de couple
renvoiP :: ABRI -> Int
renvoiP (Noeud gauche (p,q) droite) = p

--Cette fonction prend en paramétre un ABRI et renvoie un ABRI minimum de couple
renvoiD :: ABRI -> ABRI
renvoiD (Noeud gauche (p,q) droite) = droite
renvoiG :: ABRI -> ABRI
renvoiG (Noeud gauche (p,q) droite) = gauche
 
{-Cette fonction prend en paramétre un ABRI et un une clé(valeur entière) à insérer, 
 -renvoie un ABRI contenant le nouvel élélement,Le principe est le même que pour la recherche. 
 -Un nouveau noeud est créé avec la nouvelle valeur et inséré à  l’endroit ou la recherche s’est arêtee 
 -}

inserer :: ABRI -> Int -> ABRI 

inserer Vide val = (Noeud Vide (val,val) Vide)
inserer(Noeud gauche (p,q) droite) val
                |(rechercher(Noeud gauche (p,q) droite) val == True) = (Noeud gauche (p,q) droite)
                |(p-1 == val)&&(gauche == Vide) = Noeud gauche (val,q) droite
                |(p-1 == val) = if (renvoiQ gauche == val-1) then Noeud (renvoiG gauche) (renvoiP gauche,q) droite
                                else Noeud gauche (val,q) droite
                |(q+1 == val)&&(droite == Vide) = Noeud gauche (p,val) droite
                |(q+1 == val) = if (renvoiP droite == val+1) then Noeud gauche (p,renvoiQ droite) (renvoiD droite)
                                                  else Noeud gauche (p,val) droite
                | p-1 > val = (Noeud (inserer gauche val) (p,q) droite)
                | q+1 < val = (Noeud gauche (p,q) (inserer droite val))

{-
 - Cette fonction prend comme paramétre un ABRI et renvoie un string
 -}

imprimer :: ABRI -> String
imprimer Vide = ""
imprimer(Noeud gauche (p,q) droite) = imprimer gauche++ show [p,q] ++imprimer droite

{-Somme des longueurs de tous les intervalles
 -Cette fonction prend comme paramétre un ABRI et renvoie un int
 -}

tailleReelle :: ABRI -> Int
tailleReelle Vide = 0
tailleReelle (Noeud gauche (p,q) droite) = tailleReelle gauche + tailleReelle droite + (q-p)

{-Nombre de noeuds
 -Cette fonction prend comme paramétre un ABRI et renvoie un int
 -}
taille :: ABRI -> Int
taille Vide = 0
taille (Noeud gauche (p,q) droite) = taille gauche + taille droite + 1

{-Suppression d'une clef
 -Cette fonction prend deux paramétres un ABRI et un int,crée un nouvel ABRI
 - auquel elle  enleve l'élément passé comme paramétre
 - On doit envisager différents cas suivant ce qu'on trouve en allant à droite et à gauche :
 - Si p == val on incrémente p de l'étiquette correspondante 
 - Si q ==  val on décrémente q de l'étiquette correspondante 
 - si l'élément val inclus dans l'intervalle, on le supprime et on découpe l'intervalle 
 - si val < p-1 ,on supprime val du sous arbre gauche 
 - si val > q+1, on supprime val du sous arbre droite
-}

supprimer :: ABRI -> Int -> ABRI
supprimer Vide val = Vide
supprimer (Noeud gauche (p,q) droite) val
                |(p == val) = (Noeud gauche (p+1,q) droite)
                |(q == val) = (Noeud gauche (p,q-1) droite)
                |(p < val) && (val < q) = (Noeud gauche (p,val-1) (Noeud Vide (val+1,q) droite)) 
                |p-1 > val = (Noeud (supprimer gauche val) (p,q) droite)
                |q+1 < val = (Noeud gauche (p,q) (supprimer droite val))




{-Insertion d'une liste de clefs
 -Cette fonction prend en paramétre un ABRI et une liste d'entiers, renvoie un ABRI
 -Si la liste est vide, on renvoie directement 
 -Sinon on va extraire à chaque fois l'élément de tête de la liste puis on l'insère dans l'arbre 
 -}

insererListe :: ABRI -> [Int] -> ABRI
insererListe arbi [] = arbi 
insererListe abri (x:l) = insererListe (inserer abri x) l

{-Insérer tout un intervalle de clefs
 - Cette fonction prend en paramétre un ABRI, un couple d'entiers et renvoie un ABRI
 - Si l'arbre est vide,on renvoie directement l'arbre passé en paramétre
 - Si le couple existe dans l'arbre, elle nous renvoie l'arbre initial
 - Sinon on l'isère tout en tenant compte des étiquettes des sous arbres gauche et droit.
 -}

insererIntervalle :: ABRI -> Int -> Int -> ABRI
insererIntervalle Vide a b = (Noeud Vide (a,b) Vide)  
insererIntervalle (Noeud gauche (p,q) droite) a b
                 |(a >= p)&&(b <= q) = (Noeud gauche (p,q) droite)
                 |(a < p)&&(gauche == Vide) = (Noeud gauche (a,q) droite)
                 |(a < p)&&(renvoiD gauche /= Vide) = if ((a >=  renvoiP gauche)&&(a <= renvoiQ gauche)) then Noeud (renvoiG gauche) (renvoiP gauche,q) droite
                                                        else insererIntervalle (Noeud (renvoiG gauche) (renvoiP (renvoiD gauche),q) droite) a ((renvoiP gauche)-1)
                 |(a < p)&&(gauche /= Vide) = if (a <=  renvoiQ gauche) then Noeud (renvoiG gauche) (renvoiP gauche,q) droite
                                                        else Noeud gauche (a,q) droite  
                 |(b > q)&&(droite == Vide) = (Noeud gauche (p,b) droite)
                 |(b > q)&&(renvoiG droite /= Vide) = if (b > renvoiP droite) then Noeud gauche (p,renvoiQ droite) (renvoiD droite)
                                                        else Noeud gauche (p,renvoiQ (renvoiG droite)) (renvoiD droite)
                 |(b > q)&&(droite /= Vide) = if (b > renvoiP droite) then Noeud gauche (p,renvoiQ droite) (renvoiD droite)
                                              else Noeud gauche (p,b) droite  
                 | b < p-1 = Noeud (insererIntervalle gauche a b) (p,q) droite
                 | a > q+1 = Noeud gauche (p,q) (insererIntervalle droite a b)
