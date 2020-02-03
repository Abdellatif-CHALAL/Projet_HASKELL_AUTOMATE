module Phase4 where
import Dictionnaire
import Data.Map.Strict
import Data.Set
import Data.Hashable
import Data.List
import Data.Maybe


--data Etat = Etat Bool Transitions
--data AutomateFini = AutomateFini (Map Nom Etat) Nom

-- les types suivants peuvent être utilisés à la place
-- si vous savez vous en servir :
type Nom =  Int
type Transitions = Dictionnaire Char Nom
data Etat = Etat Bool Transitions deriving (Show)
data AutomateFini = AutomateFini (Map Nom Etat) Nom  deriving (Show)

-- les types suivants peuvent être utilisés à la place
-- si vous savez vous en servir :
--
-- data Etat = Etat { getFinal :: Bool
--                  , getTrans :: Transitions
--                  }
-- data AutomateFini = AutomateFini { getEtats   :: Map Nom Etat
--                                  , getInitial :: Nom
--                                  }



getEtat :: Nom -> AutomateFini -> Maybe Etat
getEtat nom (AutomateFini m etat) = if(nom==(geta(Data.Map.Strict.elemAt (pred (Data.Map.Strict.size m)) m)))
                                    then Just (getb(Data.Map.Strict.elemAt (pred (Data.Map.Strict.size m)) m))
                                    else getEtat nom (AutomateFini (Data.Map.Strict.deleteAt (pred (Data.Map.Strict.size m)) m) etat)

transition :: AutomateFini -> Etat -> Char -> Maybe Etat
transition automate (Etat _ m ) c = if(rechercherCle m c) 
                                    then getEtat (fromJust (rechercherParCle m c)) automate
                                    else Nothing

transitionMot :: AutomateFini -> Etat -> String -> Maybe Etat
transitionMot automate etat (q:l) = let borne = (transition automate etat q) in if(isNothing borne) 
                                                                                then Nothing 
                                                                                else transitionMot automate (fromJust(transition automate etat q)) l
transitionMot automate etat [] = Just etat

getBool :: Etat -> Bool
getBool (Etat x _) = x   

getb :: (a,b) -> b
getb (a,b) = b

geta :: (a,b) -> a
geta (a,b) = a

-- Reconnaissance d'un mot par un automate
reconnise :: AutomateFini -> String -> Bool
reconnise (AutomateFini m etat) mot = let borne = transitionMot (AutomateFini m etat) (fromJust (getEtat etat (AutomateFini m etat))) mot in if(isNothing borne) then False else  getBool (fromJust (borne))


-- créer un automate a partir d'une liste d'états avec leur booleen d'acceptation et d'une liste de transitions
-- l'état initial est le premier nom de la liste de noms
newAutomaton :: [(Nom,Bool)] -> [(Nom,Char,Nom)] ->  AutomateFini
newAutomaton l1 l2 = AutomateFini (Data.Map.Strict.fromList (intermediaire l1 l2 []))  (geta (head l1)) 


intermediaire :: [(Nom,Bool)] -> [(Nom,Char,Nom)] -> [(Nom,Etat)] -> [(Nom,Etat)]
intermediaire [] [] l3 = l3
intermediaire ((x1,y1):r1) l2 [] = intermediaire r1 l2 ((x1, (Etat y1 creer)):[])
intermediaire ((x1,y1):r1) l2 l3 = intermediaire r1 l2 (((x1, (Etat y1 creer)):l3))
intermediaire  [] ((x2,y2,z2):r2) ((x3,(Etat bool dico)):r3) = if(x2==x3)
                                                               then intermediaire [] r2 ((x3,(Etat bool (inserer dico y2 z2))):r3)
                                                               else intermediaire [] ((x2,y2,z2):r2) (r3++((x3,Etat bool dico):[]))



-- type des automate infinis du TP2


data EtatD = EtatD Bool (Char->EtatD) 
type AutomateD = EtatD 

{-
instance Show EtatD where
  show (EtatD n a) = (show n)
-}  
creer_useless :: EtatD
creer_useless = EtatD False(\_ -> creer_useless)


                                                

gettransition :: Dictionnaire Char Nom -> AutomateFini -> (Char->Etat)
gettransition = undefined 

-- projeter sur le type du TP2
forgetFiniteness :: AutomateFini -> AutomateD
forgetFiniteness (AutomateFini etats init) = forgetFinitenessEtat (etats Data.Map.Strict.! init)
   where
   forgetFinitenessEtat (Etat b tr) =
       EtatD b (forgetFinitenessTransition tr)
   forgetFinitenessTransition tr c =
       let nom = fromJust (rechercherParCle tr c)
       in let etat = etats Data.Map.Strict.! nom
       in forgetFinitenessEtat etat


-- exemple pour ecrire (abc)*
etat1 = Etat True (Dictionnaire.inserer (Dictionnaire.inserer creer 'a' 2) 'b' 3)
etat2 = Etat False (Dictionnaire.insererIntervalle creer 'a' 'a' 4)
etat3 = Etat False (Dictionnaire.insererIntervalle creer 'b' 'b' 4)
etat4 = Etat False (Dictionnaire.insererIntervalle creer 'c' 'c' 4)
monAutomate = AutomateFini (Data.Map.Strict.fromList [(1,etat1),(2,etat2),(3,etat3),(4,etat4)]) 1


-- version non-déterministe
type NomND = Int
type TransitionsND = Dictionnaire Char [NomND]
data EtatND = EtatND { getFinalND :: Bool
                     , getTransND :: TransitionsND
                     }
                     deriving Show
data AutomateFiniND = AutomateFiniND { getEtatsND :: Map NomND EtatND
                                     , getInitND   :: [NomND]
                                     }
                     deriving Show





determinise :: AutomateFiniND -> AutomateFini
determinise (AutomateFiniND etatsND initND) = AutomateFini etats init
        where
          init  = nommer initND
          etats = determiniseAux etatsND Data.Map.Strict.empty [initND] 
                     
determiniseAux :: (Map NomND EtatND) -> Map Nom Etat -> [[NomND]] -> Map Nom Etat
determiniseAux etatsND acc [] = acc
determiniseAux etatsND acc (noms:l)
              | Data.Map.Strict.member nomDet acc = determiniseAux etatsND acc l
              | otherwise           = determiniseAux etatsND (Data.Map.Strict.insert nomDet etat acc) ((Dictionnaire.toListVal ft)++l)
                where
                  nomDet   = nommer noms
                  etat     = Etat (estTerminalList es) (mapDico nommer ft)
                  es       = fmap (etatsND !) noms
                  ft       = fusionT es
                     
                     
estTerminalList :: [EtatND] -> Bool
estTerminalList [] = False
estTerminalList ((EtatND b _):le) = b || (estTerminalList le)
                      
                     
regroupe :: [TransitionsND] -> TransitionsND
regroupe = (mapDico concat) . unionCons 
                     
fusionT :: [EtatND] -> TransitionsND
fusionT = regroupe . (fmap getTransND)
                     
nommer :: [NomND] -> Nom
nommer l = hash l
                     
                     
-- créer un automate qui reconnaît un caractère compris dans un segment donné
segment :: Char -> Char -> AutomateFiniND
segment c1 c2 = AutomateFiniND (Data.Map.Strict.fromList [(1,etat1) , (2,etat2)]) [1]
                  where
                      etat1 = EtatND False (Dictionnaire.insererIntervalle Dictionnaire.creer c1 c2 [2])
                      etat2 = EtatND True Dictionnaire.creer
                     
-- créer un automate qui reconnaît la somme des langages reconnus par les automates entres
(!+!) :: AutomateFiniND -> AutomateFiniND -> AutomateFiniND
e1 !+! e2 = AutomateFiniND (Data.Map.Strict.union etats1 etats2) (i1 ++ i2)
                  where
                      (AutomateFiniND etats1 i1) = modifNom e1 1
                      (AutomateFiniND etats2 i2) = modifNom e2 2
                          
                     
modifNom :: AutomateFiniND -> NomND -> AutomateFiniND
modifNom (AutomateFiniND etats i) c = AutomateFiniND (mapKeys (hashWithSalt c) (fmap (modifNomE c) etats)) (fmap (hashWithSalt c) i)
                     
modifNomE :: NomND -> EtatND -> EtatND
modifNomE c (EtatND b tr) = EtatND b (mapDico (fmap (hashWithSalt c)) tr)
                     
                     
--e1 = AutomateFiniND (M.fromList [(1,(EtatND False (inserer creer 'a' [2]))),(2,(EtatND True (creer)))]) [1]
--e2 = AutomateFiniND (M.fromList [(1,(EtatND False (inserer creer 'b' [2]))),(2,(EtatND True (creer)))]) [1]
                     
                     
-- créer un automate qui reconnaît l'étoile du langage reconnu par l'automate entre
star :: AutomateFiniND -> AutomateFiniND
star e = AutomateFiniND (Data.Map.Strict.insert 0 etat0 (fmap cont etats)) [0]
      where
          cont (EtatND False tr) = EtatND False tr
          cont (EtatND True tr) = EtatND True (regroupe [tr,transInit])
          AutomateFiniND etats i = modifNom e 1
          transInit = regroupe (fmap (getTransND . (etats !)) i)
          etat0 = EtatND True transInit
                     
                     
-- créer un automate qui reconnaît la concaténation des langages reconnus par les automates entres
(!.!) :: AutomateFiniND -> AutomateFiniND -> AutomateFiniND
e1 !.! e2 = AutomateFiniND (Data.Map.Strict.union (fmap cont etats1) etats2) i1
             where
                cont (EtatND False tr) = EtatND False tr
                cont (EtatND True  tr) = EtatND False (regroupe (tr:(fmap (getTransND . (etats2 !)) i2 )))
                AutomateFiniND etats1 i1 = modifNom e1 1
                AutomateFiniND etats2 i2 = modifNom e2 2





-- type des expressions régulières
data Regexp = Segment Char Char  |  Regexp :+: Regexp  |  Star Regexp  |  Regexp :.: Regexp 

-- construire un automate reconnaissant le même langage que l'expression régulière entrée
regexp2Automata :: Regexp -> AutomateFini
regexp2Automata = determinise . regexp2AutomataND

regexp2AutomataND :: Regexp -> AutomateFiniND
regexp2AutomataND (Segment c1 c2) = segment c1 c2
regexp2AutomataND (r1 :+: r2) = regexp2AutomataND r1 !+! regexp2AutomataND r2
regexp2AutomataND (r1 :.: r2) = regexp2AutomataND r1 !.! regexp2AutomataND r2
regexp2AutomataND (Star r) = star (regexp2AutomataND r)
--regexp2AutomataND Eps =  AutomateFiniND (Data.Map.Strict.fromList [(hash "",EtatND True Dictionnaire.creer)]) [hash ""]


-- implémentations de Hashable

instance Hashable a => Hashable (Set a) where
  hashWithSalt salt set = hashWithSalt salt (Data.Set.toList set)


instance (Hashable a, Hashable b) => Hashable (Dictionnaire a b) where
 hashWithSalt salt dic = hashWithSalt salt (Dictionnaire.toListCleVal dic)

 
-- Automates figés

type TransitionsF = Dictionnaire Char EtatF 
data EtatF = EtatF Bool TransitionsF deriving(Eq,Show) 
type AutomateFiniF = EtatF


figer :: AutomateFini -> AutomateFiniF
figer (AutomateFini etats init) = let (Etat b tr) = (etats Data.Map.Strict.! init) in
                                   (EtatF b (figerTransitionF etats (Dictionnaire.toListCleVal tr) (Dictionnaire.creer)))

figerTransitionF::Map Nom Etat -> [((Char,Char),Nom)]->  Dictionnaire Char EtatF -> Dictionnaire Char EtatF                                            
figerTransitionF _ [] dic = dic 
figerTransitionF etats (((c1,c2),nom):l) dic = if (Data.Map.Strict.member nom etats) then let (Etat b tran) = (etats Data.Map.Strict.! nom)
                                                                                          in figerTransitionF (Data.Map.Strict.delete nom etats) l (Dictionnaire.insererIntervalle dic c1 c2 (EtatF b (figerTransitionF (Data.Map.Strict.delete nom etats) (Dictionnaire.toListCleVal tran) (Dictionnaire.creer))))
                                                else dic


reconiseF :: AutomateFiniF -> String -> Bool
reconiseF (EtatF b _) []        = b
reconiseF (EtatF _ trans) (c:w) = if (Dictionnaire.rechercherCle trans c) then reconiseF (fromJust (Dictionnaire.rechercherParCle trans c)) w
                                  else False



-- une partition associe les noms de nos états à un entier appellée classe d’équivalence
-- (deux états étant dans le même classe d’équivalence s’ils sont associés au même entier)
type EqClass = Int
type Partition = Map Nom EqClass

-- compte combien de valeurs différentes on a (càd. combien de classe on a dans la partition)

taillePartition :: Partition -> Int
taillePartition map = let l = tailleList (Data.Map.Strict.elems map) [] in length l

tailleList:: (Eq a)=> [a] -> [a] -> [a] 
tailleList [] l = l 
tailleList (x:l1) l = if (appartient x l) then tailleList l1 l
                        else tailleList l1 (x:l)

appartient:: (Eq a )=> a -> [a] -> Bool
appartient x [] = False
appartient x (y:l) = if (x == y) then True else appartient x l


-- associe à chaque charactère le nom de la classe d’équivalence auquel appartient l’état ciblé par la transition.
projection :: Partition -> Transitions -> Transitions
projection map dic = let a = (noveauTrans map (Dictionnaire.toListCleVal dic)) 
                     in insererTout a (Dictionnaire.creer)

insererTout::[((Char,Char),EqClass)] -> Dictionnaire Char Nom -> Dictionnaire Char Nom
insererTout [] dic = dic    
insererTout (((c1,c2),val):l) dic = insererTout l (Dictionnaire.insererIntervalle dic c1 c2 val)


noveauTrans::Partition -> [((Char,Char),Nom)] -> [((Char,Char),EqClass)]
noveauTrans map [] = []
noveauTrans map ((x,y):l) = if (Data.Map.Strict.member y map) then (x,(map ! y)):noveauTrans map l 
                            else noveauTrans  map l          



-- associe le nombre 0 aux états non terminaux et 1 aux états terminaux
partitionInnitial :: AutomateFini -> Partition
partitionInnitial (AutomateFini etat init) = Data.Map.Strict.fromList (partition0ou1 (Data.Map.Strict.toList etat))  


partition0ou1::[(Nom,Etat)] -> [(Nom,EqClass)]
partition0ou1 [] = []
partition0ou1 ((x,Etat b _):l) = if b then (x,0):partition0ou1 l
                                  else (x,1):partition0ou1 l  



-- calcule une nouvelle partition à chaque étape de la minimisation
nouvellePartition :: AutomateFini -> Partition -> Partition
nouvellePartition auto par = let p = Data.Map.Strict.fromList (nouvellePart auto par (Data.Map.Strict.toList par)) in if (taillePartition p == taillePartition par) then p
                                                                                                                       else nouvellePartition auto par                   

nouvellePart:: AutomateFini -> Partition -> [(Nom,EqClass)] -> [(Nom,EqClass)] 
nouvellePart _ _ [] = []
nouvellePart (AutomateFini etat init) par ((x,y):l) = if (Data.Map.Strict.member x etat) 
                                                   then let (Etat b tr) =  etat Data.Map.Strict.! x in ((hashWithSalt 12 (projection par tr)),y):nouvellePart (AutomateFini etat init) par l
                                                   else nouvellePart (AutomateFini etat init) par l   

-- dernière étape de la minimisation
partition2Automate :: AutomateFini -> Partition -> AutomateFini
partition2Automate (AutomateFini etat init) par = AutomateFini (Data.Map.Strict.fromList (nouveauAutomate (Data.Map.Strict.toList etat) (nouvellePartition (AutomateFini etat init) par))) (geta (Data.Map.Strict.elemAt 0 par))     

                              
nouveauAutomate::[(Nom,Etat)] -> Partition -> [(Nom,Etat)] 
nouveauAutomate [] par = []
nouveauAutomate ((x,Etat b tra):l) par = (x,Etat b tra):nouveauAutomate l par      


  --minimiser un automate
minimisation :: AutomateFini -> AutomateFini
minimisation auto = partition2Automate auto (partitionInnitial auto)




----- Bonus ------

type BigString = [String]

executAll :: AutomateFini -> String -> Map Nom Nom
executAll = undefined


paralleleExecution :: AutomateFini -> BigString -> [Map Nom Nom]
paralleleExecution = undefined

parReconnise :: AutomateFini -> BigString -> Bool
parReconnise = undefined


-- a utiliser si librairie indisponible et dans la version rendu

parMapS :: (a -> Map Nom Nom) -> [a] -> [Map Nom Nom]
parMapS = fmap


-- a utiliser si librairie disponible (pas dans la version rendue
--
-- evalDico :: Strategie (Map Nom Nom)
-- evalDico = evalTraversable return
--
-- parMapS :: (a -> Map Nom Nom) -> [a] -> [Map Nom Nom]
-- parMapS = parMap evalDico
