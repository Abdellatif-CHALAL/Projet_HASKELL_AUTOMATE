module Phase3 where
import Dictionnaire
import Data.Map.Strict
import Data.Set

-- le typage de l'automate fini deterministe

type Nom = String
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

-- exemple pour ecrire (abc)*
etat1 = Etat True (Dictionnaire.inserer creer 'a' "q2")
etat2 = Etat False (Dictionnaire.inserer creer 'b' "q3")
etat3 = Etat False (Dictionnaire.inserer creer 'c' "q1")
monAutomate = AutomateFini (Data.Map.Strict.fromList [("q1",etat1),("q2",etat2),("q3",etat3)]) "q1"

-- une fonction qui enlève les Maybe pour a 
suppJust :: Maybe a -> a
suppJust  (Just a) = a

-- Reconnaissance d'un mot par un automate
reconnise :: AutomateFini -> String -> Bool
reconnise (AutomateFini l i) [] = let Just (Etat b trans) = (Data.Map.Strict.lookup i l) in b
reconnise (AutomateFini l i) (x:xs) = let Just (Etat b trans) =  (Data.Map.Strict.lookup i l) in if rechercherCle trans x
                                                                                                 then reconnise (AutomateFini l (suppJust (rechercherParCle trans x))) xs
                                                                                                 else False

-- créer un automate a partir d'une liste d'états avec leur booleen d'acceptation et d'une liste de transitions
-- l'état initial est le premier nom de la liste de noms
newAutomaton ::[(Nom,Bool)] -> [(Nom,Char,Nom)] -> AutomateFini
newAutomaton l1 ((d,c,a):l2) = let l = newList l1 ((d,c,a):l2) in AutomateFini (Data.Map.Strict.fromList l) d                                


--verifie est ce sue existe déjà le nom dans la liste ou pas si oui renvoie True sinon False  
renvoiBool::[(Nom,Char,Nom)] -> Nom -> Bool
renvoiBool [] n = False
renvoiBool ((n1,c,n2):l) n = if (n1 == n) then True else renvoiBool l n
                          

-- renvoie la liste de couple (Char,Nom) correspondant au Nom de deuxième paramètre  
renvoiListCN::[(Nom,Char,Nom)] -> Nom -> [(Char,Nom)]
renvoiListCN [] n = []
renvoiListCN ((n1,c,n2):l) n = if (n1 == n) then (c,n2):renvoiListCN l n else renvoiListCN l n

--renvoie la liste des états associe à son nom
newList::[(Nom,Bool)] -> [(Nom,Char,Nom)] -> [(Nom,Etat)] 
newList [] l2 = []
newList ((n,b):l1) l2 = if(renvoiBool l2 n) then ((n,(Etat b (Dictionnaire.insererListe Dictionnaire.creer (renvoiListCN l2 n)))):(newList l1 l2)) 
                          else newList l1 l2



-- type des automate infinis du TP2
data EtatD = EtatD Bool (Char->EtatD) 
type AutomateD = EtatD
{-
instance Show EtatD where
         show (EtatD b f) = "EtatD " ++ (show b) ++ (show.f) 
-}

-- transforme la liste AutomateFini vers AutomateD 
forgetFiniteness :: AutomateFini -> AutomateD
forgetFiniteness (AutomateFini a n) = let Just etat =  (Data.Map.Strict.lookup n a) in
                            (EtatD (typeEtat etat) (passerEtatNext (AutomateFini a n) (toSet (transitionsEtat etat))))
                                    
-- fonction qui permet de renvoyer  la trantion  (char -> EtatD)                                    
passerEtatNext :: AutomateFini -> [([Char],Nom)] -> (char -> EtatD)
passerEtatNext (AutomateFini a i) (((x:l1),v):l2) = let Just (Etat b trans) = Data.Map.Strict.lookup v a 
                                            in (\ x-> (EtatD b (passerEtatNext (AutomateFini a i) l2 )))
                                            
-- renvoie la transition depuis l'Etat                                    
transitionsEtat :: Etat -> Transitions
transitionsEtat (Etat b t) = t

-- renvoie le Type d'etat ce forme d'un boolean 
typeEtat :: Etat -> Bool
typeEtat (Etat b a) = b
             
             
-- version non-déterministe
type NomND = String
type TransitionsND = Dictionnaire Char [NomND]  
data EtatND = EtatND Bool TransitionsND 
data AutomateFiniND = AutomateFiniND (Map NomND EtatND) [NomND] 




nommer :: [NomND] -> Nom
nommer l = show (Data.Set.fromList l)



estTerminalList :: [EtatND] -> Bool
estTerminalList [] = False
estTerminalList ((EtatND b dic):l) = b || estTerminalList l  


regroupe :: [TransitionsND] -> TransitionsND
regroupe l = Dictionnaire.unionCons (regMapDico l)
   
   
regMapDico::[TransitionsND] -> [Transitions]
regMapDico [] = []
regMapDico (x:l) = (mapDico nommer x):(regMapDico l)
 


fusionT :: [EtatND] -> TransitionsND
fusionT l = regroupe (listDic l)


listDic:: [EtatND] -> [TransitionsND]
listDic [] = []
listDic ((EtatND _ dic):l) = dic:listDic l 


creerEtat :: [EtatND] -> Etat
creerEtat l = Etat (estTerminalList l)  (mapDico nommer (fusionT l))     



determiniseAux :: (Map NomND EtatND) -> Map Nom Etat -> [[NomND]] -> Map Nom Etat
determiniseAux etatsND acc [] = acc 
determiniseAux etatsND acc (x:l) = let b = (verifieEtatND (Data.Map.Strict.assocs acc) (nommer x)) in  if (b == True)  
                                    then determiniseAux etatsND acc l
                                    else determiniseAux etatsND (Data.Map.Strict.insert (nommer x) (creerEtat (listEtatND x etatsND)) acc) ((tarnsToListVal (listDic (listEtatND x etatsND)))++l)    





etat3_1 = EtatND True (insererListe creer [('a',["q2"])])
etat3_2 = EtatND True (creer) 
monAuto3 =  AutomateFiniND (Data.Map.Strict.fromList [("q1",etat3_1),("q2",etat3_2)] ) ["q1"]



tarnsToListVal::[TransitionsND] -> [[NomND]]
tarnsToListVal [] = []
tarnsToListVal (dic:l) = (toListVal dic)++tarnsToListVal l 


listEtatND::[NomND] -> Map NomND EtatND -> [EtatND]
listEtatND [] etatsND = []
listEtatND (x:l) etatsND = let b = (verifieEtatND (Data.Map.Strict.assocs etatsND) x) in if (b == False) then listEtatND l etatsND                                            
                                                                         else (suppJust (Data.Map.Strict.lookup x etatsND)):(listEtatND l etatsND)


verifieEtatND::(Eq a)=> [(a,b)]-> a -> Bool
verifieEtatND [] x = False
verifieEtatND ((y,_):l) x =  if (x == y) then True else verifieEtatND l x 

verifieEtat::[(Nom,Etat)]-> Nom -> Bool
verifieEtat [] x = False
verifieEtat ((y,_):l) x =  if (x == y) then True else verifieEtat l x 


determinise :: AutomateFiniND -> AutomateFini
determinise  (AutomateFiniND l1 l2) = (AutomateFini  (determiniseAux l1 (Data.Map.Strict.empty) [l2]) (nommer l2))   

-- créer un automate qui reconnaît un caractère compris dans un segment donné
segment :: Char -> Char -> AutomateFiniND
segment c1 c2 = (AutomateFiniND (Data.Map.Strict.fromList [("q1",EtatND False (Dictionnaire.insererListe creer (associeVal ["q2"] [c1..c2]))),("q2",EtatND True creer)]) ["q1"])


associeVal:: [b] -> [a] -> [(a,[b])]
associeVal [val] [] = []
associeVal [val] (x:l) = (x,[val]):(associeVal [val] l)


etat1_1 = EtatND False (Dictionnaire.inserer creer 'a' ["q1","q2"])
etat1_2 = EtatND True (creer)
etat1_3 = EtatND False (Dictionnaire.inserer creer 'b' ["q1","q2"])
monAuto1 = AutomateFiniND (Data.Map.Strict.fromList [("q1",etat1_1),("q2",etat1_2)]) ["q1"]
monAuto2 = AutomateFiniND (Data.Map.Strict.fromList [("q1",etat1_3),("q2",etat1_2)]) ["q1"]

etat2_1 = EtatND False (Dictionnaire.inserer creer 'a' ["q1","q2"])
etat2_2 = EtatND False (Dictionnaire.inserer creer 'c' ["q3"])
etat2_3 = EtatND True (Dictionnaire.inserer creer 'c' ["q3"])
etat2_4 = EtatND False (Dictionnaire.insererListe creer [('b',["q1","q2"]),('c',["q1"]),('c',["q3"])])
etat2_5 = EtatND True (creer)
monAuto1' = AutomateFiniND (Data.Map.Strict.fromList [("q1",etat2_1),("q2",etat2_2),("q3",etat2_3)]) ["q1"]
monAuto2' = AutomateFiniND (Data.Map.Strict.fromList [("q1",etat2_4),("q2",etat2_5),("q3",etat2_5)]) ["q1"]

mapkv = (Data.Map.Strict.fromList [("q1",etat2_1),("q2",etat2_2),("q3",etat2_3)])
 
renommerListe :: [NomND] -> (Map NomND EtatND) -> [NomND]
renommerListe [] l = []
renommerListe (x:ln) l = if Data.Map.Strict.member x l
                         then [(x++"'")]++(renommerListe ln l)
                         else [x]++(renommerListe ln l)

renommerDic:: [(Char,[NomND])] -> (Map NomND EtatND) -> [(Char,[NomND])]
renommerDic [] l = []
renommerDic ((a,ln):ll) l = [(a,(renommerListe ln l))]++(renommerDic ll l)
               

nouveauL :: (Map NomND EtatND) -> (Map NomND EtatND) -> [(NomND,EtatND)]
nouveauL l1 l2 =  do 
                  (a,b) <- (Data.Map.Strict.toList l2)
                  if Data.Map.Strict.member a l1
                  then let (EtatND bool dic) = b
                       in let ll = (toListCleVal dic) 
                          in let nouveau = renommerDic ll l1
                             in let dicn = (insererListe creer nouveau) 
                                in let bn = (EtatND bool dicn)
                                   in 
                                       [((a++"'"),bn)]
                  else [(a,b)] 

nouveauN :: [NomND] -> [NomND] -> [NomND]
nouveauN n1 n2 = do 
                r <- n2
                if elem r n1
                then [r++"'"]
                else [r]


-- créer un automate qui reconnaît la somme des langages reconnus par les automates entres
(!+!) :: AutomateFiniND -> AutomateFiniND -> AutomateFiniND
(!+!) (AutomateFiniND l1 n1) (AutomateFiniND l2 n2) = (AutomateFiniND (Data.Map.Strict.union l1 (Data.Map.Strict.fromList (nouveauL l1 l2))) (n1++(nouveauN n1 n2))) 


                                             


changeEtatInit ::  [(NomND,EtatND)] -> [NomND] -> [(NomND,EtatND)]
changeEtatInit [] n =[]
changeEtatInit ((a,EtatND b dic):ll) n = if elem a n
                                         then ((a,EtatND True dic):(changeEtatInit ll n))
                                         else ((a,EtatND b dic):(changeEtatInit ll n))

changeEtatFi :: [(NomND,EtatND)] -> [NomND] -> [(NomND,EtatND)]   
changeEtatFi [] n = []
changeEtatFi ((a,EtatND b dic):ll) n = if b == True 
                                       then ((a,EtatND b (inserer dic ' ' n)):(changeEtatFi ll n))
                                       else ((a,EtatND b dic):(changeEtatFi ll n))


-- créer un automate qui reconnaît l'étoile du langage reconnu par l'automate entre
star :: AutomateFiniND -> AutomateFiniND
star (AutomateFiniND l n) = (AutomateFiniND (Data.Map.Strict.fromList (changeEtatInit (changeEtatFi (Data.Map.Strict.toList l) n) n)) n)




-- supprimer etats finaux et non initiaux de A
supEtatFi :: [(NomND,EtatND)] -> [NomND] -> [(NomND,EtatND)]
supEtatFi [] n = [] 
supEtatFi ((a,(EtatND b dic)):l) n = if ( b == True) && ((elem a n) == False) -- etats terminaux non initiaux
                                     then (supEtatFi l n)
                                     else ((a,(EtatND b dic)):(supEtatFi l n))



                                     
supList :: [NomND] -> (Map NomND EtatND)  -> [NomND]
supList [] l = []
supList (x:ll) l  = if Data.Map.Strict.member x l
                    then (x:(supList ll l))  
                    else ll


existe :: [NomND] -> (Map NomND EtatND) -> Bool
existe [] l = False
existe (x:ln) l = if Data.Map.Strict.member x l
                then True
                else existe ln l 


nouveauDic :: [(Char,[NomND])] ->  (Map NomND EtatND) ->[NomND] -> [(Char,[NomND])]
nouveauDic [] l i = []
nouveauDic ((a,ln):ll) l i = if existe ln l
                             then ((a,ln):(nouveauDic ll l i))
                             else ((a,((supList ln l)++i)):(nouveauDic ll l i))
                          

newL :: [(NomND,EtatND)] -> [NomND] -> [(NomND,EtatND)]
newL l1 n2 = do 
             (a,b) <- l1
             let (EtatND bool dic) = b 
             let dicn = (insererListe creer (nouveauDic (toListCleVal dic) (Data.Map.Strict.fromList l1) n2)) 
             let bn = (EtatND bool dicn)
             [(a,bn)]
                                         
-- créer un automate qui reconnaît la concaténation des langages reconnus par les automates entres
(!.!) :: AutomateFiniND -> AutomateFiniND -> AutomateFiniND
(!.!) (AutomateFiniND l1 n1) (AutomateFiniND l2 n2) = let l = Data.Map.Strict.fromList (newL (supEtatFi (Data.Map.Strict.toList l1) n1) (nouveauN n1 n2)) in
                                                      let ll =  Data.Map.Strict.fromList (nouveauL l l2) in
                                                      (AutomateFiniND (Data.Map.Strict.union l ll) n1)

-- type des expressions régulières
data Regexp = Segment Char Char  |  Regexp :+: Regexp  |  Star Regexp  |  Regexp :.: Regexp


-- construire un automate reconnaissant le même langage que l'expression régulière entrée
regexp2Automata :: Regexp -> AutomateFini  
regexp2Automata regexp = determinise (regexp2Aut regexp) 

-- construire un automateND reconnaissant le même langage que l'expression régulière entrée
regexp2Aut :: Regexp -> AutomateFiniND
regexp2Aut (Segment a b) = (segment a b)
regexp2Aut (r1 :+: r2) = ((!+!) (regexp2Aut r1) (regexp2Aut r2))
regexp2Aut (Star r) = (star (regexp2Aut r))
regexp2Aut (r1 :.: r2) = ((!.!) (regexp2Aut r1) (regexp2Aut r2))



-- minimiser un automate (bonus)
minimisation :: AutomateFini -> AutomateFini
minimisation = undefined
