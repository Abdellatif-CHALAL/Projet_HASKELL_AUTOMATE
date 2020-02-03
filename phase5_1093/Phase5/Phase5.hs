
module Phase5 where
import Dictionnaire
import Data.Map.Strict


type Nom = Int
type TransitionsM mon = Dictionnaire Char (mon Nom)
data EtatM mon = EtatM (mon ()) (TransitionsM mon)
data AutomateFiniM mon = AutomateFiniM (Map Nom (EtatM mon)) (mon Nom)

type AutomateFini = AutomateFiniM Maybe
type AutomateFiniND = AutomateFiniM []

-- les types suivants peuvent être utilisés à la place
-- si vous savez vous en servir :
--
-- data Etat = Etat { getFinal :: Bool
--                  , getTrans :: Transitions
--                  }
-- data AutomateFini = AutomateFini { getEtats   :: Map Nom Etat
--                                  , getInitial :: Nom
--                                  }


-- Reconnaissance d'un mot par un automate
reconnise :: Monad mon => AutomateFiniM mon -> String -> mon ()
reconnise = undefined


-- créer un automate a partir d'une liste d'états avec leur booleen d'acceptation et d'une liste de transitions
-- l'état initial est le premier nom de la liste de noms et on n'a pas d'effet innitial (on utilise return)
newAutomaton :: (Monad mon, Eq (mon Nom)) => [(Nom,mon ())] -> [(Nom,Char,mon Nom)] -> AutomateFiniM mon
newAutomaton = undefined


-- type des automate infinis du TP2
data EtatD = EtatD Bool (Char->EtatD)
type AutomateD = EtatD

-- projeter sur le type du TP2
forgetFiniteness :: AutomateFini -> AutomateD
forgetFiniteness = undefined


-- fonction de determinisation
determinise :: AutomateFiniND -> AutomateFini
determinise = undefined


-- créer un automate qui reconnaît un caractère compris dans un segment donné
segment :: Char -> Char -> AutomateFiniND
segment = undefined


-- créer un automate qui reconnaît la somme des langages reconnus par les automates entres
(!+!) :: AutomateFiniND -> AutomateFiniND -> AutomateFiniND
(!+!) = undefined


-- créer un automate qui reconnaît l'étoile du langage reconnu par l'automate entre
star :: AutomateFiniND -> AutomateFiniND
star = undefined


-- créer un automate qui reconnaît la concaténation des langages reconnus par les automates entres
(!.!) :: AutomateFiniND -> AutomateFiniND -> AutomateFiniND
(!.!) = undefined


-- type des expressions régulières
data Regexp = Segment Char Char  |  Regexp :+: Regexp  |  Star Regexp  |  Regexp :.: Regexp


-- construire un automate reconnaissant le même langage que l'expression régulière entrée
regexp2Automata :: Regexp -> AutomateFini
regexp2Automata = undefined



-- Automates figés

type TransitionsFM mon = Dictionnaire Char (mon (EtatFM mon))
data EtatFM mon = EtatFM (mon ()) (TransitionsFM mon)
type AutomateFiniFM mon = mon (EtatFM mon)

figer :: (Monad mon) => AutomateFiniM mon -> AutomateFiniFM mon
figer = undefined

reconiseF :: (Monad mon) => AutomateFiniFM mon -> String -> mon ()
reconiseF = undefined



-- minimiser un automate
minimisation :: AutomateFini -> AutomateFini
minimisation = undefined


----------------- Projet avancé -----------------------------


class (Monad mon) => Determinisable mon where
  distrib :: [mon a] -> mon [a] 
  proj    :: [mon a] -> [a] 

type NomND = Int
type TransitionsNDM mon = Dictionnaire Char [mon NomND]
data EtatNDM mon = EtatNDM (mon ()) (TransitionsNDM mon)
data AutomateFiniNDM mon = AutomateFiniNDM (Map Nom (EtatNDM mon)) [mon Nom]



-- fonction de determinisation
determinise2 ::  (Determinisable mon, Eq (mon Nom)) =>
                 AutomateFiniNDM mon -> AutomateFiniM mon
determinise2 = undefined



-- créer un automate qui reconnaît la somme des langages reconnus par les automates entres
(<+>) :: (Determinisable mon, Eq (mon Nom)) =>
         AutomateFiniNDM mon -> AutomateFiniNDM mon -> AutomateFiniNDM mon
(<+>) = undefined

effet :: AutomateFiniNDM mon -> mon () -> AutomateFiniNDM mon
effet = undefined


-- créer un automate qui reconnaît l'étoile du langage reconnu par l'automate entre
starM :: (Determinisable mon, Eq (mon Nom)) =>
         AutomateFiniNDM mon -> AutomateFiniNDM mon 
starM = undefined


(<.>) :: (Determinisable mon, Eq (mon Nom)) =>
         AutomateFiniNDM mon -> AutomateFiniNDM mon -> AutomateFiniNDM mon 
(<.>) = undefined

