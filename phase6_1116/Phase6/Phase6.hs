
module Phase6 where
import Automate
import MonadePerso         -- module fournit

estSousChaine :: [String] -> String -> Bool
estSousChaine [] _ = False
estSousChaine (x:l) w = if estChaine x w then True
                          else estSousChaine l w 


estChaine:: String -> String -> Bool
estChaine [] _ = True
estChaine (x:l) [] = False
estChaine (x:l) (y:w) = if (x==y) then estChaine l w
                        else estChaine (x:l) w


transducteur :: AutomateFiniM WriterSt
transducteur = undefined

annonymisation :: String -> String
annonymisation st = fst $ (runWriter $ (reconnise transducteur st))



autAPile :: AutomateFiniM StateP
autAPile = undefined

palindrome :: String -> Bool
palindrome st = runIsEmpty (reconnise autAPile st) ""
