module Printing (showExp) where

import Exp

showVar :: Var -> String
showVar variabila = getVar variabila

showExp :: ComplexExp -> String
showExp (CX x) = showVar x
showExp (CLam x rest) = "\\" ++ (showVar x) ++ "-> {" ++ (showExp rest) ++ "}"
showExp (CApp ex1 ex2) = "{" ++ (showExp ex1) ++ " " ++ (showExp ex2) ++ "}"