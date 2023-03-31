module Eval where

import Exp
import Data.List ( union, delete, nub )

vars :: Exp -> [IndexedVar]
vars (X x) = [x]
vars (Lam x exp) = x : (vars exp)
vars (App exp1 exp2) = (vars exp1) ++ vars(exp2) 


elimIfFound _ [] = [] 
elimIfFound x (y:ys) =
    if x == y then
        elimIfFound x ys
    else
        y : (elimIfFound x ys)

freeVars :: Exp -> [IndexedVar]
freeVars (X x) = [x]
freeVars (Lam x exp) = elimIfFound x (freeVars exp)
freeVars (App exp1 exp2) = (freeVars exp1) ++ (freeVars exp2)


occursFree :: IndexedVar -> Exp -> Bool
occursFree var exp = var `elem` (freeVars exp) 


freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar var list = 
    IndexedVar (ivName var) ((maxFromList (ivName var) (var:list)) + 1)
    where
        maxFromList name [] = 0
        maxFromList name (x:xs) = 
            if ivName x == name then
                max (ivCount x) (maxFromList name xs)
            else
                maxFromList name xs


renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement (X var) = 
    if var == toReplace then
        X replacement
    else
        X var
renameVar toReplace replacement (Lam var exp) = 
    if var == toReplace then
        Lam replacement (renameVar toReplace replacement exp)
    else
        Lam var (renameVar toReplace replacement exp)

renameVar toReplace replacement (App exp1 exp2) = 
    App (renameVar toReplace replacement exp1) (renameVar toReplace replacement exp2)


substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement (X var) =
    if toReplace == var then
        replacement
    else
        X var

substitute toReplace replacement (App exp1 exp2) = 
    App (substitute toReplace replacement exp1) (substitute toReplace replacement exp2)

substitute toReplace replacement (Lam var exp) = 
    if var == toReplace then
        Lam var exp
    else
        if var `occursFree` replacement then
            let newVar = freshVar var ((freeVars exp) ++ (freeVars replacement))
                newExpr = renameVar var newVar (Lam var exp) 
            in
                substitute toReplace replacement newExpr
        else
            Lam var (substitute toReplace replacement exp)
    

normalize :: Exp -> Exp
normalize (X var) = (X var)
normalize (App (Lam var expM) expN) = normalize (substitute var expN expM)
normalize (Lam var exp) = (Lam var (normalize exp))
normalize x = x

