module Sugar where

import Exp
import Data.Char


desugarVar :: Var -> IndexedVar
desugarVar (Var x) = IndexedVar x 0 


sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar name index) = 
    if index == 0 then
        Var name
    else
        Var (name ++ "_" ++ (intToString index)) 
    where 
        intToString 0 = []
        intToString x = intToDigit (x `mod` 10) : (intToString (x `div` 10))


desugarExp :: ComplexExp -> Exp
desugarExp (CX x) = X (desugarVar x)
desugarExp (CLam x cExp) = Lam (desugarVar x) (desugarExp cExp) 
desugarExp (CApp c1 c2) = App (desugarExp c1) (desugarExp c2)

sugarExp :: Exp -> ComplexExp
sugarExp (X x) = CX (sugarVar x)
sugarExp (Lam iv exp) = CLam (sugarVar iv) (sugarExp exp)
sugarExp (App exp1 exp2) = CApp (sugarExp exp1) (sugarExp exp2)