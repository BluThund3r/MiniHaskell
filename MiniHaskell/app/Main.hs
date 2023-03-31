module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand
import Data.Maybe (fromMaybe)
import Sugar
import Eval


main :: IO ()
main = do
    putStr "main>"
    line <- getLine
    let comanda = parseFirst replCommand line
    case comanda of
        (Just Quit) -> return ()
        (Just (Load _)) -> main
        (Just (Eval str)) -> doEvalStuff str
    
    where
        doEvalStuff str = do
            expr <- return (parseFirst exprParser str)
            let desugared = desugarExp (fromMaybe (CX $ Var "") expr)
            let normalized = normalize desugared
            let sugared = sugarExp normalized
            putStrLn (showExp sugared)
            main

-- TODO: UITA-TE LA parenExp
