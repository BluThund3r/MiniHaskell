module REPLCommand where

import Lab2
import Parsing
import Control.Applicative (many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String
  deriving (Show)

-- TODO: pare in regula, dar intreba totusi daca e ok
replCommand :: Parser REPLCommand
replCommand = quitCase <|> loadCase <|> evalCase
  where quitCase = do
          (symbol ":q" >> endOfInput) <|> (symbol ":quit" >> endOfInput)
          return Quit

        loadCase = do
          command <- (symbol ":l" >> space) <|> (symbol ":load" >> space)
          restString <- many anychar
          return (Load restString)

        evalCase = do
          restString <- many anychar
          return (Eval restString)

