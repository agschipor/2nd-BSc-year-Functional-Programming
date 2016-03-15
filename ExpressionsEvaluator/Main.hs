module Main where

import Parser
import System.Environment
import qualified Data.Map as Map

getExtension filename = reverse (take 4 (reverse (filename)))

main = do
  args <- getArgs

  if (length (args!!0) <= 4) || (getExtension (args!!0) /= ".exp")
    then 
      error "Error: unknown extension(only '.exp' files)"
    else
      do
        content <- readFile $ if (length args) == 0
                                then
                                  error "Error: too few arguments!"
                                else
                                  args!!0

        let expressionLines = lines content

        if length expressionLines == 0 
          then error "The document is empty!"
        else
          do
            let variables = Map.empty

            startEvaluation expressionLines  variables