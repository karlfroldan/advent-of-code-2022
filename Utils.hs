module Utils where

import Control.Monad (guard)
import Data.Functor ((<&>))
import System.Environment (getArgs)

nonNull :: [a] -> Bool
nonNull = not . null

readFileFromArg :: (String -> a) -> IO [a]
readFileFromArg f = do
  args <- getArgs
  guard (nonNull args)
  let fileName = head args
  contents <- readFile fileName <&> lines
  (return . fmap f) contents
