import Control.Monad (guard)
import Data.Functor ((<&>))
import Data.List (foldl', sort)
import System.Environment (getArgs)
import Utils

data Elf a = Elf [a] deriving (Eq, Show)

toElf :: [a] -> (a -> b) -> Elf b
toElf xs f = Elf (f <$> xs)

elfTotalCalories :: (Num a) => Elf a -> a
elfTotalCalories (Elf xs) = sum xs

groupListOfStrings :: [String] -> [[String]]
groupListOfStrings = foldl' groupList [[]]
  where
    groupList (xs : xss) "" = [] : (xs : xss)
    groupList (xs : xss) st = ((st : xs) : xss)
    groupList xss _ = xss

maxN :: (Ord a) => Int -> [a] -> [a]
maxN n = take n . reverse . sort

main :: IO ()
main = do
  contents <- readFileFromArg id <&> groupListOfStrings
  let elves = fmap (\xs -> (toElf xs read :: Elf Integer)) contents
  print ((sum . maxN 3) (elfTotalCalories <$> elves))
