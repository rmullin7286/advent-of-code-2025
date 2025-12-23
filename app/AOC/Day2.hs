module AOC.Day2 (day2, part2, Range (..), chunksOf, allEqual) where

import AOC.Answers (Day (Day), Parser, mkPart)
import AOC.Util (pairs)
import Control.Arrow ((&&&))
import Data.List (unfoldr)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

data Range = Range !Int !Int

rangeToList :: Range -> [Int]
rangeToList (Range x y) = [x .. y]

allEqual :: (Eq a) => [a] -> Bool
allEqual = all (uncurry (==)) . pairs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr $ \case
  [] -> Nothing
  xs -> Just $ splitAt n xs

equalSegments :: Int -> String -> Bool
equalSegments n = allEqual . chunksOf n

part1 :: [Range] -> Int
part1 =
  sum
    . map fst
    . filter (repeatedTwice . snd)
    . map (id &&& show)
    . concatMap rangeToList
  where
    repeatedTwice [_] = False
    repeatedTwice xs = equalSegments (length xs `div` 2) xs

part2 :: [Range] -> Int
part2 =
  sum
    . map fst
    . filter (anyEqualSegments . snd)
    . map (id &&& show)
    . concatMap rangeToList
  where
    anyEqualSegments = any <$> flip equalSegments <*> (enumFromTo 1 . ((`div` 2) . length))

parser :: Parser [Range]
parser = range `sepBy` char ','
  where
    range = Range <$> (decimal <* char '-') <*> decimal

day2 :: Day
day2 =
  Day
    2
    "data/day2.txt"
    [ mkPart 1 parser part1,
      mkPart 2 parser part2
    ]
