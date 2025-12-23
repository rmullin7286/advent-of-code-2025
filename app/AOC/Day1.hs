module AOC.Day1 (day1) where

import AOC.Answers (Day (..), Parser, mkPart, program)
import Control.Applicative ((<|>))
import Data.List (foldl', scanl')
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

dialStart :: Int
dialStart = 50

data Direction = L | R

dirInt :: Direction -> Int
dirInt L = -1
dirInt R = 1

data Step = Step Direction Int

part1 :: [Step] -> Int
part1 =
  length
    . filter (== 0)
    . scanl' applyStep dialStart
  where
    applyStep !i (Step d n) = (i + (dirInt d * n)) `mod` 100

part2 :: [Step] -> Int
part2 = snd . foldl' applyStep (dialStart, 0)
  where
    applyStep :: (Int, Int) -> Step -> (Int, Int)
    applyStep (!dial, !zeroCount) (Step d n) = (newDial, zeroCount + numTimesPassedZero)
      where
        afterTurn = dial + (dirInt d * n)
        afterTurnMod = afterTurn `mod` 100
        passedOrReachedZero =
          if
            | afterTurn == 0 -> 1
            | dial > 0 && afterTurn < 0 -> 1
            | otherwise -> 0
        numTimesPassed100 = abs afterTurn `div` 100
        numTimesPassedZero = passedOrReachedZero + numTimesPassed100
        newDial = if afterTurnMod < 0 then 100 + afterTurnMod else afterTurnMod

parser :: Parser [Step]
parser = step `sepEndBy` char '\n'
  where
    step = Step <$> dir <*> decimal
    dir = (L <$ char 'L') <|> (R <$ char 'R')

day1 :: Day
day1 =
  Day
    1
    "data/day1.txt"
    [ mkPart 1 parser part1,
      mkPart 2 parser part2
    ]
