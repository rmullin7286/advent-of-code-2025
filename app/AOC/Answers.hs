module AOC.Answers (Day (..), mkPart, program, Parser) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec, errorBundlePretty, parse)

-- | All parsing is based on megaparsec
type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void

data Day = Day Int FilePath [Part]

-- | A "Part" takes in the file text input and responds with an answer
data Part = Part Int (Text -> Either ParseError String)

mkPart :: (Show b) => Int -> Parser a -> (a -> b) -> Part
mkPart i parser answer = Part i wrapped
  where
    wrapped input = show . answer <$> parse parser "" input

program :: [Day] -> IO ()
program = mapM_ runDay
  where
    runDay (Day i file parts) = do
      putStrLn $ "Day " ++ show i
      input <- TIO.readFile file
      forM_ parts $ \(Part parti f) -> do
        putStr $ "    Part " ++ show parti ++ ": "
        case f input of
          Left err -> putStrLn $ "Failed - " ++ errorBundlePretty err
          Right v -> putStrLn v
