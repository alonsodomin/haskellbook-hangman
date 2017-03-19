module Puzzle where

import Data.List (intersperse)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map (const Nothing) str) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) ch = elem ch word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) ch = elem ch guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing   = '_'
renderPuzzleChar (Just ch) = ch

countIncorrect :: Puzzle -> Int
countIncorrect (Puzzle word _ guesses) =
  sum $ fmap (\x -> if elem x word then 0 else 1) guesses

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledIn s) ch =
  Puzzle word newFilledIn (addCh s ch)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar

        addCh guessed ch = if elem ch guessed then guessed else ch : guessed

        newFilledIn = zipWith (zipper ch) word filledIn