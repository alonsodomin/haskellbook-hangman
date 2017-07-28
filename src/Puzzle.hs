module Puzzle where

import Data.List (intersperse)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Used so far: " ++ guessed

-- Creates a fress new Puzzle in using the provided in a string as the word to guess
freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map (const Nothing) str) []

-- Checks if a specific character is part of the word to be guessed
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) ch = elem ch word

-- Checks if a specific character has already been guessed in the puzzle
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) ch = elem ch guessed

-- Renders a single character from the puzzle
-- If the character has not been guessed yet, it'd be renderered as an underscode
-- Characters that have been rendered will be rendered as they are.
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = maybe '_' id

-- Counts the number of incorrect characters
countIncorrect :: Puzzle -> Int
countIncorrect (Puzzle word _ guesses) =
  sum $ fmap (\x -> if elem x word then 0 else 1) guesses

-- Inputs the given character into the puzzle
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledIn s) ch =
  Puzzle word newFilledIn (addCh s ch)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar

        addCh guessed ch = if elem ch guessed then guessed else ch : guessed

        newFilledIn = zipWith (zipper ch) word filledIn
