module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import Puzzle

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxAttempts :: Int
maxAttempts = 7

-- Collects the words to be used in the game according to
-- the globals min & max lengths
gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l > minWordLength && l < maxWordLength

-- Chooses a random word from the list of words
randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIdx <- randomRIO (0, length wl - 1)
  return $ wl !! randomIdx

-- Generates a random word from the game words
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- Provides with how many attempts are left in the puzzle
attemptsLeft :: Puzzle -> Int
attemptsLeft puzzle = maxAttempts - countIncorrect puzzle

-- Communicates with the user after she has input a character
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already used that character, pick something else!"
      return puzzle

    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly."
      return (fillInCharacter puzzle guess)

    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

-- Terminates the game with a lose in case the word has not been guessed
-- and there are not more attempts left
gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ _) =
  if attemptsLeft puzzle == 0 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

-- Terminates the game with a win in case the word has been guessed
gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       putStrLn $ "The word is: " ++ wordToGuess
       exitSuccess
  else return ()

-- Main game loop
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Attempts left: " ++ show (maxAttempts - countIncorrect puzzle)
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character."

-- Program entry point
main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
