module A8 where

import Provided
import A6 hiding ( validateNoDict, validateWithDict )
import A7

import Control.Monad
import Control.Monad.State

-- *** A8: Monads *** --

-- Q#01
validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = isValidLength s >>= hasValidChars
-- *A8> validateNoDict "asdf"
-- Right "asdf"
-- *A8> validateNoDict "asdfasdfasdfasdfasdfasdfasdfasdf"
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.
-- *A8> validateNoDict "222222"
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = validateNoDict s >>= isInDict d
-- *A8> dict = ["asdf", "ASDF", "a", "picachu", "supercalifragilisticexpialidocious"]
-- *A8> validateWithDict dict "asdf"
-- Right "asdf"
-- *A8> validateWithDict dict "2"
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.

-- Q#02
playGame :: Game -> IO ()
playGame = undefined

-- Q#03

startGame = undefined

-- Q#04

runApp :: IO ()
runApp = putStrLn "Welcome to Part II of EMURGO Academy's Haskell course!"

-- Q#05

makeGameS = undefined


updateGameS = undefined


repeatedMoveS = undefined


processTurnS = undefined