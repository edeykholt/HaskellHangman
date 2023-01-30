{-# LANGUAGE TupleSections #-}
module A8 where

import Provided
import A6 hiding ( validateNoDict, validateWithDict )
import A7

import Control.Monad
import Control.Monad.State
import Data.Char (toUpper)

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
playGame g = do
    promptGuess
    move <- getUpperChar
    _SPACE_
    case processTurn move g of 
      Left GameOver     -> print GameOver
      Left ex           -> print ex >> playGame g
      Right updatedGame -> do
        print updatedGame
        if game_secretWord updatedGame == game_currentGuess updatedGame
            then print "You've won!"
            else playGame updatedGame
-- *A8> gg = makeGame "picachu"
-- *A8> :t gg
-- gg :: Game
-- *A8> playGame gg
--Guess?  P
--
-- **************************************************
--        Current Guess:  P _ _ _ _ _ _
--        Guessed:        P
--        Remaining Chances:      7
-- **************************************************
--
-- Guess?  i
--
-- **************************************************
--        Current Guess:  P I _ _ _ _ _
--        Guessed:        I P
--        Remaining Chances:      7
--
-- **************************************************
-- Guess?  a
-- **************************************************
--     Current Guess:  P I _ A _ _ _
--        Guessed:        A I P
--        Remaining Chances:      7
-- **************************************************
--Guess?  a
-- Repeated move. The character was previously used. Try another.


-- Q#03
startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame f = do
    secret <- setSecret
    -- eges :: Either GameException Secret
    let eges = f secret
    case makeGameIfValid eges of
      Left ge -> print ge >> startGame f
      Right ga -> print ga >> playGame ga
-- *A8> startGame validateNoDict
-- Enter a secret word:
-- Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.
-- Enter a secret word:
--
-- **************************************************
--        Current Guess:  _ _ _ _ _ _ _
--        Guessed:
--        Remaining Chances:      7
-- **************************************************
-- Guess?  a
-- **************************************************
--        Current Guess:  _ _ _ A _ _ _
--        Guessed:        A
--        Remaining Chances:      7
-- **************************************************

-- Q#04
runApp :: IO ()
runApp = do
    maybeDict <- getDict
    case maybeDict of
      Nothing -> do
        print "Missing dictionary file! Continue without dictionary? [Y/N]"
        char <- getUpperChar
        when (char == 'Y') (startGame validateNoDict)
      Just dict -> do
        let f = validateWithDict dict
        startGame f 
-- *A8> runApp
-- Enter a secret word:
-- **************************************************
--        Current Guess:  _ _ _ _ _
--        Guessed:
--        Remaining Chances:      7
-- **************************************************
       

-- Q#05
makeGameS :: Secret -> State Game ()
makeGameS s = 
  put newState
  where newState = Game (map toUpper s) (map (const '_') s ) "" _CHANCES_
-- untested

updateGameS :: Move -> State Game ()
updateGameS m = modify (updateGame m)
-- untested

updateGameS' :: Move -> State Game ()
updateGameS' m = do
  Game secretWord currentGuess usedMoves remainingChances <- get
  let
    mUpper       = toUpper m
    newGuess     = revealLetters mUpper secretWord currentGuess 
    newUsedMoves = if mUpper `elem` usedMoves then usedMoves else mUpper : usedMoves
    newChances   = if isGoodGuess then remainingChances else remainingChances - 1 
    isGoodGuess  = mUpper `elem` secretWord
  put $ Game secretWord newGuess newUsedMoves newChances
-- untested

repeatedMoveS :: Move -> State Game Bool
repeatedMoveS m = 
  do
    usedMoves <- gets game_usedMoves
    let isRepeatedMove = toUpper m `elem` usedMoves 
    pure isRepeatedMove
--untested 

processTurnS :: Move -> State Game (Either GameException ())
processTurnS m  
  | invalidMove m = pure $ Left InvalidMove
  | otherwise     = do
      isRepeatedMove <- repeatedMoveS m
      if isRepeatedMove 
        then do
          pure $ Left RepeatMove
        else do
            Game _ _ _ remainingChances <- get
            if remainingChances == 0
              then pure $ Left GameOver
              else pure $ Right ()
-- untested     
          
