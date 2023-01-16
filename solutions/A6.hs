module A6 where

import Provided

import Data.List ( intersperse, sort )
import Data.Char (isAlpha, isLetter, toUpper)
-- import Text.Pandoc.Shared (capitalize)

-- *** A6-0: WARM-UP *** --

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02
data GameException = InvalidWord | InvalidMove | RepeatMove | GameOver
-- > :t InvalidWord
--   InvalidWord :: GameException


-- Q#03
lengthInRange :: Secret -> Bool
lengthInRange s = l >= fst _LENGTH_ && l <= snd _LENGTH_
  where l = length s
-- *A6> lengthInRange "aa"
-- False
-- *A6> lengthInRange "aaa"
-- True
-- *A6> lengthInRange "aaaaaaaaaaaaaaaaaaaa"
-- -- True
-- *A6> lengthInRange "aaaaaaaaaaaaaaaaaaaax"
-- False

-- Q#04
invalidMove :: Move -> Bool
invalidMove move = not $ isLetter move
-- *A6> invalidMove 'm'
-- False
-- *A6> invalidMove '1'
-- True

-- Q#05
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters m s g = zipWith (\sChar gChar -> if m == sChar then m else gChar) s g
-- *A6> revealLetters 'a' "balbasar" "________"
-- "_a__a_a_"
-- *A6> revealLetters 'b' "balbasar" "_a__a_a_"
-- "ba_ba_a_"

-- Q#06
updateChances :: Move -> Secret -> Chances -> Chances
updateChances m s c = if elem m s then c else c - 1
-- *A6> updateChances 'a' "balbasar" 10
-- 10
-- *A6> updateChances 'x' "balbasar" 10
-- 9

-- Q#07
setSecret :: IO String
setSecret = do
  putStr "Enter a secret word:\t"
  showInput False
  c <- getLine
  showInput True
  _SPACE_
  return c
-- *A6> setSecret
-- Enter a secret word:
-- "balbsar"

-- *** A6-1: Records & Instances *** --

-- Q#08
data Game = Game {
    game_secretWord       :: Secret
  , game_currentGuess     :: Guess
  , game_usedMoves        :: [Move]
  , game_remainingChances :: Int
  }
  -- deriving (Show)  
-- *A6> Game "balbasar" "_a___a_a__" "aq" 8
-- Game {game_secretWord = "balbasar", game_currentGuess = "_a___a_a__", game_usedMoves = "aq", game_remainingChances = 8}

-- Q#09
repeatedMove :: Move -> Game -> Bool
repeatedMove m g = elem m (game_usedMoves g)
-- *A6> repeatedMove 'a' (Game "balbasar" "_a___a_a__" "aq" 8)
-- True
-- *A6> repeatedMove 'a' $ Game "balbasar" "_a___a_a__" "aq" 8
-- True
-- *A6> repeatedMove 'q' $ Game "balbasar" "_a___a_a__" "aq" 8
-- True
-- *A6> repeatedMove 'x' $ Game "balbasar" "_a___a_a__" "aq" 8
-- False

-- Q#10
makeGame :: Secret -> Game
makeGame s = Game (map (\ss -> toUpper ss) s) (map (const '_') s ) "" _CHANCES_
-- *A6> makeGame "BalbasaR" 
-- Game {game_secretWord = "BALBASAR", game_currentGuess = "________", game_usedMoves = "", game_remainingChances = 7}

-- Q#11
updateGame :: Move -> Game -> Game
updateGame m g = Game (game_secretWord g) newGuess newUsedMoves newChances
  where
    mUpper :: Move
    mUpper = toUpper m
    newGuess = revealLetters mUpper (game_secretWord g) (game_currentGuess g) 
    newUsedMoves = if repeatedMove mUpper g then game_usedMoves g else mUpper : game_usedMoves g
    newChances = if isGoodGuess then game_remainingChances g else updateChances m (game_secretWord g) (game_remainingChances g) 
    isGoodGuess = mUpper `elem` game_secretWord g
-- *A6> updateGame 'b' (makeGame "balbasar")j
-- Game {game_secretWord = "BALBASAR", game_currentGuess = "B__B____", game_usedMoves = "B", game_remainingChances = 7}
-- *A6> updateGame 'x' (makeGame "balbasar")
-- Game {game_secretWord = "BALBASAR", game_currentGuess = "________", game_usedMoves = "X", game_remainingChances = 6}

-- Q#12

-- showGameHelper :: String -> [Char] -> Int -> String
-- showGameHelper game moves chances = unlines [
--      _STARS_
--    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
--    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
--    , "\tChances:\t" ++ show chances
--    , _STARS_
--    ]



showGameHelper :: Game -> String
showGameHelper game = unlines [
  _STARS_
  , "\tCurrent Guess:\t" ++ intersperse ' ' (game_currentGuess game) ++ "\n"
  , "\tGuessed:\t" ++ intersperse ' ' (sort (game_usedMoves game)) ++ "\n"
  , "\tRemaining Chances:\t" ++ show (game_remainingChances game)
  , _STARS_ 
  ]
-- *A6> show $ showGameHelper2 $ makeGame "balbasar"
-- "\"\\n**************************************************\\n\\n\\tCurrent Guess:\\t_ _ _ _ _ _ _ _\\n\\n\\tGuessed:\\t\\n\\n\\tRemaining Chances:\\t7\\n\\n**************************************************\\n\\n\""

instance Show Game where
  show game = showGameHelper game

-- *A6> putStrLn $ show $ makeGame "balbasar"
-- 
-- **************************************************
-- 
--        Current Guess:  _ _ _ _ _ _ _ _
--
--        Guessed:
--
--        Remaining Chances:      7
-- 
-- **************************************************

-- Q#13


-- *** A6-2: Exception Contexts *** --

-- Q#14

toMaybe = undefined

-- Q#15

validateSecret = undefined

-- Q#16

hasValidChars = undefined


isValidLength = undefined


isInDict = undefined

-- Q#17

validateNoDict = undefined

validateWithDict = undefined

-- Q#18

processTurn = undefined