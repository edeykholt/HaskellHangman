module A6 where

import Provided

import Data.List ( intersperse, sort )
import Data.Char (isAlpha, isLetter)

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
revealLetters m s g = zipWith (\sn gn -> if m == sn then m else gn) s g
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
data Game

-- Q#09

repeatedMove = undefined

-- Q#10

makeGame = undefined

-- Q#11

updateGame = undefined

-- Q#12

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]


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