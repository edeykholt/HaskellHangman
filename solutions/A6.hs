module A6 where

import Provided

import Data.List ( intersperse, sort )
import Data.Char (isAlpha, isLetter, toUpper, toLower)
import Control.Monad.Except (ExceptT(ExceptT))
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
updateChances m s c = if elem (toUpper m) s then c else c - 1
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
instance Show GameException where
  show ge = case ge of
    InvalidMove -> "Invalid move. Must use an alphabetic character."
    InvalidWord -> "Invalid word. A secret word must be in the dictionary and be between " ++ show (fst _LENGTH_) ++ " and " ++ show (snd _LENGTH_) ++ " alphabetic characters."
    RepeatMove -> "Repeated move. The character was previously used. Try another."
    GameOver -> "Game over, Man!"
    -- _ -> "unexpected exception"

-- *A6> show InvalidWord
-- "Invalid word. A secret word must be between 3 and 20 alphabetic characters."

-- *** A6-2: Exception Contexts *** --

-- Q#14
toMaybe :: Bool -> a -> Maybe a
toMaybe b aa = if b then Just aa else Nothing
-- *A6> toMaybe True "hello"
-- Just "hello"
-- *A6> toMaybe False "hello"
-- Nothing

-- Q#15
validateSecret :: (Secret -> Bool) -> GameException -> Secret-> Either GameException Secret
validateSecret p ge s = 
  if p s then Right s else Left ge
-- *A6> validateSecret lengthInRange InvalidWord "balbasar"
-- Right "balbasar"
-- *A6> validateSecret lengthInRange InvalidWord "a"
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.
-- *A6> 

-- Q#16
-- might be better called withValidChars
hasValidChars :: Secret -> Either GameException Secret
hasValidChars = validateSecret (all isAlpha) InvalidWord 
-- *A6> hasValidChars "333332"
-- Left Invalid word. A secret word must be between 3 and 20 alphabetic characters.
-- *A6> hasValidChars "asdf"
-- Right "asdf"

-- might be better called withValidLength or assertValidLength
isValidLength :: Secret -> Either GameException Secret
isValidLength = validateSecret lengthInRange InvalidWord 
-- *A6> isValidLength "asdfasdfasdfasdfasdfasdf"
-- Left Invalid word. A secret word must be between 3 and 20 alphabetic characters.
-- *A6> isValidLength "a"
-- Left Invalid word. A secret word must be between 3 and 20 alphabetic characters.
-- *A6> isValidLength "aasdfasdf"
-- Right "aasdfasdf"

-- better called assertIsInDict
isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict d = validateSecret (\secret -> map toLower secret `elem` d) InvalidWord
-- *A6> dict =  ["asdf", "wert", "reew"]
-- *A6> isInDict dict "asdf"
-- Right True
-- *A6> isInDict dict "asdfa"
-- Left Invalid word. A secret word must be between 3 and 20 alphabetic characters.
-- *A6> isInDict dict "reew"
-- Right True

-- Q#17
validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = case isValidLength  s of
    Left ex -> Left ex
    Right _ -> case hasValidChars s of
      Left ex -> Left ex
      Right _  -> Right s
-- *A6> validateNoDict "asdf"
-- Right "asdf"
-- *A6> validateNoDict "asdfasdfasdfasdfasdfasdfasdfasdf"
-- Left Invalid word. A secret word must be between 3 and 20 alphabetic characters.
-- *A6> validateNoDict "asdfa222"
-- Left Invalid word. A secret word must be between 3 and 20 alphabetic characters.
-- *A6> validateNoDict "222"
-- Left Invalid word. A secret word must be between 3 and 20 alphabetic characters.

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = case validateNoDict s of
  Left ex -> Left ex
  Right _ -> case isInDict d s of
    Left ex -> Left ex
    Right _ -> Right s
-- *A6> dict = ["asdf", "ASDF", "a", "PICACHU", "supercalifragilisticexpialidocious"]
-- *A6> validateWithDict dict "supercalifragilisticexpialidocious"
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.
-- *A6> validateWithDict dict "a"
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.
-- *A6> validateWithDict dict "picachu"
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.
-- *A6> validateWithDict dict "asdf"
-- Right "asdf"

-- Q#18
processTurn :: Move -> Game -> Either GameException Game
processTurn m g  
  | invalidMove m = Left InvalidMove
  | repeatedMove (toUpper m) g = Left RepeatMove
  | otherwise = if game_remainingChances newGame > 0
        then Right newGame 
        else Left GameOver
    where 
      newGame = updateGame m g
  
 -- *A6> myGame = Game "SPOT" "SPO_" "ABCSPO" 1
-- *A6> myGame 
-- **************************************************
--        Current Guess:  S P O _
--        Guessed:        A B C O P S
--        Remaining Chances:      1
-- **************************************************
--
-- *A6> processTurn 'D' myGame
-- Left Game over, Man!
-- *A6> processTurn 'T' myGame
-- Right 
-- **************************************************
--        Current Guess:  S P O T
--        Guessed:        A B C O P S T
--        Remaining Chances:      1
--
-- **************************************************
--
-- *A6> processTurn '1' myGame
-- Left Invalid move. Must use an alphabetic character.
-- *A6> processTurn 'A' myGame
-- Left Repeated move. The character was previously used. Try another.
