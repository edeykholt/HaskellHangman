module A7 where

import Provided
import A6

import Data.Char ( toUpper )
import System.Directory ( doesFileExist )

-- *** A7: Functors & Applicatives *** --

-- Q#01
getUpperChar :: IO Char
getUpperChar = toUpper <$> getChar
-- *A7> getUpperChar
-- a'A'

-- Q#02
_DICT_ :: IO Dictionary
_DICT_ = lines <$> readFile _DICT_FILE_
-- *A7> _DICT_
-- ["aardvark","aardwolf",-- ...

-- Q#03
makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid = undefined
-- makeGameIfValid eitherGeOrS = makeGame <$> eitherGeOrS

-- Q#04

getDict = undefined