module A7 where

import Provided
import A6

import Data.Char ( toUpper )
import System.Directory ( doesFileExist, exeExtension )
import GHC.IO.Device (IODeviceType(Directory))
import GHC.RTS.Flags (getGCFlags)

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
makeGameIfValid eges = case eges of
    Left ex -> Left ex
    Right s -> case validateNoDict s of
            Left ex2 -> Left ex2
            Right _ -> makeGame <$> eges
-- *A7> makeGameIfValid $ Right "asdf"
-- Right 
-- **************************************************
-- 
--        Current Guess:  _ _ _ _
--
--        Guessed:
--
--        Remaining Chances:      7
--
-- **************************************************

-- *A7> makeGameIfValid $ Right "1"
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.
-- *A7> makeGameIfValid $ Left InvalidWord
-- Left Invalid word. A secret word must be in the dictionary and be between 3 and 20 alphabetic characters.


-- Q#04
getDict :: IO (Maybe Dictionary)
getDict = do
    hasFile <- doesFileExist _DICT_FILE_
    if hasFile
    then
        Just . lines <$> readFile _DICT_FILE_
    else 
        Just <$> _DICT_
-- *A7> getDict
-- Just ["aardvark","aardwolf","aaron","aback", ...
