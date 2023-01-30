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
getDict = gg
    where
        gg :: IO (Maybe Dictionary)
        gg = toMaybe <$> x <*> z

        x :: IO Bool
        x = doesFileExist _DICT_FILE_

        z :: IO Dictionary
        z = lines <$> readFile _DICT_FILE_
        -- TODO if _DICT_FILE is not found, return Just _DICT_

getDict2 :: IO (Maybe Dictionary)
getDict2 = toMaybe <$> doesFileExist _DICT_FILE_ <*> (lines <$> readFile _DICT_FILE_)
        -- TODO if _DICT_FILE is not found, return Just _DICT_

-- getDict3 :: Maybe (IO Dictionary)
-- getDict3 = ggg
--    where
--        xs :: IO (Maybe Dictionary)
--        xs = toMaybe <$> doesFileExist _DICT_FILE_ <*>  (lines <$> readFile _DICT_FILE_)
--        ggg = undefined