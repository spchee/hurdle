{-# LANGUAGE TemplateHaskell #-}

module Hurdle.Words where

import Data.FileEmbed (embedStringFile)
import Data.List (sort)

{-
In this file we use Template Haskell (some Haskell code run at compile time) to import these word lists and throw a compiler error if we cannot find 
them. 

This means that the compiled program doesn't need these word lists at all, because they are embedded in the code!
-}

-- | The file "assets/answers" includes all words that might be chosen by the 
-- program as answers. 
-- The list is sanitised to include only sensible words.
answerList :: [String]
answerList = lines $(embedStringFile "assets/answers")

-- |  The file "assets/allowed-guesses" contains a broader, less sanitised selection of five-letter words which may be used as guesses.
-- Note that the two files are disjoint, so we need to add the answer list to the guess list to cover all words in the system.
guessList :: [String]
guessList = sort $ lines $(embedStringFile "assets/allowed-guesses") ++ answerList

reducedGuessList :: [String]
reducedGuessList = sort $ lines $(embedStringFile "assets/reduced-guesses") ++ answerList

genReducedGuessList :: [String] -> [String]
genReducedGuessList [] = []
genReducedGuessList (x:xs)
    | length xs < 6 = x : answerList
    | otherwise = x : genReducedGuessList (drop 6 xs)

-- write list of strings to a filepath using writeFile
writeListToFile :: [String] -> FilePath -> IO ()
writeListToFile [] _ = return ()
writeListToFile (x:xs) fp = do
    appendFile fp (x ++ "\n")
    writeListToFile xs fp


