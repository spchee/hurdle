{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Hurdle where

import Hurdle.Command
import Hurdle.Match
import Data.Char
import Hurdle.Words ( guessList, answerList, reducedGuessList )
import Text.ParserCombinators.ReadP (count)
import Data.Ord (Down(Down))
import Data.List (sortBy)



--import 
--------------------------------------------------------------------------------
-- This file is your complete submission for the first coursework of CS141.
-- 
-- USER ID: 
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------


-- | 1. Given an input guess, change it so that it is all upper case.        
--
-- [JUSTIFY]
normalise :: String -> String
normalise "" = ""
normalise (x:xs)
    |   null (x:xs) = []
    |   'a' <= x && x <= 'z' = toUpper x : normalise xs
    |   'A' <= x && x <= 'Z' = x : normalise xs
    |   otherwise = normalise xs



--------------------------------------------------------------------------------
-- | 2. A valid guess is a one which appears in `guessList`.                 
--
-- [JUSTIFY]
isValid :: String -> Bool
isValid word = (normalise word) `elem`  guessList

--------------------------------------------------------------------------------
-- | 3. Our program runs a little command line. Specific strings should be   
-- treated not as guesses, but as commands. See the specification for details.
--
-- [JUSTIFY]
parseCommand :: String -> Command
parseCommand str = case normalise str of
    "GIVEUP"    -> GiveUp
    "LETTERS"   -> ShowLetters
    _           -> Guess $ normalise str




--------------------------------------------------------------------------------
-- | 4. Part one of the matching algorithm finds the exact matches.
-- For each position, give back IsExact if the two characters are the 
-- same, or IsNotExact if they are different. 
-- Implement this using explicit recursion. If you can see a more elegant 
-- solution, describe it in your justification.
--
-- [JUSTIFY]
exactMatches :: String -> String -> [ExactMatch]
exactMatches (x:xs) (y:ys)
    | x == y    = IsExact    x : exactMatches xs ys
    | otherwise = IsNotExact x : exactMatches xs ys

exactMatches _ _ = []


--------------------------------------------------------------------------------
-- | 5. We want to keep track of the "unused" characters in the answer. First, 
-- we use up all of the exact matches. This function takes the exact matches and 
-- the answer and gives back all the characters not already exactly matched.
--
-- [JUSTIFY]
removeExacts :: [ExactMatch] -> String -> String
removeExacts (x:xs) (y:ys)
    | x == IsExact  y = removeExacts xs ys
    | otherwise =   y : removeExacts xs ys
removeExacts _ _ = []



--------------------------------------------------------------------------------
-- | 6. Follow the algorithm in the specification to correctly return the list 
-- of character matches, given the result of exactMatches and any unused 
-- characters of the answer.
-- 
-- [JUSTIFY]
removeChar :: Char -> [Char] -> [Char]
removeChar c (x:xs)
    | c == x = xs
    | otherwise =  x:removeChar c xs
removeChar _ _ = error "some error"

getMatches :: [ExactMatch] -> [Char] -> [Match]
getMatches (IsExact x:xs) (y:ys) = Exact : getMatches xs (y:ys)
getMatches (IsNotExact x:xs) (y:ys)
    | x `elem` (y:ys) = Partial : getMatches xs (removeChar x (y:ys))
    | otherwise = None : getMatches xs (y:ys)
getMatches _ _ = []





--------------------------------------------------------------------------------
-- | 7. Write the complete matching algorithm as a composition of the above 
-- three functions.
--
-- [JUSTIFY]
matchingAlgo :: String -> String -> [Match]
matchingAlgo guess answer = getMatches exacts str
    where
        exacts = exactMatches guess answer
        str = removeExacts exacts answer



--------------------------------------------------------------------------------
-- | 8. Given a list of candidate words, remove those words which would not 
-- have generated the given match based on the guess that was made.
--
-- [JUSTIFY]
            -- guess -> matching-letters -> possible answer -> is possible answer?

-- eliminateOne :: String -> [Match] -> String -> Bool
-- eliminateOne guess matches answer = canRemoveExact && canRemovePartial && canRemoveNone
--     where 
--         canRemoveExact = checkExacts guess matches answer
--         canRemovePartial = True 
--         canRemoveNone = True

-- removeNones :: (String, String) -> Bool
-- removeNones ((x:xs), answer)
--     | x `notElem` answer = removeNones (xs, answer)
--     | otherwise = False 
-- removeNones (_, _) = True 


-- removePartial :: (String, [Match], String) -> (String, String) -> (String, String)
-- removePartial ((x:xs), (y:ys), (z:zs) ) (x', y') 


-- removeExacts' :: String -> [Match] -> String -> (String, [Match], String) -> (String, [Match], String)
-- removeExacts' (x:xs) (y:ys) (z:zs) (x', y', z')
--     | x == z && y == Exact = removeExacts' xs ys zs (x', y', z')
--     | otherwise = removeExacts' xs ys zs (x:x', y:y',   z:z')
-- removeExacts' _ _ _ (x', y', z') = (reverse x', reverse y', reverse z')

-- checkExacts :: Eq a => [a] -> [Match] -> [a] -> Bool
-- checkExacts (x:xs) (Exact:ys) (z:zs)
--         | x == z = checkExacts xs ys zs
--         | otherwise = False
-- checkExacts _ _ _ = True




eliminateOne :: String ->  [Match] -> String -> Bool
eliminateOne guess matches answer = matchingAlgo guess answer == matches


eliminate :: String -> [Match] -> [String] -> [String]
eliminate guess matches = filter (eliminateOne guess matches)







--------------------------------------------------------------------------------
-- | 9. Based on the whole history of the game so far, return only those words 
-- from `guessList` which might still be the hidden word.
--
-- [JUSTIFY]
eliminateAll :: [(String, [Match])] -> [String]
eliminateAll = foldr (uncurry eliminate) guessList 

--------------------------------------------------------------------------------
-- | 10. Using the above functions, write a function which produces a next guess 
-- based on the history of the game so far.
--
-- [JUSTIFY]
--nextGuess :: [(String, [Match])] -> String
--nextGuess [] = "SALET"
----nextGuess pastGuesses = snd $ maxTuple $ possibleGuesses reducedGuessList  possibleMatches (eliminateAll pastGuesses)

    




-- creating a hashing function for each set of words
possibleMatches :: [[Match]]
possibleMatches = [[Partial, None, None, None, None], [None, Partial, None, None, None], [None, None, Partial, None, None], [None, None, None, Partial, None], [None, None, None, None, Partial], [None, None, None, None, None]]

--      guess -> Matches -> remainingAnswers -> maxNumberOfWordsWhichMatch

 
getMaxMatches :: String -> [[Match]] -> [String] -> Int
getMaxMatches guess matches answers = foldr (max . length . eliminate') 0 matches
    where eliminate' matches = eliminate guess matches answers




possibleGuesses :: [String] -> [[Match]] -> [String] -> [(Int, String)]
possibleGuesses guesses matches answers = map getMaxMatches' guesses
    where getMaxMatches' guess = (getMaxMatches guess matches answers, guess)

-- if number of elements in current is smaller than first, then replace first with current
possibleGuesses' :: [String] -> [[Match]] -> [String] -> (Int, String)
possibleGuesses' guesses matches answers = foldr (\x y -> if (fst x) < (fst y) then x else y) (0, "") (possibleGuesses guesses matches answers)

-- find max of list of tuples (Int, String)
maxTuple :: [(Int, String)] -> (Int, String)
maxTuple [] = error "empty list"
maxTuple (x:xs) = foldr (\(a, b) (c, d) -> if a > c then (a, b) else (c, d)) x xs

countLetter :: Char -> String -> Int
countLetter c = length . filter (==c) 

countLettersInList  :: Char -> [String] -> Int 
countLettersInList _ [] = 0
countLettersInList c (str:strings) = countLetter c str + countLettersInList c strings 

countAllLettersInList :: [String] -> [(Int,Char)]
countAllLettersInList l = [(countLettersInList x l,x) | x <- ['A'..'Z']]

-- find highest 5 letters
highestFive :: [(Int, Char)] -> [Char]
highestFive [] = []
highestFive list = map (snd) $ take 5 $ sortBy (\(a,_) (b,_) -> compare b a) list

elimnateFirstChar :: String -> [String] -> [(Int, String)]
elimnateFirstChar _ [] = []
elimnateFirstChar (char:chars) (str:xs) 
    | char `elem` str = elimnateFirstChar (char:chars) (drop 1 str:xs) 
    | otherwise = 


