{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Hurdle where

import Hurdle.Command
import Hurdle.Match
import Data.Char
import Hurdle.Words

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
parseCommand "give up" = GiveUp
parseCommand "Letters" = ShowLetters -- 
parseCommand command = Guess $ normalise command



--------------------------------------------------------------------------------
-- | 4. Part one of the matching algorithm finds the exact matches.
-- For each position, give back IsExact if the two characters are the 
-- same, or IsNotExact if they are different. 
-- Implement this using explicit recursion. If you can see a more elegant 
-- solution, describe it in your justification.
--
-- [JUSTIFY]
exactMatches :: String -> String -> [ExactMatch]
exactMatches [] [] = []
exactMatches (x:xs) (y:ys)
    | x == y = IsExact x : exactMatches xs ys
    | otherwise = IsNotExact x : exactMatches xs ys
exactMatches _ _ = []


--------------------------------------------------------------------------------
-- | 5. We want to keep track of the "unused" characters in the answer. First, 
-- we use up all of the exact matches. This function takes the exact matches and 
-- the answer and gives back all the characters not already exactly matched.
--
-- [JUSTIFY]
removeExacts :: [ExactMatch] -> String -> String
removeExacts [] [] = []
removeExacts (x:xs) (y:ys)
    | x == IsExact y = removeExacts xs ys
    | otherwise = y : removeExacts xs ys
removeExacts _ _ = []



--------------------------------------------------------------------------------
-- | 6. Follow the algorithm in the specification to correctly return the list 
-- of character matches, given the result of exactMatches and any unused 
-- characters of the answer.
-- 
-- [JUSTIFY]
removeChar :: Char -> [Char] -> [Char]
removeChar c (x:xs) =
    if c == x then xs
    else x:removeChar c xs
removeChar _ _ = error "some error"

getMatches :: [ExactMatch] -> [Char] -> [Match]
getMatches [] [] = []
getMatches (IsExact x:xs) (y:ys) = Exact : getMatches xs (y:ys)
getMatches (IsNotExact x:xs) (y:ys)
    | x `elem` (y:ys) = Partial : getMatches xs (removeChar x (y:ys))
    | otherwise = None : getMatches xs (y:ys)
getMatches [] a = []
getMatches a [] = []





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


eliminateOne (x:xs) (y:ys) (z:zs) = case y of
    Exact   -> z == x && eliminateOne xs ys zs
    Partial -> z /=  x && i /=[]  &&  eliminateOne xs ((removeAtIndex (ys) (head i -1)) ++ [y]) ((removeAtIndex  (zs) (head i -1)) ++ [z])
    None -> x `notElem` (z:zs) && eliminateOne xs (ys ++ [y]) (zs ++ [z])
    where
        i = partialExist x (y:ys) (z:zs)
        partialExist element matches str = [ i | i <- [1.. (length matches-1)], (str !! i) == element && (matches !! i) /= Exact]
        removeAtIndex str i = take i str ++ drop (1 + i) str
eliminateOne _ _ _ = True






eliminate :: String -> [Match] -> [String] -> [String]
eliminate _ _ [] = []
eliminate guess exacts (z:zs) =  [z | eliminateOne guess exacts z] ++  eliminate guess exacts zs



--------------------------------------------------------------------------------
-- | 9. Based on the whole history of the game so far, return only those words 
-- from `guessList` which might still be the hidden word.
--
-- [JUSTIFY]
eliminateAll :: [(String, [Match])] -> [String]
eliminateAll = error "Not implemented"


--------------------------------------------------------------------------------
-- | 10. Using the above functions, write a function which produces a next guess 
-- based on the history of the game so far.
--
-- [JUSTIFY]
nextGuess :: [(String, [Match])] -> String
nextGuess = error "Not implemented"