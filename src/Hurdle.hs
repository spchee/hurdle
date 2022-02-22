module Hurdle where

import Hurdle.Command
import Hurdle.Match
import Data.Char
import Hurdle.Words ( guessList, answerList, reducedGuessList )
import Text.ParserCombinators.ReadP (count)
import Data.Ord (Down(Down))
import Data.List (sortBy)
import Data.Foldable (maximumBy)



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
-- I initially was going to attempt to use 'case of', but realised that because it
-- was a range of values, it wouldn't have worked even thought it would've looked a bit
-- neater. So I opted to use guards. 
--  Using the map function and the maybe type instead of tail-recursion was also an option i considered, 
-- and did implement, but it was harder to read and less elegant despite being probably a little faster.
-- (I still kept the map function one though so you can see what I did) 

normalise :: String -> String
normalise "" = ""
normalise (x:xs)
    |   x `elem` ['a'.. 'z'] = toUpper x : normalise xs
    |   x `elem` ['A'.. 'Z'] = x : normalise xs
    |   otherwise = normalise xs


-- normaliseOne :: Char -> Maybe Char
-- normaliseOne c
--     |   c `elem` ['a'.. 'z'] = Just $ toUpper c
--     |   c `elem` ['A'.. 'Z'] = Just c
--     |   otherwise = Nothing

-- normalise' :: String -> String
-- normalise' str = filter (/=' ') $ map ((fromMaybe ' ') . normaliseOne) str



--------------------------------------------------------------------------------
-- | 2. A valid guess is a one which appears in `guessList`.                 
--
-- using the previously defined normalise function, I was easily able to find if a word was
-- valid by simply checking if it was in the guessList using elem. 
-- Another Idea I had was to use a tree to speed up the search, but I think it was worth
-- the effort to implement.

isValid :: String -> Bool
isValid word = (normalise word) `elem`  guessList

--------------------------------------------------------------------------------
-- | 3. Our program runs a little command line. Specific strings should be   
-- treated not as guesses, but as commands. See the specification for details.
--
-- I initially used guards, but afterwards realised it would be a lot more elegant to use
-- case of instead.
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
-- I initially though to use guards and tail-recusion to quickly check if the characters were the same. 
--  But I soon realised I could use zipWith to quickly check if the characters were the same and 
--  generate a matchlist.


exactMatches :: String -> String -> [ExactMatch]
exactMatches w1 w2 = zipWith (\x y -> (if x == y then IsExact x else IsNotExact x)) w1 w2
    where
        w1' = normalise w1
        w2' = normalise w2



--------------------------------------------------------------------------------
-- | 5. We want to keep track of the "unused" characters in the answer. First, 
-- we use up all of the exact matches. This function takes the exact matches and 
-- the answer and gives back all the characters not already exactly matched.
--
-- I tried to initially use zipWith and maybes, but using maybes turned out to make the 
-- code more complicated than it needed to be since I had to then as well filter out 
-- the nothings, and then remove the justs, etc. 
-- So I resorted to using tail recursion and guards.


removeExacts :: [ExactMatch] -> String -> String
removeExacts (x:xs) (y:ys)
    | x == IsExact y = removeExacts xs ys
    | otherwise      = y : removeExacts xs ys
removeExacts _ _ = []


--------------------------------------------------------------------------------
-- | 6. Follow the algorithm in the specification to correctly return the list 
-- of character matches, given the result of exactMatches and any unused 
-- characters of the answer.
-- 
-- Since I only wanted to remove the first instance of a character, 
-- I used tail recursion and guards to find the character then remove it.
-- rather than say filter which would remove all characters but be neater to use.
--
-- I then used pattern matching and guards to account for the different cases in 
-- getMatches. I opted against using 'case of' because I found
-- it was a little harder to read compared to the current solution.

removeChar :: Char -> String -> String
removeChar c (x:xs)
    | c == x = xs
    | otherwise =  x:removeChar c xs
removeChar _ _ = error "some error"

-- returns a list of matches for the given word and ExactMatch list
getMatches :: [ExactMatch] -> [Char] -> [Match]
getMatches (IsExact x:xs) (y:ys) = Exact : getMatches xs (y:ys)

-- When not exact, if the character is in the word, it is a Partial else None
getMatches (IsNotExact x:xs) (y:ys)
    | x `elem` (y:ys) = Partial : getMatches xs (removeChar x (y:ys))
    | otherwise = None : getMatches xs (y:ys)
getMatches _ _ = []





--------------------------------------------------------------------------------
-- | 7. Write the complete matching algorithm as a composition of the above 
-- three functions.
--
-- The following simply uses the above functions to get the matches.
-- I used the where clause to make it easier to read.

-- Given two strings, returns a list of matches
matchingAlgo :: String -> String -> [Match]
matchingAlgo guess answer = getMatches exacts noExacts
    where
        exacts = exactMatches guess answer
        noExacts = removeExacts exacts answer



--------------------------------------------------------------------------------
-- | 8. Given a list of candidate words, remove those words which would not 
-- have generated the given match based on the guess that was made.
--
-- I dedided to use two seperate functions as it makes it easier to understand and 
-- makes sense intuitively when writing the actual code.
--
-- Previously I opted to not use the matchingAlgo function as previously defined, 
-- as I initially approached it by using a guess word and the list of matches, and using that
-- to determine if the answer was in the list of possible matches rather than using the matching algo
-- function to get the list of matches for two different words.
-- I was then simply able to use the filter function to effectively filter all of
-- the words which didn't match the pattern given. 

-- single word -> list of matches -> single word -> whether the matches of the two words matches the matches given.
eliminateOne :: String ->  [Match] -> String -> Bool
eliminateOne guess matches answer = (matchingAlgo guess answer) == matches

-- single word -> matches -> list of words -> list of words which match the match pattern wotj word given
eliminate :: String -> [Match] -> [String] -> [String]
eliminate guess matches = filter (eliminateOne guess matches)







--------------------------------------------------------------------------------
-- | 9. Based on the whole history of the game so far, return only those words 
-- from `guessList` which might still be the hidden word.
--
-- Uses foldr to continuously eliminate words from the guessList based on the
-- previous guesses. 
-- Decided to keep redundant 'gusses' in function to make it slightly easier to read. 
-- (Despite haskell's suggestion to remove it)
eliminateAll :: [(String, [Match])] -> [String]
eliminateAll guesses = foldr (uncurry eliminate) guessList guesses

--------------------------------------------------------------------------------
-- | 10. Using the above functions, write a function which produces a next guess 
-- based on the history of the game so far.
--
-- Takes about 60-70 seconds to run on my i5 6600k .
--
-- Used two different functions, one for when the list is still large, and one for when the list is below 100. 
-- This allows me to use a slower but more accurate method of guessing when the list is small. 
-- Using a pre-set first word also speeds it up considerably.
-- You can see my justification for each individual nextGuess function below.

nextGuess :: [(String, [Match])] -> String
nextGuess [] = "SALET"
nextGuess pastGuesses
    | length list < 100 = nextGuessSlow list
    | otherwise = head $ nextGuessFast  (list) []
    where list = eliminateAll pastGuesses




--------------------------------------------------------------------------------

-- Code for nextGuessSlow
-- Inspired by the 3b1b inplementation which is found here: https://www.youtube.com/watch?v=v68zYyaEmEA
-- Initially tried to do something similar but with less matches and instead of using the fancy
-- formula, I'd simply take the maximum length of the list that could be possibly be generated.
-- Unfortunately it was too slow so I ditched it, created nextGuessFast, and then fully implemented 
-- this one which is only used when the list is longer than 100.


-- Generates all possible variations of a list of matches with length 5. 
-- Tried to use an algorithm which generates binary numbers, except for base 3 and replace the digits with the matches.
-- Adapted from https://www.reddit.com/r/haskell/comments/2rmwly/create_an_infinite_list_with_all_binary_numbers/cnhelce/
genMatches :: [[Match]]
genMatches = drop 121 $ take 364 m
    where m = [] : [ new : old | old <- m, new <- [None, Partial, Exact] ]



-- scores a single word based on the equation sum(p(x) * log_2 (1/p(x))). Higher = better. 
-- word -> list of possible answers -> score 
scoreWord :: Floating a => String -> [String] -> a
scoreWord word list = sum scores
    where
        -- gets list of number of matches for each possible variation of [match] 
        wordsRemaining = [ x | m <- genMatches, x <- [length $ eliminate word m list] ]
        wordsRemaining' = filter (/=0) wordsRemaining -- filters out ones with no words

        -- finds probability of certain [Match] occuring
        probabilities = [  fromIntegral(x) / fromIntegral(length list ) | x <- wordsRemaining' ]

        -- finds log_2 (p(x)) and multiplies it with each p(x)
        log p = logBase (2) (1 / p)
        logs = [ log p | p <- probabilities ]
        scores = zipWith ((*)) logs ( probabilities)

-- finds the score for every word in a list
-- Used list comprehension as it meant I could much more easily create pairs of words and scores
-- rather than using say using the map function where I'd need to implement a seperate function 
-- to create the pairs.
--                          guesses -> answers -> (score, word)
scoreWords :: Floating a => [String] -> [String] -> [(a, String)]
scoreWords guesses answers = [ (scoreWord word answers, word) | word <- guesses ]

-- finds the word with the maximum score and sets it as next guess
--       possible guesses -> next guess
nextGuessSlow :: [String] -> String
nextGuessSlow answers = snd $ maximumBy (\(a,_) (b,_) -> compare a b) scores
    where
        scores = scoreWords answers answers
-------------------------------------------------------------

-- Code for nextGuessFast function
-- finds the character which is most common, elimnates all words which don't have that character,
-- then proceeds to find the most common character which is most common of the new list of words and repeat
-- until either only 1 word is left, or 5 characters have been used. 
-- slightly worse than nextGuessSlow but much faster in execution time
-- 
-- I decided to implement this after trying to implement other algorithms but them just taking too long to process
-- So this one is roughtly O(n log n) time and therefore is much faster than many other O(n^2) algorithms.
-- Using character frequency rather than eliminating words on matches is much faster but a little less accurate. 
--
-- Using nextGuessFast is able to do 300 tests in about 9 seconds compared with the over a minute of using 
-- nextGuessSlow and nextGuessFast combined. 


-- Decided to use two seperate functions for counting letters in list for readability.
-- Char -> string to filter -> how many times char appears in string
countLetterInString :: Char -> String -> Int
countLetterInString c = length . filter (==c)


--  single char -> list of strings -> how many times a char appears in list
countLettersInList  :: Char -> [String] -> Int
countLettersInList _ [] = 0
countLettersInList c (str:strings) = countLetterInString c str + countLettersInList c strings

-- list of words -> [(how many times character appears in list, character)]
countAllLettersInList :: [String] -> [(Int,Char)]
countAllLettersInList l = [(countLettersInList x l,x) | x <- ['A'..'Z']]

-- Finds the highest 5 characters
-- list of (char count, char) -> list of 5 highest chars
highestFive :: [(Int, Char)] -> [Char]
highestFive list = map (snd) $ take 5 $ sort $ removeZeroes list
    where
        removeZeroes l = filter (\(a,_) -> a /=0  ) l -- filters non-occuring characters
        sort l = sortBy (\(a,_) (b,_) -> compare b a) l -- sorts in ascending order 


-- usedChars -> list of chars in order of frequency -> list of chars without the used chars in order of frequency
removeUsedChars :: [Char] -> [Char] -> [Char]
removeUsedChars usedChars = filter (`notElem` usedChars)


filterStringsByChar :: Char -> [String] -> [String]
filterStringsByChar c = filter (elem c)


--      list of words -> chars which have been used -> new filtered list
nextGuessFast :: [String] -> [Char] -> [String]
nextGuessFast  list usedChars
    | length usedChars == 5 || length list == 1 || (length chars - length usedChars) == 0 || null unusedChars = list
    | otherwise = nextGuessFast  filteredStrings (usedChars ++ [head unusedChars]) -- repeats for new smaller list
    where
        chars =  highestFive $ countAllLettersInList list -- top 5 common chars
        unusedChars = removeUsedChars usedChars chars --removes used
        filteredStrings = filterStringsByChar (head unusedChars) list -- list of strings which include the most common unused char