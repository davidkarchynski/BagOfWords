module Parser 
    (Gram,              -- n-gram (word/stem, bigram etc.
     Sentence,          -- list of words
     Corpus,            -- list of words
     stringToSentence,  -- [Char] -> String -> Sentence
     wordsToNGrams,     -- Int -> Sentence -> Sentence
     sanitizeWords      -- [[Char]] -> Sentence -> Sentence
    ) where

import Data.Char
import Data.List

{-
Since we constantly deal with nested lists of Char, let's adopt some convention for readability:
[Char] refers to delimeters/suffixes
[[Char]] refers to a list of delimeters/suffixes
String refers to the initial unparsed text message
Gram refers to n-gram (word/stem, bigram etc.)
-}

type Gram = [Char]
type Sentence = [Gram]
type Corpus = [Gram]

testSentence = stringToSentence " ,.?!" "What? is this thing? ... called Love."
testSanitizedSentence = sanitizeWords ["this", "is", "a", ""] ["s", "ed"] testSentence
parserEndToEndTest = wordsToNGrams 2 testSanitizedSentence
-- parserEndToEndTest should return ["what thing", "thing call", "call love"]

-- stringToSentence splits a string into words using delimiters in dlims
stringToSentence :: [Char] -> String -> Sentence
stringToSentence dlims str = splitSep (flip elem dlims) str
-- stringToSentence " ,.?!" "What? is this thing? ... called Love."
--      should return ["What","","is","this","thing","","","","","","called","Love",""]

--splitSep separates a list of elements into a list of list of elements by the given separator
--splitSep function taken from assignment 3 solution.
splitSep :: (a -> Bool) -> [a] -> [[a]]
splitSep p [] = [[]]
splitSep p (h:t)  
    | p h = []:rest
    | otherwise = (h : head rest) : tail rest
        where rest = splitSep p t

-- wordsToNGrams take output of stringToSentence to produce a list of n-grams 
-- returns [] if sentence has fewer than n grams
wordsToNGrams :: Int -> Sentence -> Sentence
wordsToNGrams n [] = []
wordsToNGrams n (h:t)
    | n < (length (h:t)) = (buildNGram n (h:t)) : (wordsToNGrams n t)
    | n == (length (h:t)) = [buildNGram n (h:t)]
    | otherwise = []
-- wordsToNGrams 2 ["rose", "red", "other", "name"]
--      should return ["rose red ","red other ","other name "]
-- wordsToNGrams 3 ["rose", "red", "other", "name"]
--      should return ["rose red other","red other name"]
-- wordsToNGrams 4 ["rose", "red", "other", "name"]
--      should return ["rose red other name"]
-- wordsToNGrams 2 ["rose", "red", "other", "name"]
--      should return []

-- buildNGram combines the first n grams in the given sentence and returns that as a single gram
buildNGram :: Int -> Sentence -> Gram
buildNGram 0 _ = []
buildNGram _ [] = []
buildNGram 1 (h:_) = h
buildNGram n (h:t) = h ++ " " ++ (buildNGram (n-1) t)

-- filterWords, given wrds, a list of words, removes all the words from lst
-- e.g., lst may include articles, prepositions, pronouns, etc.
filterWords :: [Gram] -> Sentence -> Sentence
filterWords gramsToRemove wrds = filter (flip notElem gramsToRemove) wrds
-- filterWords ["This", "is", "a"] ["This", "is", "a", "test", "sentence"]
--      should return ["test","sentence"]
-- filterWords [] ["This", "is", "a", "test", "sentence"]
--      should return ["This","is","a","test","sentence"]
-- filterWords ["This", "is", "a"] []
--      should return []


-- sanitizeWords filters ignoredWords, and converts remaining words to lower-case stems,
--      i.e. strip suffixes -ing, -ed, -s
sanitizeWords :: [Gram] -> [[Char]] -> Sentence -> Sentence
sanitizeWords ignoredWords sfxs wrds = 
    map (\ gram -> stripApplicableSuffix sfxs gram) filteredWrds
    where
        lowerWrds = map (\ gram -> map toLower gram) wrds
        filteredWrds = filterWords ignoredWords lowerWrds
-- sanitizeWords [] ["ing", "s", "ed"] ["Looking", "looks", "Looked"]
--      should return ["look", "look", "look"]
-- sanitizeWords [] ["ing", "s", "ed"] ["Looking", "apples", "Wondered"]
--      should return ["look","apple","wonder"]
-- sanitizeWords [] [] ["Looking", "apples", "Wondered"]
--      should return ["looking","apples","wondered"]
-- sanitizeWords [] ["ing", "s", "ed"] []
--      should return []
-- sanitizeWords ["look"] ["ing", "s", "ed"] ["Looking", "looks", "Looked"]
--      should return ["look", "look", "look"]
-- sanitizeWords ["looking"] ["ing", "s", "ed"] ["Looking", "apples", "Wondered"]
--      should return ["apple","wonder"]
-- sanitizeWords ["apples"] [] ["Looking", "apples", "Wondered"]
--      should return ["looking","wondered"]
-- sanitizeWords ["test"] ["ing", "s", "ed"] []
--      should return []

-- stripApplicableSuffix strips the appropriate suffix (from given list of suffixes) from the given gram
-- if no suffixes apply, returns gram input unchanged
stripApplicableSuffix :: [[Char]] -> Gram -> Gram
stripApplicableSuffix sfxs gram = foldr (\ sfx acc -> 
    if (isSuffixOf sfx gram) then (stripSuffix sfx gram) else acc) gram sfxs

-- stripSuffix strips the given suffix from the given gram
stripSuffix :: [Char] -> Gram -> Gram
stripSuffix sfx wrd = take n wrd
    where n = (length wrd) - (length sfx)
