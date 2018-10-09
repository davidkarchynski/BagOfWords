module Parser where
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

-- TODO: implement wordsToNGrams
-- wordsToNGrams take output of splitStringToWords to produce a list of n-grams 
wordsToNGrams :: Int -> Sentence -> Sentence
wordsToNGrams n [[]] = [[]]
-- wordsToNGrams dlims str = ...

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


-- sanitizeWords converts all words to lower-case stems, i.e. strip suffixes -ing, -ed, -s
sanitizeWords :: [[Char]] -> Sentence -> Sentence
sanitizeWords sfxs wrds = 
    map (\ gram -> stripApplicableSuffix sfxs (map toLower gram)) wrds
--sanitizeWords sfxs [] = []
-- sanitizeWords sfxs wrds = ...

-- sanitizeWords ["ing", "s", "ed"] ["Looking", "looks", "Looked"]
--      should return ["look", "look", "look"]
-- sanitizeWords ["ing", "s", "ed"] ["Looking", "apples", "Wondered"]
--      should return ["look","apple","wonder"]


-- stripApplicableSuffix strips the appropriate suffix (from given list of suffixes) from the given gram
-- if no suffixes apply, returns gram input unchanged
stripApplicableSuffix :: [[Char]] -> Gram -> Gram
stripApplicableSuffix sfxs gram = foldr (\ sfx acc -> 
    if (isSuffixOf sfx gram) then (stripSuffix sfx gram) else acc) gram sfxs

-- stripSuffix strips the given suffix from the given gram
-- if suffix does not apply, returns gram input unchanged
stripSuffix :: [Char] -> Gram -> Gram
stripSuffix sfx wrd = 
    reverse (getRoot (stripPrefix rSfx rWrd) rWrd)
    where
        rSfx = reverse sfx
        rWrd = reverse wrd
    
-- getRoot returns the root of the word if a suffix was stripped from it
-- otherwise returns the word itself
getRoot :: Maybe Gram -> Gram -> Gram
getRoot Nothing wrd = wrd
getRoot (Just strippedWrd) _ = strippedWrd