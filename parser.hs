module Parser where

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

-- split string into words using delimiters in dlims
stringToSentence :: [Char] -> String -> Sentence
stringToSentence dlims [] = [[]]
stringToSentence dlims str = splitSep (flip elem dlims) str

-- stringToSentence " ,.?!" "What? is this thing? ... called Love."
--  should return ["What","","is","this","thing","","","","","","called","Love",""]

--splitSep separates a list of elements into a list of list of elements by the given separator
splitSep :: (a -> Bool) -> [a] -> [[a]]
splitSep p [] = [[]]
splitSep p (h:t)  
    | p h = []:rest
    | otherwise = (h : head rest) : tail rest
        where rest = splitSep p t

-- take output of splitStringToWords to produce a list of n-grams 
wordsToNGrams :: Int -> Sentence -> Sentence
wordsToNGrams n [[]] = [[]]
-- wordsToNGrams dlims str = ...

-- given wrds, a list of words, remove all the words from lst
-- e.g., lst may include articles, prepositions, pronouns, etc.

filterWords :: [Gram] -> Sentence -> Sentence
filterWords [] wrds = wrds 
-- TODO: add implementation
-- filterWords wrds lst = ...

-- convert all words to lower-case stems, i.e. strip suffixes -ing, -ed, -s
sanitizeWords :: [[Char]] -> Sentence -> Sentence
sanitizeWords sfxs [] = []
-- sanitizeWords sfxs wrds = ...
