module Vectorizer where

import Parser

-- Produce a sparse vector of 0s and 1s, where a number at position n in output list
-- corresponds to a Gram at position n in Corpus: 1 when Gram is in the sentence
-- 0 when Gram is missing. length of Corpus = length of output list
vectorizeSentence :: Corpus -> Sentence -> [Int]
vectorizeSentence c s = foldr (\ corpusGram acc -> if (elem corpusGram s) then 1:acc else 0:acc) [] c

-- vectorizeSentence ["alert", "password", "security", "username"] ["please", "give", "us", "your", "username", "and", "password"]
--      should return [0, 1, 0, 1]
-- vectorizeSentence ["alert", "password", "security", "username"] ["please", "give", "us", "your", "first", "and", "last", "name"]
--      should return [0, 0, 0, 0]
-- vectorizeSentence ["alert", "password", "security", "username"] ["security", "alert", "we", "need", "your", "username", "and", "password"]
--      should return [1, 1, 1, 1]
-- vectorizeSentence [] ["security", "alert", "we", "need", "your", "username", "and", "password"]
--      should return []
-- vectorizeSentence ["alert", "password", "security", "username"] []
--      should return [0, 0, 0, 0]