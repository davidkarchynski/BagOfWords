module Vectorizer where

import Parser

-- Produce a sparse vector of 0s and 1s, where a number at position n in output list
-- corresponds to a Gram at position n in Corpus: 1 when Gram is in the sentence
-- 0 when Gram is missing. length of Corpus = length of output list
vectorizeSentence :: Corpus -> Sentence -> [Int]
vectorizeSentence [] s = []
-- vectorizeSentence c s = ...