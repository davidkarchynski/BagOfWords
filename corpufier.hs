module Corpufier where

import Parser

-- pass a sanitized/filtered list of sentences to get a set of unique grams
-- sorted alphabetically
createCorpus :: [Sentence] -> Corpus
createCorpus [] = []
