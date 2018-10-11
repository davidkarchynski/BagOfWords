module Corpufier where

import Parser
import Data.List

-- pass a sanitized/filtered list of sentences to get a set of unique grams
-- sorted alphabetically

createCorpus :: [Sentence] -> Corpus
createCorpus [] = []
createCorpus sentences =
    removeDups (concat sentences)
-- createCorpus [["what","thing","call","love"], ["what","thing","matter","in","life"], ["i","call","day"]]
--     should return ["call","day","i","in","life","love","matter","thing","what"]

removeDups :: (Ord a) => [a] -> [a]
removeDups = map head . group . sort