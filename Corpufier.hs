module Corpufier where

import Parser
import Data.List
import CustomTypes
import Data.Map (Map)
import qualified Data.Map as Map

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

-- numOfGramOccurs takes a list of sanitized sentences and produces a list of tuples tup where 'fst tup' is a gram,
-- and 'snd tup' is a number of documents containing the gram, sorted alphabetically
-- Note: we can use the returned list to create a corpus if we 'map fst lst'.
--       The output list can also be passed to gramOccursMap function to construct
--       a map of gram occurences across documents
gramOccursMap sentences =
    Map.fromList . map (\lst -> (head lst, length lst)) $ group $ sort $ concat $ map removeDups sentences

-- -- takes in a list produced by numOfGramOccurs and returns a map that maps a gram to a total number of documents
-- -- where gram occurs
-- gramOccursMap lst = Map.fromList lst

-- takes a map of gram occurences across documents and a gram
-- returns a number of documents containing word
lookupNumOccurences gram map = case Map.lookup gram map of
    Nothing -> 0
    Just freq -> freq

-- takes in a sentence (i.e a doc), a gram occurences map, total number of docs and a gram 
-- to get the gram's frequency value
gramFrequency sentence nDocs occMap gram = 
    if isNaN freq then 0 else freq
    where
        occurs = count gram sentence
        tf = 1 + logBase (fromIntegral 10) (fromIntegral occurs)
        idf = logBase (fromIntegral 10) (fromIntegral nDocs / fromIntegral totalOccurs)
        freq = tf * idf
        totalOccurs = lookupNumOccurences gram occMap

-- returns the number of occurences of x in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

tfIdf docs =
     [(\(doc, freqVect) -> filterZeroFreq doc freqVect) tup | tup <- zip docs freqVectors]
     where
        nDocs = length docs
        occMap = gramOccursMap docs
        freqVectors = map (frequencyVector nDocs occMap) docs


frequencyVector nDocs occMap sentence =
    map (gramFrequency sentence (fromIntegral nDocs) occMap) sentence

filterZeroFreq sentence freqVect =
    [fst tup| tup <- zip sentence freqVect, (snd tup) > 0]