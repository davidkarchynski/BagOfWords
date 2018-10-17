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
gramOccursMap :: [Sentence] -> Map Gram Int
gramOccursMap sentences =
    Map.fromList . map (\lst -> (head lst, length lst)) $ group $ sort $ concat $ map removeDups sentences

-- -- takes in a list produced by numOfGramOccurs and returns a map that maps a gram to a total number of documents
-- -- where gram occurs
-- gramOccursMap lst = Map.fromList lst

-- takes a map of gram occurences across documents and a gram
-- returns a number of documents containing word
lookupNumOccurences :: Gram -> Map Gram Int -> Int
lookupNumOccurences gram map = case Map.lookup gram map of
    Nothing -> 0
    Just freq -> freq

-- takes in a sentence (i.e a doc), a gram occurences map, total number of docs and a gram 
-- to get the gram's frequency value
gramFrequency :: RealFloat a => Sentence -> Int -> Map Gram Int -> Gram -> a
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

-- takes in a list of parsed sentences and a threshold value and
-- filters out grams that have tf-idf values <= the threshold.
tfIdfFilter :: RealFloat b => [Sentence] -> b -> [Sentence]
tfIdfFilter docs threshold =
     filter (not . null) [(\(doc, freqVect) -> filterSentence doc freqVect threshold) tup | tup <- zip docs freqVectors]
     where
        nDocs = length docs
        occMap = gramOccursMap docs
        freqVectors = map (frequencyVector nDocs occMap) docs

-- takes in a total number of documents (nDocs) an occurence map (occMap) and a sentence
-- and returns a vector of corresponding tf-idf values for each gram in the sentence
frequencyVector :: (RealFloat b) => Int -> Map Gram Int -> Sentence -> [b]
frequencyVector nDocs occMap sentence =
    map (gramFrequency sentence nDocs occMap) sentence

-- takes in a sentence and its corresponding tf-idf vector and filters out
-- the grams that have a corresponding tf-idf value <= threshold
filterSentence :: Ord b => Sentence -> [b] -> b -> Sentence
filterSentence sentence freqVect threshold =
    [fst tup| tup <- zip sentence freqVect, (snd tup) > threshold]
-- input sentence = ["test", "gram", "is", "here"]
--       freqVect = [0.056, 0.123, 0.003, 0.044] -- corresponding tf-idf vector for the sentence
--       threshold = 0.04
-- returns ["test", "gram", "here"]