module Classifier where

import Parser

-- need to explore built-in Matrix types, or fine tune custom type
type Matrix= [[Int]]

-- create a matrix where each row is a vectorized sentence, plus last column being 
-- a classification vector (where 1 corresponds to "spam" and 0 to "ham")
makeTrainMatrix :: [Sentence] -> [Int] -> Matrix
makeTrainMatrix [] cls = [[]]

-- compute one of P(Gram=0|Spam) P(Gram=1|Spam) P(Gram=0|Ham) P(Gram=1|Ham) for each Gram in Corpus
-- boolean parameters correspond to Gram = 0/1, Class = Spam/Ham
computeCondProb :: Corpus -> Matrix -> Bool -> Bool -> [Double]
computeCondProb [] m g s = []

-- given a sentence, conditional probabilities for each word in Corpus and marginal probabilities
-- for each class, classify sentence as "spam" or "ham"
classifySentence :: Sentence -> [[Double]] -> [Double] -> Bool
classifySentence [] pw pc = False
