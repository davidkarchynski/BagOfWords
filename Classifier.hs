module Classifier 
    (classifySentence,
    classSentence) 
    where
    
import CustomTypes
import Corpufier
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Sparse.SpVector (SpVector, foldlWithKeySV', lookupDenseSV, svDim)
-- import Vectorizer -- for testing


-- there are 2 input matrices: one for each category (i.e., spam/ham)
-- in each matrix each row is a vectorized sentence 
-- input vector is vectorized sentence we want to classify
-- returns true if sentence classified as spam and false otherwise
classifySentence :: Matrix -> Matrix -> Vector -> Bool
classifySentence spamM hamM v = (pSpam > pHam)
                                where 
                                     condPSpam = map (fst) condPs           
                                     margPSpam = (fromIntegral $ length spamM)/(fromIntegral $ (length spamM) + (length hamM))
                                     pSpam = (Prelude.product condPSpam)*margPSpam
                                     
                                     condPHam = map (snd) condPs
                                     margPHam = 1 - margPSpam
                                     pHam = (Prelude.product condPHam)*margPHam
 
                                     condPs = getCondProb (zip spamCount hamCount)
                                     spamCount = getAllCounts spamM v
                                     hamCount = getAllCounts hamM v
-- some basic tests
-- x = sparsifyVectSentence (2, [(0, 1), (1, 0)])
-- y = sparsifyVectSentence (2, [(0, 0), (1, 1)])
-- classifySentence [x] [y] x should be True
-- classifySentence [y] [x] x should be False


-- given a list of (spamCount, hamCount) zipped tuples
-- returns the conditional probability of each as (spamCount/totalCount, hamCount/totalCount)
getCondProb :: (Integral a, Fractional b) => [(a, a)] -> [(b, b)]
getCondProb zippedCounts = map (\(x,y) -> let intX = fromIntegral x
                                              intY = fromIntegral y
                                              intSum = intX + intY in
                                              (intX/intSum, intY/intSum)) 
                                          zippedCounts
-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 1)])
-- getCondProb [(0, 1), (1,0)] should return [(0.0, 1.0), (1.0, 0.0)]
-- getCondProb [(1, 1), (2,2)] should return [(0.5, 0.5), (0.5, 0.5)]
-- getCondProb [(3, 1), (1,4)] should return [(0.75, 0.25), (0.2, 0.8)]


-- given a reference matrix mtrx and a target vector vctr
-- returns a vector of occurences in the matrix for each word present in vector
getAllCounts :: Matrix -> Vector -> [Int]
getAllCounts mtrx v = foldlWithKeySV' (\acc i e -> acc ++ [(countSameElement mtrx (length mtrx) i e)]) [] v

-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 1)])
-- getAllCounts [x] x should be [1,1,1,1] 
-- getAllCounts [x, x] x should be [2,2,2,2]
-- getAllCounts [x,y] x should be [1,2,1,1]
-- getAllCounts [y, y] x sould be [0,2,0,0]
-- getAllCounts [x, x] y sould be [2]

-- count occurences of 1s or 0s in column indx in the matrix mtrx
countSameElement :: Matrix -> Int -> Int -> Int -> Int
countSameElement mtrx sz indx elmnt = if (elmnt == 1) then countOnes else (sz - countOnes)
                 where 
                       countOnes = sum [if (lookupDenseSV indx v == 1) then 1 else 0 | v <- mtrx]

-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 1)])
-- countSameElement [x, x, x, y] 4 0 1 should be 3
-- countSameElement [x, x, x, y] 4 2 0 should be 1
-- countSameElement [y, y] 4 1 1 should be 2

classSentence :: [Sentence] -> [Sentence] -> [Gram] -> Bool
classSentence spams hams sentence =
    (pSentence + pHamSpam) > 0
    where
        nSpam = length $ concat spams
        nHam = length $ concat hams
        nTotal = nSpam + nHam
        sMap = gramOccursMap spams
        hMap = gramOccursMap hams
        pSpam = fromIntegral nSpam / fromIntegral nTotal
        pHam = fromIntegral nHam / fromIntegral nTotal
        pHamSpam = log (pSpam / pHam)
        pSentence = sum $ map (\gram -> gramPosteriorProb gram sMap hMap nSpam nHam) sentence

gramProb :: (Fractional p) => Gram -> Map Gram Int -> Int -> p
gramProb gram occMap totalCount =
    if gramOccurs == 0 then 0 else fromIntegral gramOccurs / fromIntegral totalCount
    where
        gramOccurs = (lookupNumOccurences gram occMap) :: Int

gramPosteriorProb :: RealFloat p => Gram -> Map Gram Int -> Map Gram Int -> Int -> Int -> p
gramPosteriorProb gram sMap hMap nSpam nHam =
    if isInfinite prob || isNaN prob then 0 else prob
    where
        sProb = gramProb gram sMap nSpam
        hProb = gramProb gram hMap nHam
        prob = log(sProb / hProb)