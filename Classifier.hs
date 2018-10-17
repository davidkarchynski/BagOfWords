module Classifier 
    (classifySentence,
    classSentence
    ) where
    
import CustomTypes
import Corpufier
import Data.Map (Map)
import qualified Data.Map as Map

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
-- classifySentence [[1, 0]] [[0, 1]] [1, 0] should be True
-- classifySentence [[1, 0]] [[0, 1]] [0, 1] should be False
-- classifySentence [[1, 0, 0, 0], [1, 1, 0, 0], [1, 0, 0, 1]] [[0, 1, 0, 0], [0, 0, 1, 1], [0, 0, 0, 1]] [1, 0, 0, 0] should be True

-- given a list of (spamCount, hamCount) zipped tuples
-- returns the conditional probability of each as (spamCount/totalCount, hamCount/totalCount)
getCondProb :: (Integral a, Fractional b) => [(a, a)] -> [(b, b)]
getCondProb zippedCounts = map (\(x,y) -> let intX = fromIntegral x
                                              intY = fromIntegral y
                                              intSum = intX + intY in
                                              (intX/intSum, intY/intSum)) 
                                          zippedCounts
-- getCondProb [(0, 1), (1,0)] should return [(0.0, 1.0), (1.0, 0.0)]
-- getCondProb [(1, 1), (2,2)] should return [(0.5, 0.5), (0.5, 0.5)]
-- getCondProb [(3, 1), (1,4)] should return [(0.75, 0.25), (0.2, 0.8)]
 
-- given a reference matrix mtrx and a target vector vctr
-- returns a vector of with each entry i = the number of matches between vctr's i'th entry and mtrx's i'th entry in each of its row
getAllCounts :: Matrix -> Vector -> [Int]
getAllCounts mtrx vctr = map (\ (e, index) -> countSameElement mtrx index e) indexedVector
    where indexedVector = zip vctr [0..]
-- getAllCounts [[1, 0, 1], [0, 0, 1], [1, 1, 1]] [0, 0, 1] should give [1,2,3]
-- getAllCounts [[1, 0, 1], [0, 0, 1], [1, 1, 1]] [1, 1, 1] should give [2,1,3]

-- count occurences of element elmnt in column indx in the matrix mtrx
countSameElement :: Matrix -> Int -> Int -> Int
countSameElement mtrx indx elmnt = length (filter (\ v -> v !! indx == elmnt) mtrx)
-- countSameElement [[1, 0, 1], [0, 0, 1], [1, 1, 1]] 0 1 should give 2
-- countSameElement [[1, 0, 1], [0, 0, 1], [1, 1, 1]] 1 1 should give 1

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

gramProb gram occMap totalCount =
    if gramOccurs == 0 then 0 else fromIntegral gramOccurs / fromIntegral totalCount
    where
        gramOccurs = (lookupNumOccurences gram occMap) :: Int

gramPosteriorProb gram sMap hMap nSpam nHam =
    if isInfinite prob || isNaN prob then 0 else prob
    where
        sProb = gramProb gram sMap nSpam
        hProb = gramProb gram hMap nHam
        prob = log(sProb / hProb)
