module Classifier 
    (classifySentence
    ) where
    
import CustomTypes
import Corpufier
import Data.Map (Map)

import Data.Sparse.SpVector (SpVector, foldlWithKeySV', lookupDenseSV, svDim)
import Vectorizer -- for testing
import MatrixOps  -- for testing


-- there are 2 input matrices: one for each category (i.e., spam/ham)
-- in each matrix each row is a vectorized sentence 
-- input vector is vectorized sentence we want to classify
-- returns true if sentence classified as spam and false otherwise
classifySentence :: ReducedVector -> ReducedVector -> Vector -> Bool
classifySentence (spamL, spamV) (hamL, hamV) v =
    (pSpam > pHam)
        where 
             condPSpam = map (fst) condPs           
             margPSpam = (fromIntegral $ spamL)/(fromIntegral $ spamL + hamL)
             pSpam = (product condPSpam)*margPSpam
             
             condPHam = map (snd) condPs
             margPHam = 1 - margPSpam
             pHam = (product condPHam)*margPHam

             condPs = getCondProb (zip spamCount hamCount)
             spamCount = getAllCounts spamL spamV v
             hamCount = getAllCounts hamL hamV v
-- some basic tests
-- x = sparsifyVectSentence (2, [(0, 1), (1, 0)])
-- y = sparsifyVectSentence (2, [(0, 0), (1, 1)])
-- classifySentence (1, x) (1, y) x should be True
-- classifySentence (1, y) (1, x) x should be False


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
getAllCounts :: Int -> Vector -> Vector -> [Int]
getAllCounts mtrxLen rVec v = foldlWithKeySV' (\acc i e -> acc ++ [getMatches i e]) [] v
    where
          lookupVal i = lookupDenseSV i rVec
          getMatches i e = if (e==0) then mtrxLen - (lookupVal i)
                                     else lookupVal i

-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 1)])
-- getAllCounts 1 x x should be [1,1,1,1] 
-- getAllCounts 2 (matrixToVector [x, x]) x should be [2,2,2,2]
-- getAllCounts 2 (matrixToVector [x, y]) x should be [1,2,1,1]
-- getAllCounts 2 (matrixToVector [y, y]) x sould be [0,2,0,0]
-- getAllCounts 2 (matrixToVector [x, x]) y sould be [2]
