module ClassifierCosSim 
        (classifySentenceCosSim) where

import MatrixOps
import CustomTypes
import Vectorizer -- use for testing

-- there are 2 input matrices: one for each category (i.e., spam/ham)
-- in each matrix each row is a vectorized sentence 
-- input vector is vectorized sentence we want to classify
-- returns true if sentence classified as spam and false otherwise
classifySentenceCosSim :: ReducedVector -> ReducedVector -> Vector -> Bool
classifySentenceCosSim (_, spamV) (_, hamV) v = (cosSpam > cosHam)
                                                            where
                                                                cosSpam = vectorCosine spamV v
                                                                cosHam = vectorCosine hamV v

-- x = sparsifyVectSentence (2, [(0, 1), (1, 0)])
-- y = sparsifyVectSentence (2, [(0, 0), (1, 1)])
-- classifySentenceCosSim (1, x) (1, y) x should be True
-- classifySentenceCosSim (1, y) (1, x) x should be False


-- given two vectors of the same dimension
-- calculates the cosine of the angle between them (number between 0 and 1 inclusive)
-- a result of 1 means that the two vectors have the same direction
-- a result of 0 means that the two vectors are orthogonal
-- ie. the larger the result, the more similar in direction are the two vectors
vectorCosine :: Floating a => Vector -> Vector -> a
vectorCosine v1 v2 = (fromIntegral (dotProduct v1 v2)) / productOfLengths 
    where productOfLengths = (vectorLength v1) * (vectorLength v2)

-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 4)])
-- vectorCosine y y should return 1
-- vectorCosine x y should return 0.5