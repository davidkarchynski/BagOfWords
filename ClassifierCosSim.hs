module ClassifierCosSim where

import CustomTypes
import Data.Foldable
import Data.Sparse.SpVector (toListSV, SpVector, fromListSV)
-- import Vectorizer -- use for testing

-- there are 2 input matrices: one for each category (i.e., spam/ham)
-- in each matrix each row is a vectorized sentence 
-- input vector is vectorized sentence we want to classify
-- returns true if sentence classified as spam and false otherwise
--classifySentenceCosSim :: Matrix -> Matrix -> Vector -> Bool
classifySentenceCosSim spamM hamM v = (cosSpam > cosHam)
                                where
                                    cosSpam = vectorCosine spamVector v
                                    cosHam = vectorCosine hamVector v
                                    spamVector = matrixToVector spamM
                                    hamVector = matrixToVector hamM

-- x = sparsifyVectSentence (2, [(0, 1), (1, 0)])
-- y = sparsifyVectSentence (2, [(0, 0), (1, 1)])
-- classifySentenceCosSim [x] [y] x should be True
-- classifySentenceCosSim [y] [x] x should be False

-- given an m x n matrix representing m vectorized sentences of n grams
-- returns a vector with the average direction (summation) of all vectorized sentences
matrixToVector :: (Eq a, Num a) => [SpVector a] -> SpVector a
matrixToVector [] = fromListSV 0 []
matrixToVector (h:t) = foldl (\ acc v -> vectorSum acc v) h t
-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 4)])
-- matrixToVector [x, y] should return SV (4) [(0,1),(1,5),(2,1),(3,1)]
-- matrixToVector [] should return SV (0) []

-- given two vectors of the same dimension, return their vector sum
vectorSum :: (Eq a, Num a) => SpVector a -> SpVector a -> SpVector a
vectorSum v1 v2 = fromListSV (length v1) (elemWithSameInd ++ elemWithDiffIndices)
        where
             elemWithSameInd = filter ((-1, -1)/=) [if (i1==i2) then (i1, e1+e2) else (-1, -1)| (i1, e1) <- l1, (i2, e2) <- l2]
             sameInd = map (fst) elemWithSameInd
             elemWithDiffIndices = foldl' (\acc (i,e) -> if (i `elem` sameInd) then acc else acc ++ [(i,e)]) [] (l1++l2)
             l1 = toListSV v1
             l2 = toListSV v2

-- vectorSum [(0, 1), (1, 1), (2, 1), (3, 1)] [(1, 4)] should return [(1,5)]


-- given two vectors of the same dimension
-- calculates the cosine of the angle between them (number between 0 and 1 inclusive)
-- a result of 1 means that the two vectors have the same direction
-- a result of 0 means that the two vectors are orthogonal
-- ie. the larger the result, the more similar in direction are the two vectors
vectorCosine :: Floating a => SpVector Int -> SpVector Int -> a
vectorCosine v1 v2 = (fromIntegral (dotProduct v1 v2)) / productOfLengths 
    where productOfLengths = (vectorLength v1) * (vectorLength v2)

-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 4)])
-- vectorCosine y y should return 1
-- vectorCosine x y should return 0.5

-- given two vectors of the same dimension
-- calculates their dot product
dotProduct :: (Eq b, Num b) => SpVector b -> SpVector b -> b
dotProduct v1 v2 = sum $ map (snd) (filter ((-1, -1)/=) [if (i1==i2) then (i1, e1*e2) else (-1, -1)| (i1, e1) <- toListSV v1, (i2, e2) <- toListSV v2])

-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 4)])
-- dotProduct y y should return 16
-- dotProduct x x should return 4
-- dotProduct x y should return 4

-- calculates the length of a given vector
-- works only for sparse vector of 1s
vectorLength :: (Floating a) => Vector -> a
vectorLength v = sqrt (fromIntegral (sumOfSquares))
    where sumOfSquares = foldl' (\ acc (i, e) -> e^2 + acc) 0 (toListSV v)

-- vectorLength $ sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)]) = 2.0
-- vectorLength $ sparsifyVectSentence (4, [(1, 4)]) = 1.0
