module ClassifierCosSim 
        (classifySentenceCosSim) where

import CustomTypes
import Data.Foldable
import Data.Sparse.SpVector (toListSV, fromListSV, foldlWithKeySV', svDim, lookupDenseSV)
import Vectorizer -- use for testing

-- there are 2 input matrices: one for each category (i.e., spam/ham)
-- in each matrix each row is a vectorized sentence 
-- input vector is vectorized sentence we want to classify
-- returns true if sentence classified as spam and false otherwise
classifySentenceCosSim :: [Vector] -> [Vector] -> Vector -> Bool
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
matrixToVector :: [Vector] -> Vector
matrixToVector [] = fromListSV 0 []
matrixToVector (h:t) = foldl (\ acc v -> vectorSum acc v) h t
-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 4)])
-- matrixToVector [x, y] should return SV (4) [(0,1),(1,5),(2,1),(3,1)]
-- matrixToVector [] should return SV (0) []

-- given two vectors of the same dimension, return their vector sum
vectorSum :: Vector -> Vector -> Vector
vectorSum v1 v2 = fromListSV vectLen vectSumList
        where
             vectSumList = vectorSumHelper (toListSV v1) (toListSV v2) []
             vectLen = svDim v1
             
vectorSumHelper :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
vectorSumHelper iv1 [] acc = iv1 ++ acc
vectorSumHelper [] iv2 acc = iv2 ++ acc
vectorSumHelper ((i1, e1):t1) ((i2, e2):t2) acc =
    if (i1 == i2) then vectorSumHelper t1 t2 ((i1, e1+e2):acc)
                  else if (i1 < i2)
                  then vectorSumHelper t1 ((i2, e2):t2) ((i1, e1):acc)
                  else vectorSumHelper ((i1, e1):t1) t2 ((i2, e2):acc)
             
-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 4)])
-- vectorSum x y should give SV (4) [(0,1),(1,5),(2,1),(3,1)]


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

-- given two vectors of the same dimension
-- calculates their dot product
dotProduct :: Vector -> Vector -> Int
dotProduct v1 v2 = foldlWithKeySV' (\acc i e -> acc + (lookupDenseSV i v1)*e) 0 v2
    
-- x = sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)])
-- y = sparsifyVectSentence (4, [(1, 4)])
-- dotProduct y y should return 16
-- dotProduct x x should return 4
-- dotProduct x y should return 4

-- calculates the length of a given vector
vectorLength :: (Floating a) => Vector -> a
vectorLength v = sqrt (fromIntegral (sumOfSquares))
    where sumOfSquares = foldlWithKeySV' (\ acc _ e -> e^2 + acc) 0 v

-- vectorLength $ sparsifyVectSentence (4, [(0, 1), (1, 1), (2, 1), (3, 1)]) = 2.0
-- vectorLength $ sparsifyVectSentence (4, [(1, 4)]) = 1.0
-- vectorLength $ sparsifyVectSentence (4, [(0, 0), (1, 3), (2, 2), (3, 6)]) = 7.0