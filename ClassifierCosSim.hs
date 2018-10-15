module ClassifierCosSim
    (classifySentenceCosSim
    ) where

import CustomTypes
import Data.Foldable

-- there are 2 input matrices: one for each category (i.e., spam/ham)
-- in each matrix each row is a vectorized sentence 
-- input vector is vectorized sentence we want to classify
-- returns true if sentence classified as spam and false otherwise
classifySentenceCosSim :: Matrix -> Matrix -> Vector -> Bool
classifySentenceCosSim spamM hamM v = (cosSpam > cosHam)
                                where
                                    cosSpam = vectorCosine spamVector v
                                    cosHam = vectorCosine hamVector v
                                    spamVector = matrixToVector spamM
                                    hamVector = matrixToVector hamM
-- classifySentenceCosSim [[1, 0]] [[0, 1]] [1, 0] should be True
-- classifySentenceCosSim [[1, 0]] [[0, 1]] [0, 1] should be False
-- classifySentenceCosSim [[1, 0, 0, 0], [1, 1, 0, 0], [1, 0, 0, 1]] [[0, 1, 0, 0], [0, 0, 1, 1], [0, 0, 0, 1]] [1, 0, 0, 0] should be True

-- given an m x n matrix representing m vectorized sentences of n grams
-- returns a vector with the average direction (summation) of all vectorized sentences
matrixToVector :: Matrix -> Vector
matrixToVector [] = [] 
matrixToVector (h:t) = foldl (\ acc v -> vectorSum acc v) h t
-- matrixToVector [[0, 1, 2], [5, 5, 5], [1, 2, 3]] should return [6, 8, 10]
-- matrixToVector [[0, 1, 2]] should return [0, 1, 2]
-- matrixToVector [] should return []

-- given two vectors of the same dimension, return their vector sum
vectorSum :: Vector -> Vector -> Vector
vectorSum v1 v2 = [e1 + e2 | (e1, e2) <- zippedVector]
    where zippedVector = zip v1 v2
-- vectorSum [0, 0, 0] [1, 2, 3] should return [1, 2, 3]
-- vectorSum [1, 2, 3] [1, 2, 3] should return [2, 4, 6]
-- vectorSum [1, 2, 3] [9, 8, 7] should return [10, 10, 10]
-- vectorSum [] [] should return []

-- given two vectors of the same dimension
-- calculates the cosine of the angle between them (number between 0 and 1 inclusive)
-- a result of 1 means that the two vectors have the same direction
-- a result of 0 means that the two vectors are orthogonal
-- ie. the larger the result, the more similar in direction are the two vectors
vectorCosine :: (Floating a) => Vector -> Vector -> a
vectorCosine v1 v2 = (fromIntegral $ dotProduct v1 v2) / productOfLengths
    where productOfLengths = (vectorLength v1) * (vectorLength v2)
-- vectorCosine [1, 0] [2, 0]       should return 1
-- vectorCosine [1, 0, 0] [0, 1, 0] should return 0
-- vectorCosine [1, 0] [1, 1]       should return 0.7071067811865475
-- vectorCosine [1, 0, 0] [1, 1, 0] should return 0.7071067811865475

-- given two vectors of the same dimension
-- calculates their dot product
dotProduct :: Vector -> Vector -> Int
dotProduct v1 v2 = sum (map (\ (e1, e2) -> e1 * e2) zippedVectors)
    where zippedVectors = zip v1 v2
-- dotProduct [] [] = 0
-- dotProduct [0, 0, 0] [1, 1, 1] = 0
-- dotProduct [1, 2, 3] [1, 1, 1] = 6
-- dotProduct [1000, 100, 10, 1] [4, 3, 2, 1] = 4321

-- calculates the length of a given vector
vectorLength :: (Floating a) => Vector -> a
vectorLength v = sqrt (fromIntegral (sumOfSquares))
    where sumOfSquares = foldl' (\ acc e -> e^2 + acc) 0 v
-- vectorLength [1, 1, 1, 1] = 2.0
-- vectorLength [3, 4] = 5.0