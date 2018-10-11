module ClassifierCosSim
    (vectorCosine
    ) where

import CustomTypes

-- given two vectors of the same dimension
-- calculates the cosine of the angle between them (number between 0 and 1 inclusive)
-- a result of 0 means that the two vectors have the same direction
-- a result of 1 means that the two vectors are orthogonal
-- ie. the smaller the result, the more similar in direction are the two vectors
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
    where sumOfSquares = foldr (\ e acc -> e^2 + acc) 0 v
-- vectorLength [1, 1, 1, 1] = 2.0
-- vectorLength [3, 4] = 5.0