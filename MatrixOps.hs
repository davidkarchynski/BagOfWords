module MatrixOps where

import CustomTypes
import Data.Bool
import Data.Foldable
import Data.Sparse.SpVector (toListSV, fromListSV, foldlWithKeySV', svDim, lookupDenseSV)
import Vectorizer -- use for testing

-- given an m x n matrix representing m vectorized sentences of n grams
-- returns a vector with the average direction (summation) of all vectorized sentences
matrixToVector :: Matrix -> Vector
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


-- returns true if vector has a single non-zero entry
validVector :: Vector -> Bool
validVector v = foldlWithKeySV' (\ acc _ e -> (e /= 0) || acc) False v