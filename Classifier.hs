module Classifier 
    (classifySentence
    ) where

type Vector = [Int] 
type Matrix = [Vector]


-- there are 2 input matrices: one for each category (i.e., spam/ham)
-- in each matrix each row is a vectorized sentence 
-- input vector is vectorized sentence we want to classify
classifySentence :: Matrix -> Matrix -> Vector -> Bool
classifySentence spamM hamM v = if (pSpam > pHam) then True else False
                                where 
                                     condPSpam = map (fst) condPs           
                                     margPSpam = (fromIntegral $ length spamM)/(fromIntegral ((length spamM) + (length hamM)))
                                     pSpam = (foldr(*) 1 condPSpam)*margPSpam
                                     
                                     condPHam = map (snd) condPs
                                     margPHam = 1 - margPSpam
                                     pHam = (foldr(*) 1 condPHam)*margPHam
 
                                     condPs = map (\(x,y) -> let intX = fromIntegral x
                                                                 intY = fromIntegral y
                                                                 intSum = fromIntegral (x + y) in
                                                             (intX/intSum, intY/intSum)) 
                                                 zippedCounts
                                     spamCount = getAllCounts spamM v
                                     hamCount = getAllCounts hamM v
                                     zippedCounts = (zip spamCount hamCount)
-- some basic tests
-- classifySentence [[1, 0]] [[0, 1]] [1, 0] should be True
-- classifySentence [[1, 0]] [[0, 1]] [0, 1] should be False
-- classifySentence [[1, 0, 0, 0], [1, 1, 0, 0], [1, 0, 0, 1]] [[0, 1, 0, 0], [0, 0, 1, 1], [0, 0, 0, 1]] [1, 0, 0, 0] should be True

getAllCounts :: Matrix -> Vector -> [Int]
getAllCounts mtrx vctr = foldl (\acc x -> acc ++ [countSameElement mtrx (length acc) x]) [] vctr
-- getAllCounts [[1, 0, 1], [0, 0, 1], [1, 1, 1]] [0, 0, 1] should give [1,2,3]
-- getAllCounts [[1, 0, 1], [0, 0, 1], [1, 1, 1]] [1, 1, 1] should give [2,1,3]

-- count occurences of element elmnt in column indx in the matrix mtrx
countSameElement :: Matrix -> Int -> Int -> Int
countSameElement mtrx indx elmnt = foldr (\v acc -> if (v !! indx == elmnt) then (acc + 1) else acc) 0 mtrx

-- countSameElement [[1, 0, 1], [0, 0, 1], [1, 1, 1]] 0 1 should give 2
-- countSameElement [[1, 0, 1], [0, 0, 1], [1, 1, 1]] 1 1 should give 1