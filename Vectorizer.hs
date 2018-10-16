module Vectorizer where

-- cabal install sparse-linear-algebra-0.3.1
import Data.Sparse.SpVector (fromListSV, SpVector, dotS)
import Parser
import CustomTypes

-- Produce a sparse vector of 0s and 1s, where a number at position n in output list
-- corresponds to a Gram at position n in Corpus: 1 when Gram is in the sentence
-- 0 when Gram is missing. length of Corpus = length of output list
vectorizeSentence :: Corpus -> Sentence -> (Int, [(Int, Int)])
vectorizeSentence c s  = (length c, foldl (\acc (gr, indx) -> if (elem gr s) then acc++[(indx, 1)] else acc) [] indexedCorpus)
                            where indexedCorpus = zip c [0..]

-- vectorizeSentence ["alert", "password", "security", "username"] ["please", "give", "us", "your", "username", "and", "password"]
--      should return (4,[(1,1),(3,1)])
-- vectorizeSentence ["alert", "password", "security", "username"] ["please", "give", "us", "your", "first", "and", "last", "name"]
--      should return (4, [])
-- vectorizeSentence ["alert", "password", "security", "username"] ["security", "alert", "we", "need", "your", "username", "and", "password"]
--      should return (4, [(0,1),(1,1),(2,1),(3,1)])
-- vectorizeSentence [] ["security", "alert", "we", "need", "your", "username", "and", "password"]
--      should return (0, [])
-- vectorizeSentence ["alert", "password", "security", "username"] []
--      should return (4, [])

-- sparsifyVectSentence :: Foldable t => (Int, t (Int, a)) -> SpVector a
sparsifyVectSentence (size, v) = fromListSV size v

-- sparsifyVectSentence (4,[(1,1),(3,1)]) should be SV (4) [(1,1),(3,1)]
-- sparsifyVectSentence (4, [(0,1),(1,1),(2,1),(3,1)]) should be SV (4) [(0,1),(1,1),(2,1),(3,1)]
-- sparsifyVectSentence (4, []) should be SV (4) []
-- sparsifyVectSentence (0, []) should be SV (0) []