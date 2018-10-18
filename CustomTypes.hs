module CustomTypes
    (Gram,              -- n-gram (word/stem, bigram etc.
     Sentence,          -- list of words
     Corpus,            -- list of words
     Vector,            -- sparse vector of 1s
     ReducedVector,     -- a vector representing the sum of all rows in a matrix and the matrix's length
     Matrix,            -- list of Vectors
     Strategy           -- (ReducedVector -> ReducedVector -> Vector -> Bool)
    ) where
    
{-
Since we constantly deal with nested lists of Char, let's adopt some convention for readability:
[Char] refers to delimeters/suffixes
[[Char]] refers to a list of delimeters/suffixes
String refers to the initial unparsed text message
Gram refers to n-gram (word/stem, bigram etc.)
-}

import Data.Sparse.SpVector (SpVector)

type Gram = [Char]
type Sentence = [Gram]
type Corpus = [Gram]

type Vector = SpVector Int
type ReducedVector = (Int, Vector) -- int holds the length of the matrix the vector represents
type Matrix = [Vector]  -- note that each vector is a row in the matrix

type Strategy = (ReducedVector -> ReducedVector -> Vector -> Bool)