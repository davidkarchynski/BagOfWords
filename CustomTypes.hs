module CustomTypes
    (Gram,              -- n-gram (word/stem, bigram etc.
     Sentence,          -- list of words
     Corpus,            -- list of words
     Vector,            -- list of Ints
     Matrix,            -- list of Vectors 
     Strategy           -- (Matrix -> Matrix -> Vector -> Bool)
    ) where
    
{-
Since we constantly deal with nested lists of Char, let's adopt some convention for readability:
[Char] refers to delimeters/suffixes
[[Char]] refers to a list of delimeters/suffixes
String refers to the initial unparsed text message
Gram refers to n-gram (word/stem, bigram etc.)
-}
type Gram = [Char]
type Sentence = [Gram]
type Corpus = [Gram]

type Vector = [Int] 
type Matrix = [Vector]  -- note that each vector is a row in the matrix

type Strategy = (Matrix -> Matrix -> Vector -> Bool)