# BagOfWords
Spam detection in Haskell

We implement a variation of Bag of Words spam detection algorithm. This involves sourcing, sanitizing and parsing training data used to construct a corpus of unique elements. This corpus is then used to vectorize new unclassified sentences and label them as "spam" or not accordingly.

The program loop asks for user to select the value of n to be used in constructing n-gram corpus, classification strategy (Naive Bayes or Cosine Similarity) and file name. Intermediate data matrices are cached during the first iteration of the program making classification faster on successive runs. Our implementation takes advantage of some external code: sparse vector implementation from Data.Sparse.SpVector and Porter Stemming Algorithm (see the code for references). Besides Porter Stemming, we use term-frequency-inverse document frequency to eliminate uninformative stems (i.e., present in most documents).

To run either compile into executable: ghc --make Bag.hs -O2
Or :load Bag from ghci followed by main
