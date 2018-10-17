# BagOfWords
Spam detection in Haskell

Classification


There are several strategies for classifying new sentences:

(a) Naive Bayes:

(b) Cosine distance between the unclassified sentence and classified sentences

An alternative approach is to compute a resultant vector for each category (by summing the vectors with a given label) and then compute cosine between the taraget vector and resultants. Smaller cosine determines category.
