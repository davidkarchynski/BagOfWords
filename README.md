# BagOfWords
Spam detection in Haskell

1. Parsing a sentence

a) Given a sentence parse it into a sanitized list of words

   Sanitize the parsed list of strings:

Filter prepositions, articles, punctuation, pronouns, etc.

Strip suffixes, ignore letter cases and derive stems (i.e., instead of looking, looked, looks, Look -> look)

b) Extension: Given a sentence parse it into a sanitized list of bigrams (potentially n-grams, where n can be input from user)


2. Create a corpus

Given a collection of sanitized list of words produce a corpus of unique words sorted alphabetically.


3. Vectorize a sentence

Given a corpus of unique words (or bigrams) sorted alphabetically and a sanitized sentence produce a sparse vector of 0's and 1's.
   * In addition, to sanitization in step 1 we want to filter out all words that are not in corpus

For example, if our sorted corpus is [name, other, sweet, rose], then a sentence "Sweet smell of red roses" would be sanitized to [sweet, rose] and then vectorized to [0 0 1 1]

4. Classification

Sentences used to construct corpus are pre-labeled as either 1 (e.g., "spam") or 0 ("ham").
That is we need to keep track which sentence belong to which group.

There are several strategies for classifying new sentences:

(a) Naive Bayes:

To come back to our example from step 3, we compute probabilities of [0 0 1 1] being "spam" and "ham":

P(spam) = P(name=0|label=spam)*P(other=0|label=spam)*P(sweet=1|label=spam)*P(rose=1|label=spam)*P(spam)
P(ham) = P(name=0|label=ham)*P(other=0|label=ham)*P(sweet=1|label=ham)*P(rose=1|label=ham)*P(ham)

We compute each conditional and marginal probability by counting appropriate sums in the matrix of stacked vectorized training sentences.
For example, consider matrix:

0 1 0 1
0 0 0 1
1 1 1 0

which represents 3 training sanitized sentences [name rose], [rose], [name other sweet]

Suppose that the vector of labels is

1
1
0

Meaning that the first 2 sentences are spam and the last one is ham

So, e.g, P(spam)= 2/3, P(ham) = 1/3, P(name=1|spam) = 1/2, P(name=0|spam) = 1/2, P(name=1|ham)=1, P(name=0|ham)=0

To find the probability of a new sentence [a,b,c,d] being spam or ham (where a, b, c, d are either 0 or 1), we sanitize, vectorize and compute P(ham|name=a, other=b, sweet=c, rose=d) and P(spam|name=a, other=b, sweet=c, rose=d). The label is determined by comparing these probabilities.


(b) Distance (e.g., Euclidean) between the unclassified sentence and classified sentences

An alternative approach is to compute a resultant vector for each category (by summing the vectors with a given label) and then compute Euclidean distance from the new vectorized sentence to each resultant vector. Whichever is closer, determines the type.

