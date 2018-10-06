import Parser
import Classifier
import Corpufier
import Vectorizer


-- to train the model need to provide a path to folder with 2 subfolders
-- one subfolder will contain "spam" texts, the second - "ham"

-- construct Corpus from docs in filepath
buildCorpus :: FilePath -> IO Corpus
buildCorpus fldr = do
                     return []

-- construct conditional and marginal probabilities from docs in filepath
-- assuming that two subfolders in filepath are "spam" and "ham",
-- use location of a doc in either subfolder to determine its class
buildProb :: FilePath -> IO [[Double]]
buildProb fldr = do
                   return []

-- driver of the program, fldr contains training data
classifyFile :: FilePath -> FilePath -> IO Bool
classifyFile f fldr = do
                        return True 




 

