import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.IO
import Data.Maybe
import Parser
import Classifier
import ClassifierCosSim
import Corpufier
import Vectorizer
import Data.List
import Data.Ord (comparing)
import CustomTypes
import Data.Bool

-- To run it, try:
-- ghci
-- :load Bag
-- main

main :: IO ()
main =
    do
        putStrLn "Starting Bag-Of-Words Spam/Ham Classifier program..."
        (vectSpams, vectHams, corpus) <- loadLearningData
        () <- uiLoop vectSpams vectHams corpus
        return ()
        
uiLoop :: Matrix -> Matrix -> Corpus -> IO ()
uiLoop vectSpams vectHams corpus =
    do
        strat <- choiceDriver stratSelectPrompt stratList stratMap
        putStrLn "Please enter the name of the text file you would like to classify."
        filePath <- getLine
        isSpam <- classifyFile vectSpams vectHams corpus filePath 1 strat
        let response = if (isSpam) then "This is spam" else "This is ham"
        putStrLn response
        yesNoResponse <- choiceDriver continuePrompt [] yesNoMap
        if (yesNoResponse) then uiLoop vectSpams vectHams corpus else return ()
         
         
continuePrompt = "Would you like to evaluate another file? (enter y or n)"
yesNoMap = [("y", True), ("n", False)]

stratSelectPrompt = "Please select a classifying strategy (enter 1 or 2)."
stratList = ["1. Naive Bayes", "2. Cosine Similarity"]
stratMap = [("1", classifySentence), ("2", classifySentenceCosSim)]
         
-- given a prompt, list of options, and a map of options to return values
-- asks user to choose an option and return the value corresponding to the option picked
-- if user chooses an invalid option, will continue to prompt user until given a valid choice         
choiceDriver :: String -> [String] -> [(String, a)] -> IO a
choiceDriver prompt options optionMap =
    do
        putStrLn prompt
        mapM_ putStrLn options
        response <- getLine
        let returnValMaybe = getVal response optionMap
        if (isNothing returnValMaybe) 
            then do choiceDriver prompt options optionMap
            else return (fromJust returnValMaybe)
        
-- given a searchString and a list of tuples (key, value),
-- returns Just value corresponding to the matching key if found
-- Nothing otherwise
getVal :: String -> [(String, a)] -> Maybe a
getVal searchString [] = Nothing
getVal searchString (h:t) = if (searchString == fst h)
                   then Just (snd h)
                   else getVal searchString t
    
-- loads learning data into memory
-- processes known ham and spam messages into a corpus and matrices to be later used for determining whether
-- a new unknown message is spam or ham
loadLearningData :: IO (Matrix, Matrix, Corpus)
loadLearningData =
    do
        file <- readFile "SMSSpamCollection"
        let values = file `seq` sortBy (comparing head) $ map (splitsep (=='\t')) (splitsep (=='\n') file)
        let groupedData = groupBy (\x y -> (head x) == (head y)) values
        let spams = map (!!1) $ groupedData !! 1
        let hams = map (!!1) (head groupedData)
        
        let parsedSpams = tfIdfFilter (map (parseGrams wordBlackList 1 dlims) spams) 0.0
        let parsedHams = tfIdfFilter (map (parseGrams wordBlackList 1 dlims) hams) 0.0
        
        let corpus = createCorpus $ parsedSpams ++ parsedHams

        let vectSpams = map (sparsifyVectSentence) (map (vectorizeSentence corpus) parsedSpams)
        let vectHams = map (sparsifyVectSentence) (map (vectorizeSentence corpus) parsedHams)
        return (vectSpams, vectHams, corpus)

-- determines whether the test file is ham/spam based on either Naive Bayes strategy or Cosine Similarity Strategy
classifyFile :: Matrix -> Matrix -> Corpus -> FilePath -> Int -> Strategy -> IO Bool
classifyFile vectSpams vectHams corpus f n classifyStrat =
    do
        newMessage <- readFile f
        let parsedNewMessage = parseGrams wordBlackList n dlims newMessage
        let newMessageVect = sparsifyVectSentence $ vectorizeSentence corpus parsedNewMessage

        let isSpam = classifyStrat vectSpams vectHams newMessageVect
        return isSpam

dlims = "\\\"\n*;,.?!:-()[] " -- don't forget to include whitespaces        
wordBlackList = ["a", "an", "the", "he", "she", "it", "they", "i", "we", "is", ""] -- include empty string        


splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep fun lst = foldr (\x (h:t) -> if fun x then []:h:t else (x:h):t) [[]] lst


