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
import MatrixOps
import Data.Sparse.SpVector (nzSV)
import System.Directory (doesFileExist)

-- To run it, try:
-- ghci
-- :load Bag
-- main

main :: IO ()
main =
    do
        putStrLn "Starting Bag-Of-Words Spam/Ham Classifier program..."
        n <- choiceDriver nGramsSelectPrompt [] nGramsMap
        putStrLn "Loading and processing training data..."
        (vectSpams, vectHams, corpus) <- loadLearningData n
        -- forces vectSpams and vectHams to evaluate instead of being lazy
        putStrLn ((nzSV (snd vectSpams)) `seq` "finished processing part 1")
        putStrLn ((nzSV (snd vectHams)) `seq` "finished processing part 2")
        () <- uiLoop vectSpams vectHams corpus n
        return ()

        
uiLoop :: ReducedVector -> ReducedVector -> Corpus -> Int -> IO ()
uiLoop vectSpams vectHams corpus n =
    do
        strat <- choiceDriver stratSelectPrompt stratList stratMap 
        putStrLn "Please enter the name of the text file you would like to classify."
        filePath <- processFileName
        isSpam <- classifyFile vectSpams vectHams corpus filePath n strat
        let response = if (isSpam) then "This is spam" else "This is ham"
        putStrLn response
        yesNoResponse <- choiceDriver continuePrompt [] yesNoMap
        if (yesNoResponse) then uiLoop vectSpams vectHams corpus n else return ()             
         
nGramsSelectPrompt = "Please select a value for n (enter 1, 2 or 3) to use for splitting the documents into n-grams."
nGramsMap = [("1", 1), ("2", 2), ("3", 3)]

continuePrompt = "Would you like to evaluate another file? (enter y or n)"
yesNoMap = [("y", True), ("n", False)]

stratSelectPrompt = "Please select a classifying strategy. (enter 1 or 2)"
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

-- prompt user to enter a valid filepath, otherwise repeat
processFileName :: IO String
processFileName = do
                     f <- getLine  
                     check <- doesFileExist f
                     if (check) then return f else do  
                                                       putStrLn "The file name you entered is invalid."
                                                       processFileName  
      
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
loadLearningData :: Int -> IO (ReducedVector, ReducedVector, Corpus)
loadLearningData n =
    do
        file <- readFile "SMSSpamCollection"
        let values = file `seq` sortBy (comparing head) $ map (splitSep (=='\t')) (splitSep (=='\n') file)
        let groupedData = groupBy (\x y -> (head x) == (head y)) values
        let spams = map (!!1) $ groupedData !! 1
        let hams = map (!!1) (head groupedData)
        
        let parsedSpams = map (parseGrams wordBlackList n dlims) spams
        let parsedHams = map (parseGrams wordBlackList n dlims) hams
        
        let corpus = createCorpus $ tfIdfFilter (parsedSpams ++ parsedHams) 2.0

        let matrixSpams = map (sparsifyVectSentence) (map (vectorizeSentence corpus) parsedSpams)
        let matrixHams = map (sparsifyVectSentence) (map (vectorizeSentence corpus) parsedHams)
        let matrixSpamsLength = length matrixSpams
        let matrixHamsLength = length matrixHams
        let vectSpams = matrixToVector matrixSpams
        let vectHams = matrixToVector matrixHams
        
        return ((matrixSpamsLength, vectSpams), (matrixHamsLength, vectHams), corpus)

-- determines whether the test file is ham/spam based on either Naive Bayes strategy or Cosine Similarity Strategy
classifyFile :: ReducedVector -> ReducedVector -> Corpus -> FilePath -> Int -> Strategy -> IO Bool
classifyFile vectSpams vectHams corpus f n classifyStrat =
    do
        newMessage <- readFile f
        let parsedNewMessage = parseGrams wordBlackList n dlims newMessage
        let newMessageVect = sparsifyVectSentence $ vectorizeSentence corpus parsedNewMessage

        let isSpam = classifyStrat vectSpams vectHams newMessageVect
        return isSpam

dlims = "\\\"\n*;,'./+?!:<>@-=&%#$^_()[] 0123456789" -- don't forget to include whitespaces        
wordBlackList = ["a", "an", "the", "he", "she", "it", "they", "i", "we", "is", ""] -- include empty string        


