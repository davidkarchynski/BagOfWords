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

-- To run it, try:
-- ghci
-- :load Bag
-- main

main :: IO ()
main =
    do
        putStrLn "Starting Bag-Of-Words Spam/Ham Classifier program..."
        strat <- choiceDriver stratSelectPrompt stratList stratMap
        putStrLn "Please enter the name of the text file you would like to classify."
        filePath <- getLine
        isSpam <- classifyFile filePath 1 strat
        let response = if (isSpam) then "This is spam" else "This is ham"
        putStrLn response
        return ()
         
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
        
-- to train the model need to provide a path to folder with 2 subfolders
-- one subfolder will contain "spam" texts, the second - "ham"

-- driver of the program
-- fldr is a subfolder in the current directory containing 2 subfolders "spam" and "ham"
-- pick your n to split text into n-grams


classifyFile :: FilePath -> Int -> Strategy -> IO Bool
classifyFile f n classifyStrat = do
                        --content <- listFldrContent "train"
                        --dir <- getCurrentDirectory
                        --let dirTrainSpam = dir ++ "/train/" ++ head content ++ "/"
                        --let dirTrainHam = dir ++ "/train/" ++ head (tail content) ++ "/"
                        --spams <- getDirectoryContents dirTrainSpam 
                        --hams <- getDirectoryContents dirTrainHam  
                        --spamStrings <- mapM readFile (map (dirTrainSpam ++) $ filter (`notElem` [".", ".."]) spams) 
                        --hamStrings <- mapM readFile (map (dirTrainHam ++) $ filter (`notElem` [".", ".."]) hams) 

                        file <- readFile "SMSSpamCollection"
                        let values = file `seq` sortBy (comparing head) $ map (splitsep (=='\t')) (splitsep (=='\n') file)
                        let groupedData = groupBy (\x y -> (head x) == (head y)) values
                        let spams = map (!!1) $ groupedData !! 1
                        let hams = map (!!1) (head groupedData)
                        
                        -- can later change these to read from relevant files
                        let dlims = "\n;,.?!:-()[] " -- don't forget to include whitespaces
                        let wordBlackList = ["a", "an", "the", "he", "she", "it", "they", "i", "we", "is", ""] -- include empty string
                        
                        --let parsedSpams = map (parseGrams wordBlackList n dlims) spamStrings
                        --let parsedHams = map (parseGrams wordBlackList n dlims) hamStrings  
                        let parsedSpams = tfIdfFilter (map (parseGrams wordBlackList n dlims) spams) 0.0--3.3
                        let parsedHams = tfIdfFilter (map (parseGrams wordBlackList n dlims) hams) 0.0--3.6
                        
                        let corpus = createCorpus $ parsedSpams ++ parsedHams

                        let vectSpams = map (sparsifyVectSentence) (map (vectorizeSentence corpus) parsedSpams)
                        let vectHams = map (sparsifyVectSentence) (map (vectorizeSentence corpus) parsedHams)

                        -- classify
                        newMessage <- readFile f
                        let parsedNewMessage = parseGrams wordBlackList n dlims newMessage
                        let newMessageVect = sparsifyVectSentence $ vectorizeSentence corpus parsedNewMessage

                        let isSpam = classifyStrat vectSpams vectHams newMessageVect
                        return isSpam

                        
-- check that subfolders spam/ham are in the target folder
-- if at least one is missing then return an empty list
-- otherwise return a list containing only 2 elements: spam/ham
listFldrContent :: [Char] -> IO [[Char]]
listFldrContent fldr = do
                          dir <- getCurrentDirectory
                          contents <- getDirectoryContents $ dir ++ "/" ++ fldr
                          let check = verifySubFolders contents ["spam", "ham"] 
                          if check
                             then  return ["spam", "ham"]  
                             else return []
                  
-- verify that list of subfolders contains the subfolders of interst
verifySubFolders :: [[Char]] -> [[Char]] -> Bool
verifySubFolders lst sbs = foldr (&&) True [doesListContainSubFolder lst x| x <- sbs]

-- check if string is contained in the list of strings
doesListContainSubFolder :: [[Char]] -> [Char] -> Bool
doesListContainSubFolder [] s = False 
doesListContainSubFolder (hd:tl) s
                                   | hd == s = True
                                   | otherwise = doesListContainSubFolder tl s

splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep fun lst = foldr (\x (h:t) -> if fun x then []:h:t else (x:h):t) [[]] lst


