import System.Directory (getDirectoryContents, getCurrentDirectory)
import Parser
import Classifier
import Corpufier
import Vectorizer


-- to train the model need to provide a path to folder with 2 subfolders
-- one subfolder will contain "spam" texts, the second - "ham"

-- construct conditional and marginal probabilities from docs in filepath
-- assuming that two subfolders in filepath are "spam" and "ham",
-- use location of a doc in either subfolder to determine its class
buildProb :: FilePath -> [[Double]]
buildProb fldr = []


-- driver of the program
-- fldr is a subfolder in the current directory containing 2 subfolders "spam" and "ham"

-- temp signature used for debugging
-- classifyFile :: FilePath -> IO [Sentence]

classifyFile :: FilePath -> IO Bool
classifyFile f = do
                        content <- listFldrContent "train"
                        dir <- getCurrentDirectory
                        let dirTrainSpam = dir ++ "/train/" ++ head content ++ "/"
                        let dirTrainHam = dir ++ "/train/" ++ head (tail content) ++ "/"
                        spams <- getDirectoryContents dirTrainSpam 
                        hams <- getDirectoryContents dirTrainHam  
                        spamStrings <- mapM readFile (map (dirTrainSpam ++) $ filter (`notElem` [".", ".."]) spams) 
                        hamStrings <- mapM readFile (map (dirTrainHam ++) $ filter (`notElem` [".", ".."]) hams) 
                        let allString = spamStrings ++ hamStrings 
                        
                        --------------------------------------------------------
                        -- build corpus from spam and ham
                        
                        -- can later change these to read from relevant files
                        let dlims = ",.?!"
                        let wordBlackList = ["a", "the", "he", "she", "it", "they", "i", "we"]
                        
                        let unsanitizedSentences = map (stringToSentence dlims) allString
                        -- build prob matrices

                        let newMessage = readFile f
                        -- vectorize newMessage
                        -- classify
                        
                        return True

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


