{-# OPTIONS -Wall #-}
{-
Names: Jainey and Aniruddha
Time spent on assignment: 3 hours
-}

import           Data.Char          (isAlpha, isPunctuation, toLower)
import           Data.Hashable      (hash)
import           Data.List
import qualified Data.Map.Strict    as Map
import           Data.Typeable      (typeOf)
import           System.Environment (getArgs)


{- This function is used to clean the text file and only store the alphabetic charecters
and dispose the other type of characters. -}
processFile :: String -> [String]
processFile str = let
      goodChars c = isAlpha(c) || c `elem` "’`'"
      chunks s = let (word, notGood) = span goodChars s
                     (_, rest) = span (not . goodChars) notGood
                 in word : (if rest == "" then [] else chunks rest)
      trim = (dropWhile isPunctuation) . (dropWhileEnd isPunctuation)
      lower = map toLower str
      in [trimmed | word <- chunks lower, let trimmed = trim word, trimmed /= ""]


{- This function is used to insert the terms from the novel into the Map by hashing
 the words and increasing the count. -}
insertion :: Num a => [String] -> Map.Map Int a -> Map.Map Int a
insertion []     countMap = countMap
insertion (x:xs) countMap = if Map.member (hash x) countMap
                            then (insertion xs (Map.alter (incMaybe) (hash x) countMap))
                            else (insertion xs (Map.insert (hash x) 1 countMap))

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe x = case x of
                Nothing -> Nothing
                Just a  -> Just (a + 1)


{-This fuction get the words from the list and looks it up in the map, and maintains a
count of the words found in the map. -}
lookupWords :: Num a => [String] -> Map.Map Int a -> [(String, a)] -> [(String, a)]
lookupWords []     countMap countList = countList
lookupWords (x:xs) countMap countList = lookupWords xs countMap (aux x)
        where aux word = case Map.lookup (hash word) countMap of
                                  Nothing    -> countList ++ [(x, 0)]
                                  Just value -> countList ++ [(x, value)]


{- In this implementation, we want to further reduce chances of collision in the
map, by maintaining a list of (word, count) tuples as the value for the hash of
the word as the key. This implementation is still under progress. -}
-- insertionTuple :: Num a => [String] -> Map.Map Int [(String, a)] -> Map.Map Int [(String, a)]
-- insertionTuple []     countMap = countMap
-- insertionTuple (x:xs) countMap = if Map.member (hash x) countMap
--                             then tupleMaybe x countMap
--                             else (insertionTuple xs (Map.insert (hash x) [(x, 1)] countMap))
--
-- tupleMaybe element map = if element `elem` fst (Map.elems map)
--                            then Map.alter (incMaybeTuple element) (hash x) map
--                            else (insertionTuple xs (Map.alter (incMaybeTuple) (hash x) countMap))
--
-- incMaybeTuple :: Num a => Maybe [(String, a)] -> Maybe [(String, a)]
-- incMaybeTuple element x = case x of
--                               Nothing     -> Nothing
--                               Just (_, a) -> Just (_, a + 1)


main :: IO()
main = do
        args <- getArgs
        if length args < 2
           then print "Usage: BookFile.txt WordFile.txt"
           else do
                  fileContent <- readFile (args!!0)
                  sWords <- readFile (args!!1)
                  let countMap = Map.empty
                      wordList = lines sWords
                      cleaned = processFile fileContent
                      result = insertion (cleaned) countMap
                      counts = lookupWords wordList result []
                  -- print (cleaned)
                  -- print (result)
                  print (counts)
