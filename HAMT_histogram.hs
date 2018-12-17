import           HAMT

{-# OPTIONS -Wall #-}
{-
Names:
Time spent on assignment: 3 hours
-}

import           Data.Char          (isAlpha, isPunctuation, toLower)
import           Data.Hashable
import           Data.List
import qualified Data.Map.Strict    as Map
import           Data.Typeable      (typeOf)
import           System.Environment (getArgs)
import           Data.Ord
import           Data.Function


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
insertion :: (Eq k, Hashable k, Num v) => [k] -> HAMT k v -> HAMT k v
insertion []     countMap = countMap
insertion (x:xs) countMap = if HAMT.hamtContainsKey x countMap
                              then (insertion xs (HAMT.hamtInsert (x, incValue x countMap) countMap))
                              else (insertion xs (HAMT.hamtInsert (x, 1) countMap))

incValue :: (Eq k, Hashable k, Num v) => k -> HAMT k v -> v
incValue k h = case HAMT.hamtGet k h of
                          Nothing -> 0
                          Just value -> value + 1

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe x = case x of
                Nothing -> Nothing
                Just a  -> Just (a + 1)


{-This fuction get the words from the list and looks it up in the map, and maintains a
count of the words found in the map. -}
lookupWords :: (Eq k, Hashable k, Num v) => [k] -> HAMT k v -> [(k, v)] -> [(k, v)]
lookupWords []     _        countList = countList
lookupWords (x:xs) countMap countList = lookupWords xs countMap (aux x)
        where aux word = case HAMT.hamtGetEntry x countMap of
                                  Nothing    -> countList ++ [(x, 0)]
                                  Just (_, value) -> countList ++ [(x, value)]


sortBySecond :: Ord v => [(k, v)] -> [(k, v)]
sortBySecond t = sortBy (compare `on` (\(a,b)->b)) t

histogram :: Ord v => [(k, v)] -> [(k, v)]
histogram xs = sortBySecond xs


main :: IO()
main = do
        args <- getArgs
        if length args < 2
           then print "Usage: BookFile.txt WordFile.txt"
           else do
                  fileContent <- readFile (args!!0)
                  sWords <- readFile (args!!1)
                  let hamtempty = HAMT.hamtEmpty
                      wordList = lines sWords
                      cleaned = processFile fileContent
                      result = insertion (cleaned) hamtempty
                      counts = lookupWords wordList result []
                  mapM_ go (histogram counts)
               where go (h, l) = putStrLn $ show h ++ " " ++ replicate l '*'
