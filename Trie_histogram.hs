import           Trie

{-
Names:
Time spent on assignment: 3 hours
-}

import           Data.Char          (isAlpha, isPunctuation, toLower)
import           Data.Function
import           Data.Hashable
import           Data.List
import qualified Data.Map.Strict    as Map
import           Data.Ord
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
insertion :: (Eq v, Ord k, Num v) => [k] -> Trie k v -> Trie k v
insertion []     countMap = countMap
insertion (x:xs) countMap = if Trie.lookup [x] countMap
                              then (insertion xs (Trie.insert [x] 1 countMap))
                              else (insertion xs (Trie.insert [x] 1 countMap))

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
                  let hamtempty = Trie.empty
                      wordList = lines sWords
                      cleaned = processFile fileContent
                      result = insertion (cleaned) hamtempty
                  print("Done.")
               --        counts = lookupWords wordList result []
               --    mapM_ go (histogram counts)
               -- where go (h, l) = putStrLn $ show h ++ " " ++ replicate l '*'
