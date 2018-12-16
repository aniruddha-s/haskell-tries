import           Control.Monad
import           Data.Char          (isAlpha, isPunctuation, toLower)
import           Data.Hashable      (hash)
import           Data.List          (dropWhile, dropWhileEnd)
import qualified Data.Map.Strict    as Map
import           Data.Typeable      (typeOf)
import           System.Environment (getArgs)

data Trie a = Trie (Map.Map a (Trie a)) Bool

empty :: Trie a
empty = Trie Map.empty False

insert :: Ord a => [a] -> Trie a -> Trie a
insert []           (Trie t _)   = Trie t True
insert word@(c:cs) (Trie t end) =
   case Map.lookup c t of
      Nothing   -> insert word (Trie (Map.insert c empty t) end)
      Just trie -> Trie (Map.insert c (insert cs trie) t) end

-- size :: Trie a -> Int
-- size (Trie t end) = (if end then 0 else 1) + length (concat [map (char :) (autocomplete [] trie) | (char, trie) <- Map.toList tries])
-- size (firstChar:rest) (Trie tries _) = maybe [] (map (firstChar :) . autocomplete rest) (Map.lookup firstChar tries)


lookupChar :: Ord a => [a] -> Trie a -> Bool
lookupChar []     (Trie _ end) = end
lookupChar (c:cs) (Trie t _)   = maybe False (lookupChar cs) (Map.lookup c t)


autocomplete :: Ord a => [a] -> Trie a -> [[a]]
autocomplete [] (Trie tries wordEnd) =
  (if wordEnd then [[]] else []) ++
  concat [map (char :) (autocomplete [] trie) | (char, trie) <- Map.toList tries]
autocomplete (firstChar : rest) (Trie tries _) =
  maybe [] (map (firstChar :) . autocomplete rest) (Map.lookup firstChar tries)


insertTrie :: Ord a => Trie a -> [[a]] -> Trie a
insertTrie t []     = t
insertTrie t (x:xs) = insertTrie (insert x t) xs

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

auxmain :: Trie Char -> IO()
auxmain t = do
             print ("Enter input to check for autocomplete: ")
             line <- getLine
             unless (line == "*q") $ do
             print (autocomplete line t)
             auxmain t

main :: IO()
main = do
         args <- getArgs
         if length args < 1
            then print "Usage: TextFile.txt"
            else do
                  fileContent <- readFile (args!!0)
                  let cleaned = processFile fileContent
                      new = insert "abc" empty
                      result = insertTrie new cleaned
                  auxmain result
         print ("Quitting! ")
