import           Control.Monad
import qualified Data.Map.Strict as Map

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

main :: IO()
main = do
         let new = insert "abc" empty
             s = ["public", "domain", "in", "the", "u", "s", "unless", "a", "copyright", "notice", "is", "included", "thus", "we", "do", "not", "necessarily", "particular", "paper", "edition", "people", "start", "at", "pg", "search", "project", "gutenberg"]
             result = insertTrie new s
         line <- getLine
         unless (line == "*q") $ do
           print (autocomplete line result)
           main
         -- print (autocomplete "a" cfg)
