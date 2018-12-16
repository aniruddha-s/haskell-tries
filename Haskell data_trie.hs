import           Data.Char
import qualified Data.Map.Strict as Map

newtype Trie k v = Trie (Map.Map k (TrieT k v))
    deriving (Eq)

data TrieT k v = Leaf (k, v)
                 | InternalNode Char (Map.Map k (TrieT k v))
    deriving (Eq, Show)

empty :: Trie k v
empty = Trie (Map.empty)

digits :: Integer -> [Integer]
digits n = [toInteger (digitToInt x) | x <- show n]

tokenizeKey :: Integer -> [[Char]]
tokenizeKey n = map (show :: Integer -> [Char]) $ digits n

-- Handle key seperation
insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert []          v (Trie t) = (Trie t)
insert keys@(k:ks) v (Trie t) = case Map.lookup k t of
                    Nothing   -> undefined
                    Just trie -> undefined
                  -- Nothing -> insert keys v (Trie (Map.insert k 0 empty)) empty)
                  -- Just trie -> Trie ((Map.insert k (insert ks trie) t) empty)

        -- where auxEmpty k v   = (Map.insert k (Leaf (k, v)) Map.empty)
        --       aux      k v t = (Map.)


main :: IO()
main = print("Done")
