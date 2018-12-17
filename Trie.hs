module Trie where
import           Control.Monad
import           Data.Char          (isAlpha, isPunctuation, toLower)
import           Data.Hashable      (hash)
import           Data.List          (dropWhile, dropWhileEnd)
import qualified Data.Map.Strict    as Map
import           Data.Typeable      (typeOf)
import           System.Environment (getArgs)


data Trie k v = Trie (Map.Map k (Trie k v)) v
  deriving (Show, Read, Eq)

empty :: Num v => Trie k v
empty = Trie Map.empty (-1)


insert :: (Ord k, Num v) => [k] -> v -> Trie k v -> Trie k v
insert [] v (Trie k _) = Trie k v
insert word@(c:cs) v (Trie t end) =
   case Map.lookup c t of
      Nothing   -> insert word v (Trie (Map.insert c empty t) end)
      Just trie -> Trie (Map.insert c (insert cs v trie) t) end


lookup :: (Eq v, Ord k, Num v) => [k] -> Trie k v -> Bool
lookup []     (Trie _ v) = (v /= -1)
lookup (c:cs) (Trie t _) = maybe False (Trie.lookup cs) (Map.lookup c t)

getValue :: (Ord k, Num v) => [k] -> Trie k v -> v
getValue [] (Trie _ v)     = v
getValue (c:cs) (Trie t _) = maybe (-1) (Trie.getValue cs) (Map.lookup c t)
