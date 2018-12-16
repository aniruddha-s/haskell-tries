import           Test.QuickCheck
import           Test.QuickCheck.Function

prop_empty :: Bool
prop_empty = HAMT.size (HAMT.hamtEmpty) == 0

prop_insert :: Trie t -> k -> v -> Bool
prop_insert t k v = (HAMT.hamtContainsKey k (HAMT.insert (k, v) t))

prop_insert_into_empty :: k -> v -> Bool
prop_insert_into_empty k v = HAMT.size (HAMT.hamtInsert (k, v) (HAMT.hamtEmpty)) == 1
