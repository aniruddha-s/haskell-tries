{-# OPTIONS -Wall #-}

module HashArrayMppedTrie where
import Data.Sequence
import Data.Hashable
import Data.Bits

data HAMT k v = Empty
                | Leaf (k, v)
                | CollisionLeaf [(k, v)]
                | Internal Bitmap (Seq (HAMT k v))
  deriving (Show, Read, Eq)

type Bitmap = Int

-- Constuctor for an empty HAMT. Will probably add some convenience constructors later
hamtEmpty :: Hashable k => HAMT k v
hamtEmpty = Empty

-- Insert is incomplete and partially broken at this point, bitmap values are not consistent while descending levels
hamtInsert :: Hashable k => (k, v) -> HAMT k v -> HAMT k v
hamtInsert (k', v') t' = aux h' (k', v') lvls t'
  where aux h e lv Empty | lv <= 0   = (Leaf e)
                         | otherwise = Internal (bit (high5bits h) :: Int) (singleton (aux (shiftR h 5) e (lv - 1) Empty))
        h'   = hash k'
        lvls = (quot (finiteBitSize h') 5) + 1

-- function for extracting five msbs
high5bits :: Int -> Int
high5bits b = rotateR ((.&.) b mask) steps
  where mask = rotateL (32 :: Int) steps 
        steps = ((finiteBitSize b) - 4)
