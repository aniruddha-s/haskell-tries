{-# OPTIONS -Wall #-}

module HashArrayMappedTrie where
import qualified Data.Sequence as Seq
import Data.Hashable
import Data.Bits
import Data.List

newtype HAMT k v = HAMT (Maybe (HAMTX k v))
  deriving (Show, Read, Eq)

data HAMTX k v =  Leaf (k, v)
                | CollisionLeaf [(k, v)]
                | Internal Bitmap (Seq.Seq (HAMTX k v))
  deriving (Show, Read, Eq)

type Bitmap = Int

width = finiteBitSize $ maxBound :: Int


-- Constuctor for an empty HAMT. Will probably add some convenience constructors later
hamtEmpty :: Hashable k => HAMT k v
hamtEmpty = HAMT Nothing

-- Insert is incomplete and partially broken at this point, bitmap values are not consistent while descending levels
hamtInsert :: (Eq k, Hashable k) => (k, v) -> HAMT k v -> HAMT k v
hamtInsert (k', v') (HAMT ht) = case ht of
                          Nothing -> HAMT (Just (auxEmpty h' (k', v') lvls))
                          Just t' -> HAMT (Just (aux h' (k', v') lvls t'))
  where aux _ e _  (CollisionLeaf xs) = CollisionLeaf (replaceInList e xs)
        aux _ (k2, v2) _  (Leaf (k1, v1)) | k1 == k2  = Leaf (k1, v2)
                                          | otherwise = CollisionLeaf [(k2, v2), (k1, v1)]
        aux h e lv (Internal bm hmts) = let
                                        bmidx = high6bits h
                                        seqidx = getIndexFor bmidx bm in
                                        if testBit bm (bmidx)
                                        then Internal bm (Seq.update seqidx (aux (shiftL h 6) e (lv - 1) (Seq.index hmts seqidx)) hmts)
                                        else Internal (setBit bm bmidx) (Seq.insertAt seqidx (auxEmpty (shiftL h 6) e (lv - 1)) hmts)
        auxEmpty h e lv | lv <= 0   = Leaf e
                        | otherwise = Internal (bit (high6bits h) :: Int) (Seq.singleton (auxEmpty (shiftL h 6) e (lv - 1)))
        h'    = hash k'
        lvls  = quot (finiteBitSize h') 6 + 1

-- function for extracting six msbs
high6bits :: Int -> Int
high6bits b = rotateR (b .&. mask) steps
  where mask = shiftL (63 :: Int) steps 
        steps = finiteBitSize b - 6

getIndexFor :: Int -> Int -> Int
getIndexFor bmidx bm = popCount (bm .&. ((bit bmidx) - 1))

replaceInList :: Eq k => (k, v) -> [(k, v)] -> [(k, v)]
replaceInList x xs = x:(deleteBy (\(k1,_) (k2,_) -> k1 == k2) x xs)