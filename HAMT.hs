{-# OPTIONS -Wall #-}

module HAMT where
import qualified Data.Sequence as Seq
import Data.Hashable
import Data.Bits
import Data.List
import Data.Maybe

newtype HAMT k v = HAMT (Maybe (HAMTX k v))
  deriving (Show, Read, Eq)

data HAMTX k v =  Leaf (k, v)
                | CollisionLeaf [(k, v)]
                | Internal Bitmap (Seq.Seq (HAMTX k v))
  deriving (Show, Read, Eq)

type Bitmap = Int

width = finiteBitSize $ (maxBound :: Int)

stride = fromIntegral $ floor $ logBase (fromIntegral 2) (fromIntegral width)

-- Constuctor for an empty HAMT. Will probably add some convenience constructors later
hamtEmpty :: HAMT k v
hamtEmpty = HAMT Nothing


hamtFromList :: (Eq k, Hashable k) => [(k, v)] -> HAMT k v
hamtFromList = foldr (\a t -> hamtInsert a t) hamtEmpty


hamtInsert :: (Eq k, Hashable k) => (k, v) -> HAMT k v -> HAMT k v
hamtInsert (k', v') (HAMT ht) = case ht of
                          Nothing -> HAMT (Just (auxEmpty h' (k', v') lvls))
                          Just t' -> HAMT (Just (aux h' (k', v') lvls t'))
  where aux _ e _  (CollisionLeaf xs) = CollisionLeaf (replaceInList e xs)
        aux _ (k2, v2) _  (Leaf (k1, v1)) | k1 == k2  = Leaf (k1, v2)
                                          | otherwise = CollisionLeaf [(k2, v2), (k1, v1)]
        aux h e lv (Internal bm hmts) = let
                                        bmidx = highsixbits h
                                        seqidx = getIndexFor bmidx bm in
                                        if testBit bm (bmidx)
                                        then Internal bm (Seq.update seqidx (aux (shiftL h stride) e (lv - 1) (Seq.index hmts seqidx)) hmts)
                                        else Internal (setBit bm bmidx) (Seq.insertAt seqidx (auxEmpty (shiftL h stride) e (lv - 1)) hmts)
        auxEmpty h e lv | lv <= 0   = Leaf e
                        | otherwise = Internal (bit (highsixbits h) :: Int) (Seq.singleton (auxEmpty (shiftL h stride) e (lv - 1)))
        h'    = hash k'
        lvls  = quot (finiteBitSize h') stride + 1


hamtInsertWith :: (Eq k, Hashable k) => (v -> v -> v) -> (k, v) -> HAMT k v -> HAMT k v
hamtInsertWith f (k,v) t = case hamtGetEntry k t of
                            Nothing      -> hamtInsert (k,v) t
                            Just (k1,v1) -> hamtInsert (k, f v v1) t


-- function for extracting six msbs
highsixbits :: Int -> Int
highsixbits b = rotateR (b .&. mask) steps
  where mask = shiftL ((width - 1) :: Int) steps 
        steps = finiteBitSize b - stride


getIndexFor :: Int -> Int -> Int
getIndexFor bmidx bm = popCount (bm .&. ((bit bmidx) - 1))


replaceInList :: Eq k => (k, v) -> [(k, v)] -> [(k, v)]
replaceInList x xs = x:(deleteBy (\(k1,_) (k2,_) -> k1 == k2) x xs)


hamtGetEntry :: (Eq k, Hashable k) => k -> HAMT k v -> Maybe(k,v)
hamtGetEntry k' (HAMT ht) = case ht of
                        Nothing -> Nothing
                        Just t  -> aux h' k' t
  where aux _ k (CollisionLeaf xs) = find (\ (k1,_) -> k1 == k) xs
        aux _ k (Leaf (k1,v1))     = if k1 == k then Just (k1,v1) else Nothing
        aux h k (Internal bm hmts) = let
                                      bmidx = highsixbits h
                                      seqidx = getIndexFor bmidx bm in
                                      if testBit bm (bmidx)
                                      then aux (shiftL h stride) k (Seq.index hmts seqidx)
                                      else Nothing
        h'    = hash k'


hamtGet :: (Eq k, Hashable k) => k -> HAMT k v -> Maybe v
hamtGet k ht = case hamtGetEntry k ht of
                Nothing     -> Nothing
                Just (_,v1) -> Just v1


hamtContainsKey :: (Eq k, Hashable k) => k -> HAMT k v -> Bool
hamtContainsKey k ht = isJust $ hamtGetEntry k ht


hamtSize :: HAMT k v -> Int
hamtSize (HAMT ht) = case ht of
                      Nothing -> 0
                      Just t  -> auxsize t
  where auxsize (CollisionLeaf xs) = length xs
        auxsize (Leaf _)           = 1
        auxsize (Internal _ hmts)  = foldr (\x sz -> sz + auxsize x) 0 hmts

hamtAsList :: HAMT k v -> [(k,v)]
hamtAsList (HAMT ht) = case ht of
                      Nothing -> []
                      Just t  -> auxlist t
  where auxlist (CollisionLeaf xs) = xs
        auxlist (Leaf x)           = [x]
        auxlist (Internal _ hmts)  = foldr (\x xs -> auxlist x ++ xs) [] hmts