{-# LANGUAGE FlexibleContexts #-}

import           Data.Char          (isAlpha, isPunctuation, toLower)
import           Data.Hashable      (hash)
-- import qualified Data.HashMap       as HM
import           Data.List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Typeable      (typeOf)
import           System.Environment (getArgs)


processFile :: String -> [String]
processFile str = let
      goodChars c = isAlpha(c) || c `elem` "`'"
      chunks s = let (word, notGood) = span goodChars s
                     (_, rest) = span (not . goodChars) notGood
                 in word : (if rest == "" then [] else chunks rest)
      trim = (dropWhile isPunctuation) . (dropWhileEnd isPunctuation)
      lower = map toLower str
      in [trimmed | word <- chunks lower, let trimmed = trim word, trimmed /= ""]


-- insertion :: [String] -> Map k a -> Map k a
-- insertion (x:xs) countMap = if Map.member (hash x) countMap
--                                 then Map.alter (+1) (hash x) countMap
--                                 else Map.insert (hash x) 1 countMap

hashWords :: String -> Int
hashWords s = hash s



main :: IO()
main = do
        args <- getArgs
        if length args < 1
           then print "Usage: File"
           else do
                  fileContent <- readFile (args!!0)
                  let countMap = Map.empty
                  insertion (processFile fileContent) countMap
                  print (processFile fileContent)
              where insertion (x:xs) countMap = if Map.member (hash x) countMap
                                              then Map.alter (+1) (hash x) countMap
                                              else Map.insert (hash x) 1 countMap
