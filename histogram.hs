import           Data.Char          (isAlpha, isPunctuation, toLower)
import           Data.Hashable      (hash)
-- import qualified Data.HashMap       as HM
import           Data.List
-- import           Data.Map.Strict    (Map)
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


insertion :: Num a => [String] -> Map.Map Int a -> Map.Map Int a
insertion []     countMap = countMap
insertion (x:xs) countMap = if Map.member (hash x) countMap
                            then (insertion xs (Map.alter (incMaybe) (hash x) countMap))
                            else (insertion xs (Map.insert (hash x) 1 countMap))

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe x = case x of
                Nothing -> Nothing
                Just a  -> Just (a + 1)

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
                      cleaned = processFile fileContent
                      result = insertion (cleaned) countMap
                  print (result)
