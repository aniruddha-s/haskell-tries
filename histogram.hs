import           Data.Char          (isAlpha, isPunctuation, toLower)
import           Data.Hashable      (hash)
import qualified Data.HashMap       as HM
import           Data.List
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


insertion :: [String] -> HM.Map String a -> HM.Map String a
insertion (x:xs) countMap = if HM.member x countMap
                                then HM.alter (+1) x countMap
                                else HM.insert x 1 countMap

hashWords :: String -> Int
hashWords s = hash s



main :: IO()
main = do
        args <- getArgs
        if length args < 1
           then print "Usage: File"
           else do
                  fileContent <- readFile (args!!0)
                  let countMap = HM.empty
                  insertion (processFile fileContent) countMap
                  print (processFile fileContent)
