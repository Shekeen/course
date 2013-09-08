module Algorithm.FastAnagrams where

import Data.Char
import Data.List
import Data.Function
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: String -> FilePath -> IO [String]
fastAnagrams word filename = do
    anagramsInFile <- fmap (map NoCaseString . lines) $ readFile filename
    return $ map ncString $ filter (flip S.member $ S.fromList $ map NoCaseString $ permutations word) anagramsInFile

newtype NoCaseString =
  NoCaseString {
    ncString :: String
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare s1 s2 = compare (map toLower $ ncString s1) (map toLower $ ncString s2)

instance Show NoCaseString where
  show = show . ncString
