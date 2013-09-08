module Structure.BKTree
(
  BKTree
, empty
, bktree
, isEmpty
, null
, size
, (.:.)
, member
, withinDistance
, fromWords
, fromDictionaryFile
) where

import Structure.MetricSpace
import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (any, foldr, foldl)
import Data.Foldable
import Data.Monoid

data BKTree a = Node a !Int (BMap a)
              | Leaf
  deriving (Eq, Show)

empty :: BKTree a
empty = Leaf


instance MetricSpace a => Monoid (BKTree a) where
  mempty = empty
  mappend a b = foldl' (flip (.:.)) a (asList b)
  mconcat = foldl' mappend empty

instance Foldable BKTree where
  foldl f z = foldl' f z . asList
  foldr f z = foldr f z . asList

bktree :: (MetricSpace a, Foldable f) => f a -> BKTree a
bktree = foldl (flip (.:.)) empty


isEmpty :: BKTree a -> Bool
isEmpty Leaf = True
isEmpty _ = False


size :: BKTree a -> Int
size Leaf = 0
size (Node _ s _) = s


(.:.) :: MetricSpace a => a -> BKTree a -> BKTree a
el .:. Leaf = Node el 1 M.empty
el .:. Node z s m = Node z (s+1) (M.alter (\zz -> Just $! case zz of
                                                            Just w -> el .:. w
                                                            Nothing -> Node el 1 M.empty)
                                          (el <--> z)
                                          m)


infixr 5 .:.

member :: MetricSpace a => a -> BKTree a -> Bool
member _ Leaf = False
member el (Node z _ m) = d == 0 || any (member el) (M.lookup d m)
  where
    d = el <--> z


withinDistance :: MetricSpace a => Int -> a -> BKTree a -> [(Int, a)]
withinDistance = undefined


fromWords :: String -> BKTree String
fromWords = bktree . words


fromDictionaryFile :: FilePath -> IO (BKTree String)
fromDictionaryFile = fmap fromWords . readFile


-- not exported

type BMap a = Map Int (BKTree a)

asList :: BKTree a -> [a]
asList Leaf = []
asList (Node a _ m) = a : (fmap snd (M.toList m) >>= asList)

usingMap :: (BMap a -> x) -> x -> BKTree a -> x
usingMap _ l Leaf = l
usingMap f _ (Node _ _ m) = f m

breakMap :: Int -> Int -> BMap a -> BMap a
breakMap d n m = fst $ M.split (d + n + 1) (snd $ M.split (d - n - 1) m)
