module PQueue
  ( PQueue
  , emptyPQ
  , insertPQ
  , popPQ
  , sizePQ
  ) where

import Data.Foldable

-- define a skewheap data type
{-
skew heaps have only three operations: 
1. making new (singleton) one, 
2. merging two skew heaps, 
3. and popping off the root. 
Traditional “insert” is done by making a new skew heap with one element, and merging it with the main heap.
-}
data SkewHeap a = SEmpty
                | SNode a (SkewHeap a) (SkewHeap a)
                deriving (Show, Eq, Foldable)

-- create a new SkewHeap with one item
makeSH :: a -> SkewHeap a
makeSH x = SNode x SEmpty SEmpty

-- pop off the root, returns the root and the new SkewHeap
-- a must belong to the Ord typeclass becasue the data must be comparable to merge
-- if the SkewHeap is empty, return SEmpty
-- oth, pattern match SkewHeap a = SNode r h1 h2, where h1 and h2 = SkewHeap r
popSH :: Ord a => SkewHeap a -> (Maybe a, SkewHeap a)
popSH SEmpty          = (Nothing, SEmpty)
popSH (SNode r h1 h2) = (Just r , mergeSH h1 h2)

-- merge two SkewHeaps
-- a must belong to the Ord typeclass becasue the data must be comparable
{-
1. Merging any skew heap with an empty heap is that same skew heap.
2. When merging two heaps, the new heap is an SNode with the smaller root, 
   whose children are the merge of the smaller tree and the original children.
-}
mergeSH :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeSH SEmpty h = h
mergeSH h SEmpty = h
mergeSH hA@(SNode xA lA rA) hB@(SNode xB lB rB)
    | xA < xB    = SNode xA (mergeSH rA hB) lA      -- root of hA is the new root, don't merge left subtree
    | otherwise  = SNode xB (mergeSH rB hA) lB      -- root of hB is the new root, , don't merge left subtree


-- Define a PQueue interface
-- PQ is a wrapper around SkewHeap
newtype PQueue a = PQ (SkewHeap a) deriving Show

-- return an empty PQueue
emptyPQ :: PQueue a
emptyPQ = PQ SEmpty

-- insert into PQ by creating a new SH with one element and merging it with the original PQ
insertPQ :: Ord a => a -> PQueue a -> PQueue a
insertPQ x (PQ h) = PQ (mergeSH h (makeSH x))

-- call popSH on PQ and return the result
popPQ :: Ord a => PQueue a -> (Maybe a, PQueue a)
popPQ (PQ h) = (res, PQ h')
  where
    (res, h') = popSH h

-- using the toList function from Foldable module
sizePQ :: PQueue a -> Int
sizePQ (PQ h) = length (toList h)