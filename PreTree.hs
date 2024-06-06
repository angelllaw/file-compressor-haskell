module PreTree where

import GHC.Generics
import Weighted

-- Defining a data type PreTree that holds any type 'a'
-- datatypes PTLeaf and PTNode are automatically constructed
-- Deriving Instances: PreTree is an instance of the Show, Eq, and generic class, which allows for printing, comparison, and generic operations
data PreTree a = PTLeaf a
               | PTNode (PreTree a) (PreTree a)
               deriving (Show, Eq, Generic)

-- insert into an empty tree
-- makePT: a function that takes in a a datatype and output a PreTree containing a datatype
-- since PTLeaf :: a -> PreTree a, this is eta-reduced
makePT :: a -> PreTree a
makePT = PTLeaf

-- merge two PreTrees
-- since PTNode :: PreTree a -> PreTree a -> PreTree a, this is eta-reduced
mergePT :: PreTree a -> PreTree a -> PreTree a
mergePT = PTNode

-- define a type synonym for a weighted data type which hold a PreTree datatype (instead of any type a)
type WeightedPT a = Weighted (PreTree a)

-- insert into an empty weighted PreTree
-- makeWPT takes an Int representing the weight and a value of type a, and it returns a WeightedPT a, where a is a PreTree a
-- WPair w . makePT :: a -> WeightedPT a
-- WPair expects a Int and value of type PreTree a
-- makePT is the second arg of WPair that returns a PreTree a 
makeWPT :: Int -> a -> WeightedPT a
makeWPT w = WPair w . makePT

-- merge two WeightedPT
-- w1 :: Int
-- pt1 :: PreTree a
mergeWPT :: WeightedPT a -> WeightedPT a -> WeightedPT a
mergeWPT (WPair w1 pt1) (WPair w2 pt2)
    = WPair (w1 + w2) (mergePT pt1 pt2)