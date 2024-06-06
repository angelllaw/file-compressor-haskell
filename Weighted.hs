module Weighted
  ( Weighted(..)
  ) where

-- Defining a data type Weighted which holds any type a
-- The data type WPair is automatically constructed that contains field accessors of type int and type a
-- can access like this: _wWeight weighted_instance
data Weighted a = WPair { _wWeight :: Int
                        , _wItem   :: a
                        } deriving (Show, Functor)

-- two Weighted are equal if _wWeight is equal
instance Eq (Weighted a) where
    WPair w1 _ == WPair w2 _ = w1 == w2

-- order two Weighted by _wWeight
instance Ord (Weighted a) where
    compare (WPair w1 _) (WPair w2 _) = compare w1 w2