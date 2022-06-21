{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Stark.Types.Index ( Index (Index, unIndex) ) where


import Data.Bits (Bits)


newtype Index = Index { unIndex :: Int }
  deriving (Eq, Ord, Num, Enum, Real, Integral, Bits)
