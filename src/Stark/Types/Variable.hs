module Stark.Types.Variable ( Variable (Variable, unVariable) ) where

import           Data.Kind (Type)
type Variable :: Type
newtype Variable = Variable { unVariable :: Int }
