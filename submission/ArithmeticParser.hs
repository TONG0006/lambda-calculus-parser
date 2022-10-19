{-# OPTIONS_GHC -Wno-typed-holes #-}
module ArithmeticParser where

import           AdditionalParser
import           Data.Builder
import           Parser

intLambda :: Parser Builder
intLambda = intToLam <$> int
