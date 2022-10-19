{-# OPTIONS_GHC -Wno-typed-holes #-}
module ArithmeticParser where

import           AdditionalParser
import           ArithmeticHelper
import           Data.Builder
import           Data.Functor
import           Parser

intLambda :: Parser Builder
intLambda = intToLam <$> int

expLambda :: Parser (Builder -> Builder -> Builder)
expLambda = betweenSpaces (string "**") $> expBuilder

multLambda :: Parser (Builder -> Builder -> Builder)
multLambda = betweenSpaces (is '*') $> multBuilder

addLambda :: Parser (Builder -> Builder -> Builder)
addLambda = betweenSpaces (is '+') $> addBuilder

minusLambda :: Parser (Builder -> Builder -> Builder)
minusLambda = betweenSpaces (is '-') $> minusBuilder

basicArithmeticExpression :: Parser Builder
basicArithmeticExpression = chain intLambda (addLambda ||| minusLambda)

arithmeticExpression :: Parser Builder
arithmeticExpression = chain (chain (chain intLambda expLambda) multLambda) (addLambda ||| minusLambda)

-- expr :: Parser Builder
-- expr = chain term (add ||| minus)
