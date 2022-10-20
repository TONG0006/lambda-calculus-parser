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

arithmeticTerm :: Parser Builder
arithmeticTerm = intLambda ||| bracket arithmeticExpression

basicArithmeticExpression :: Parser Builder
basicArithmeticExpression = chain arithmeticTerm (addLambda ||| minusLambda)

arithmeticExpression :: Parser Builder
arithmeticExpression = chain (chain (chain arithmeticTerm expLambda) multLambda) (addLambda ||| minusLambda)

-- expr :: Parser Builder
-- expr = chain term (add ||| minus)
