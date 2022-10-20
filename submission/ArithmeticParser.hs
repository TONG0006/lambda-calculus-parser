{-# OPTIONS_GHC -Wno-typed-holes #-}
module ArithmeticParser where

import           AdditionalParser (binaryToken, bracket, chain, int)
import           ArithmeticHelper (addBuilder, expBuilder, minusBuilder,
                                   multBuilder)
import           Data.Builder     (Builder, intToLam)
import           Parser           (Parser, (|||))

basicArithmeticPrecedence :: [Parser (Builder -> Builder -> Builder)]
basicArithmeticPrecedence = [addLambda ||| minusLambda]

arithmeticPrecedence :: [Parser (Builder -> Builder -> Builder)]
arithmeticPrecedence = [expLambda, multLambda] ++ basicArithmeticPrecedence

intLambda :: Parser Builder
intLambda = intToLam <$> int

expLambda :: Parser (Builder -> Builder -> Builder)
expLambda = binaryToken "**" expBuilder

multLambda :: Parser (Builder -> Builder -> Builder)
multLambda = binaryToken "*" multBuilder

addLambda :: Parser (Builder -> Builder -> Builder)
addLambda = binaryToken "+" addBuilder

minusLambda :: Parser (Builder -> Builder -> Builder)
minusLambda = binaryToken "-" minusBuilder

arithmeticTerm :: Parser Builder
arithmeticTerm = intLambda ||| bracket arithmeticExpression

basicArithmeticExpression :: Parser Builder
basicArithmeticExpression = foldl chain arithmeticTerm basicArithmeticPrecedence

arithmeticExpression :: Parser Builder
arithmeticExpression = foldl chain arithmeticTerm arithmeticPrecedence
