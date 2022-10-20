{-# OPTIONS_GHC -Wno-typed-holes #-}
module ArithmeticParser where

import           AdditionalParser (betweenSpaces, bracket, chain, int)
import           ArithmeticHelper (addBuilder, expBuilder, minusBuilder,
                                   multBuilder)
import           Data.Builder     (Builder, intToLam)
import           Data.Functor     (($>))
import           Parser           (Parser, is, string, (|||))

basicArithmeticPrecedence :: [Parser (Builder -> Builder -> Builder)]
basicArithmeticPrecedence = [addLambda ||| minusLambda]

arithmeticPrecedence :: [Parser (Builder -> Builder -> Builder)]
arithmeticPrecedence = [expLambda, multLambda] ++ basicArithmeticPrecedence

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
basicArithmeticExpression = foldl chain arithmeticTerm basicArithmeticPrecedence

arithmeticExpression :: Parser Builder
arithmeticExpression = foldl chain arithmeticTerm arithmeticPrecedence
