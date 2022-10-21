{-# OPTIONS_GHC -Wno-typed-holes #-}

module ListParser where
import           AdditionalParser   (array, chain, unaryToken1)
import           ArithmeticParser   (intLambda)
import           Data.Builder       (Builder)
import           ListHelper         (consBuilder, headBuilder, isNullBuilder,
                                     nullBuilder, tailBuilder)
import           LogicBuilderParser (logicalTerm)
import           Parser             (Parser, (|||))
import           Prelude            hiding (fail)

-- logicalTrue :: Parser Builder
-- logicalTrue = constToken "True" trueChurchEncoding

-- logicalFalse :: Parser Builder
-- logicalFalse = constToken "False" falseChurchEncoding

-- logical :: Parser Builder
-- logical = logicalTrue ||| logicalFalse

-- logicalIf :: Parser Builder
-- logicalIf = ternaryToken1 ifBuilder
--     ("if", token1 logicalExpression)
--     ("then", token1 logicalExpression)
--     ("else", logicalExpression)

-- logicalNot :: Parser Builder
-- logicalNot = unaryToken1 "not" $ notBuilder <$> logicalTerm

-- logicalOperator :: Parser Builder
-- logicalOperator = logicalIf ||| logicalNot

-- logicalTerm :: Parser Builder
-- logicalTerm = logical ||| logicalOperator ||| bracket logicalExpression

-- andToken :: Parser (Builder -> Builder -> Builder)
-- andToken = binaryToken1 "and" andBuilder

-- orToken :: Parser (Builder -> Builder -> Builder)
-- orToken = binaryToken1 "or" orBuilder

-- logicalPrecedence :: [Parser (Builder -> Builder -> Builder)]
-- logicalPrecedence = [andToken, orToken]

-- logicalExpression :: Parser Builder
-- logicalExpression = foldl chain logicalTerm logicalPrecedence

consToken :: Parser Builder
consToken = foldr consBuilder nullBuilder <$> array datatypeTerm

headToken :: Parser Builder
headToken = unaryToken1 "head" $ headBuilder <$> listToken

tailToken :: Parser Builder
tailToken = unaryToken1 "rest" $ tailBuilder <$> listToken

isNullToken :: Parser Builder
isNullToken = unaryToken1 "isNull" $ isNullBuilder <$> listToken

listToken :: Parser Builder
listToken = tailToken ||| consToken

listOperator :: Parser Builder
listOperator = listToken ||| headToken ||| isNullToken

listPrecedence :: [Parser (Builder -> Builder -> Builder)]
listPrecedence = []

datatypeTerm :: Parser Builder
datatypeTerm = intLambda ||| logicalTerm

listExpression :: Parser Builder
listExpression = foldl chain listOperator listPrecedence
