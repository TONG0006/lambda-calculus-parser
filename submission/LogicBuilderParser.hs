{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LogicBuilderParser where
import           AdditionalParser    (binaryToken1, bracket, chain, constToken,
                                      token1, unaryToken1)
import           Control.Applicative (liftA3)
import           Data.Builder        (Builder)
import           LogicHelper         (andBuilder, falseChurchEncoding,
                                      ifBuilder, notBuilder, orBuilder,
                                      trueChurchEncoding)
import           Parser              (Parser, (|||))
import           Prelude             hiding (fail)

logicalTrue :: Parser Builder
logicalTrue = constToken "True" trueChurchEncoding

logicalFalse :: Parser Builder
logicalFalse = constToken "False" falseChurchEncoding

logical :: Parser Builder
logical = logicalTrue ||| logicalFalse

ifToken :: Parser Builder
ifToken = unaryToken1 "if" $ token1 logicalExpression

thenToken :: Parser Builder
thenToken = unaryToken1 "then" $ token1 logicalExpression

elseToken :: Parser Builder
elseToken = unaryToken1 "else" logicalExpression

logicalIf :: Parser Builder
logicalIf = liftA3 ifBuilder ifToken thenToken elseToken

logicalNot :: Parser Builder
logicalNot = unaryToken1 "not" $ notBuilder <$> logicalTerm

logicalOperator :: Parser Builder
logicalOperator = logicalIf ||| logicalNot

logicalTerm :: Parser Builder
logicalTerm = logical ||| logicalOperator ||| bracket logicalExpression

andToken :: Parser (Builder -> Builder -> Builder)
andToken = binaryToken1 "and" andBuilder

orToken :: Parser (Builder -> Builder -> Builder)
orToken = binaryToken1 "or" orBuilder

logicalPrecedence :: [Parser (Builder -> Builder -> Builder)]
logicalPrecedence = [andToken, orToken]

logicalExpression :: Parser Builder
logicalExpression = foldl chain logicalTerm logicalPrecedence
