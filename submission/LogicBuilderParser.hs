{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LogicBuilderParser where
import           AdditionalParser    (betweenSpaces1, bracket, chain, token,
                                      token1)
import           Control.Applicative (liftA3)
import           Data.Builder        (Builder)
import           Data.Functor        (($>))
import           LogicHelper         (andBuilder, falseChurchEncoding,
                                      ifBuilder, notBuilder, orBuilder,
                                      trueChurchEncoding)
import           Parser              (Parser, string, (|||))
import           Prelude             hiding (fail)

logicalPrecedence :: [Parser (Builder -> Builder -> Builder)]
logicalPrecedence = [andToken, orToken]

ifToken :: Parser Builder
ifToken = token (string "if") *> token logicalExpression

thenToken :: Parser Builder
thenToken = token (string "then") *> token logicalExpression

elseToken :: Parser Builder
elseToken = token (string "else") *> token logicalExpression


logicalTrue :: Parser Builder
logicalTrue = string "True" $> trueChurchEncoding

logicalFalse :: Parser Builder
logicalFalse = string "False" $> falseChurchEncoding

logical :: Parser Builder
logical = logicalTrue ||| logicalFalse

logicalOperator :: Parser Builder
logicalOperator = logicalIf ||| logicalNot


logicalTerm :: Parser Builder
logicalTerm = logical ||| logicalOperator ||| bracket logicalExpression

logicalIf :: Parser Builder
logicalIf = liftA3 ifBuilder ifToken thenToken elseToken

logicalNot :: Parser Builder
logicalNot = token1 (string "not") *> (notBuilder <$> logicalTerm)


andToken :: Parser (Builder -> Builder -> Builder)
andToken = betweenSpaces1 (string "and") $> andBuilder

orToken :: Parser (Builder -> Builder -> Builder)
orToken = betweenSpaces1 (string "or") $> orBuilder


notToken :: Parser (Builder -> Builder)
notToken = betweenSpaces1 (string "not") $> notBuilder

logicalExpression :: Parser Builder
logicalExpression = foldl chain logicalTerm logicalPrecedence
