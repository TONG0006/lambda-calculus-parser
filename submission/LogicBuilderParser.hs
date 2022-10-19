{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LogicBuilderParser where
import           AdditionalParser
import           Control.Applicative
import           Data.Builder
import           Data.Functor
import           LogicHelper
import           Parser
import           Prelude             hiding (fail)

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
logicalIf = liftA3 ifBuilder (token (string "if") *> token logicalTerm) (token (string "then") *> token logicalTerm) (token (string "else") *> token logicalTerm)

logicalNot :: Parser Builder
logicalNot = token1 (string "not") *> (notBuilder <$> logicalTerm)


andToken :: Parser (Builder -> Builder -> Builder)
andToken = betweenSpace (string "and") $> andBuilder

orToken :: Parser (Builder -> Builder -> Builder)
orToken = betweenSpace (string "or") $> orBuilder


notToken :: Parser (Builder -> Builder)
notToken = betweenSpace (string "not") $> notBuilder

logicalExpression :: Parser Builder
logicalExpression = chain (chain logicalTerm andToken) orToken
