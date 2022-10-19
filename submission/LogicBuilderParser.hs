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
logicalTrue = token (string "True") $> trueChurchEncoding

logicalFalse :: Parser Builder
logicalFalse = token (string "False") $> falseChurchEncoding

-- ifToken :: Parser Builder
-- ifToken = liftA3 ifBuilder (logicalTerm <* token (string "if")) logicalTerm logicalTerm

-- andToken :: Parser Builder
-- andToken = liftA2 andBuilder (logicalTerm <* token (string "and")) logicalTerm

-- orToken :: Parser Builder
-- orToken = liftA2 orBuilder (logicalTerm <* token (string "or")) logicalTerm

-- notToken :: Parser Builder
-- notToken = token (string "not") *> (notBuilder <$> logicalTerm)

logicalValue :: Parser Builder
logicalValue = logicalTrue ||| logicalFalse ||| logicalNot ||| bracket andOrToken

-- logicalOperators :: Parser Builder
-- logicalOperators = ifToken ||| notToken ||| andToken ||| orToken

-- logicalTerm :: Parser Builder
-- logicalTerm = bracket logicalOperators ||| logicalValue

-- logicalExpression :: Parser Builder
-- logicalExpression = foldr1 ap <$> list1 (logicalOperators ||| logicalTerm)


logicalNot :: Parser Builder
logicalNot = token (string "not") *> (notBuilder <$> logicalValue)


andToken :: Parser (Builder -> Builder -> Builder)
andToken = betweenSpace (string "and") $> andBuilder

orToken :: Parser (Builder -> Builder -> Builder)
orToken = betweenSpace (string "or") $> orBuilder


notToken :: Parser (Builder -> Builder)
notToken = betweenSpace (string "not") $> notBuilder


-- orToken :: Parser (Builder -> Builder -> Builder)
-- orToken = chain logicalValue $ (spaces >> string "or" >> spaces) >> pure orBuilder

andOrToken :: Parser Builder
andOrToken = chain (chain logicalValue andToken) orToken
