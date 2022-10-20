{-# OPTIONS_GHC -Wno-typed-holes #-}

module ComparatorParser where
import           AdditionalParser (betweenSpaces)
import           ComparatorHelper (leqBuilder)
import           Data.Builder     (Builder)
import           Data.Functor     (($>))
import           Parser           (Parser, is, string)
import           Prelude          hiding (fail)

-- basicArithmeticPrecedence :: [Parser (Builder -> Builder -> Builder)]
-- basicArithmeticPrecedence = [addLambda ||| minusLambda]

-- arithmeticPrecedence :: [Parser (Builder -> Builder -> Builder)]
-- arithmeticPrecedence = [expLambda, multLambda] ++ basicArithmeticPrecedence

-- intLambda :: Parser Builder
-- intLambda = intToLam <$> int

-- expLambda :: Parser (Builder -> Builder -> Builder)
-- expLambda = betweenSpaces (string "**") $> expBuilder

-- multLambda :: Parser (Builder -> Builder -> Builder)
-- multLambda = betweenSpaces (is '*') $> multBuilder

-- addLambda :: Parser (Builder -> Builder -> Builder)
-- addLambda = betweenSpaces (is '+') $> addBuilder

-- minusLambda :: Parser (Builder -> Builder -> Builder)
-- minusLambda = betweenSpaces (is '-') $> minusBuilder

-- arithmeticTerm :: Parser Builder
-- arithmeticTerm = intLambda ||| bracket arithmeticExpression

-- basicArithmeticExpression :: Parser Builder
-- basicArithmeticExpression = foldl chain arithmeticTerm basicArithmeticPrecedence

-- arithmeticExpression :: Parser Builder
-- arithmeticExpression = foldl chain arithmeticTerm arithmeticPrecedence

-- comparatorPrecedence = []

leqLambda :: Parser (Builder -> Builder -> Builder)
leqLambda = betweenSpaces (string "<=") $> leqBuilder

geqLambda :: Parser (Builder -> Builder -> Builder)
geqLambda = betweenSpaces (string ">=") $> leqBuilder

ltLambda :: Parser (Builder -> Builder -> Builder)
ltLambda = betweenSpaces (is '<') $> leqBuilder

gtLambda :: Parser (Builder -> Builder -> Builder)
gtLambda = betweenSpaces (is '>') $> leqBuilder

eqLambda :: Parser (Builder -> Builder -> Builder)
eqLambda = betweenSpaces (string "==") $> leqBuilder

neqLambda :: Parser (Builder -> Builder -> Builder)
neqLambda = betweenSpaces (string "!=") $> leqBuilder

-- comparatorExpression = foldl chain
