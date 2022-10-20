{-# OPTIONS_GHC -Wno-typed-holes #-}

module ComparatorParser where
import           AdditionalParser (binaryToken)
import           ComparatorHelper (eqBuilder, geqBuilder, gtBuilder, leqBuilder,
                                   ltBuilder, neqBuilder)
import           Data.Builder     (Builder)
import           Parser           (Parser)
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
leqLambda = binaryToken "<=" leqBuilder

geqLambda :: Parser (Builder -> Builder -> Builder)
geqLambda = binaryToken ">=" geqBuilder

ltLambda :: Parser (Builder -> Builder -> Builder)
ltLambda = binaryToken "<" ltBuilder

gtLambda :: Parser (Builder -> Builder -> Builder)
gtLambda = binaryToken ">" gtBuilder

eqLambda :: Parser (Builder -> Builder -> Builder)
eqLambda = binaryToken "==" eqBuilder

neqLambda :: Parser (Builder -> Builder -> Builder)
neqLambda = binaryToken "!=" neqBuilder

-- comparatorExpression = foldl chain
