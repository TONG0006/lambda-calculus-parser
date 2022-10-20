{-# OPTIONS_GHC -Wno-typed-holes #-}

module ComparatorParser where
import           AdditionalParser   (binaryToken, bracket, chain)
import           ArithmeticParser   (arithmeticExpression)
import           ComparatorHelper   (eqBuilder, geqBuilder, gtBuilder,
                                     leqBuilder, ltBuilder, neqBuilder)
import           Data.Builder       (Builder)
import           LogicBuilderParser (logicalPrecedence, logicalTerm)
import           Parser             (Parser, (|||))
import           Prelude            hiding (fail)

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

comparatorPrecedence :: [Parser (Builder -> Builder -> Builder)]
comparatorPrecedence = [leqLambda ||| geqLambda ||| ltLambda ||| gtLambda ||| eqLambda ||| neqLambda]

comparatorTerm :: Parser Builder
comparatorTerm = arithmeticExpression ||| bracket comparatorExpression

comparatorExpression :: Parser Builder
comparatorExpression = foldl chain (comparatorTerm ||| logicalTerm) (comparatorPrecedence ++ logicalPrecedence)
