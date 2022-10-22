module ArithmeticParser where
import           AdditionalParser (binaryToken, bracket, chain, int)
import           ArithmeticHelper (addBuilder, expBuilder, minusBuilder,
                                   multBuilder)
import           Data.Builder     (Builder, intToLam)
import           Parser           (Parser, (|||))

-- $setup
-- >>> import AdditionalBuilder (normalBuild)
-- >>> import Data.Builder (build)
-- >>> import Data.Lambda (lamToInt, normal)
-- >>> import Parser (parse)

-- | A list determining the precedence of basic arithmetic operations
basicArithmeticPrecedence :: [Parser (Builder -> Builder -> Builder)]
basicArithmeticPrecedence = [addLambda ||| minusLambda]

-- | A list determining the precedence of all arithmetic operations
arithmeticPrecedence :: [Parser (Builder -> Builder -> Builder)]
arithmeticPrecedence = [expLambda, multLambda] ++ basicArithmeticPrecedence

-- | Parses an integer and convert it to an equivalent lambda calculus expression
-- >>> lamToInt <$> normalBuild intLambda "0"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild intLambda "1"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild intLambda "2"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild intLambda "12"
-- Result >< Just 12
-- >>> lamToInt <$> normalBuild intLambda "-1"
-- Result >< *** Exception: The expression `_-1` is malformed:
--   Error: The expression contains a negative number `-1`
-- >>> lamToInt <$> normalBuild intLambda "1.2"
-- UnexpectedChar '1'
intLambda :: Parser Builder
intLambda = intToLam <$> int

-- | Parses additive summations in lambda calculus
-- >>> lamToInt <$> normalBuild (chain intLambda addLambda) "0+0"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda addLambda) "0"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda addLambda) "1+2"
-- Result >< Just 3
-- >>> lamToInt <$> normalBuild (chain intLambda addLambda) "1+2+3"
-- Result >< Just 6
-- >>> lamToInt <$> normalBuild (chain intLambda addLambda) "1+"
-- Result >+< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda addLambda) "+2"
-- UnexpectedChar '+'
addLambda :: Parser (Builder -> Builder -> Builder)
addLambda = binaryToken "+" addBuilder

-- | Parses subtractive summations in lambda calculus
-- >>> lamToInt <$> normalBuild (chain intLambda minusLambda) "0-0"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda minusLambda) "0"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda minusLambda) "2-1"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda minusLambda) "5-2-1"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild (chain intLambda minusLambda) "1-"
-- Result >-< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda minusLambda) "-2"
-- Result >< *** Exception: The expression `_-1` is malformed:
--   Error: The expression contains a negative number `-2`
-- >>> lamToInt <$> normalBuild (chain intLambda minusLambda) "1-2"
-- Result >< Just 0
minusLambda :: Parser (Builder -> Builder -> Builder)
minusLambda = binaryToken "-" minusBuilder

-- | Parses products in lambda calculus
-- >>> lamToInt <$> normalBuild (chain intLambda multLambda) "0*0"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda multLambda) "0"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda multLambda) "1*2"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild (chain intLambda multLambda) "2*3*4"
-- Result >< Just 24
-- >>> lamToInt <$> normalBuild (chain intLambda multLambda) "1*"
-- Result >*< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda multLambda) "*2"
-- UnexpectedChar '*'
multLambda :: Parser (Builder -> Builder -> Builder)
multLambda = binaryToken "*" multBuilder

-- | Parses powers in lambda calculus
-- >>> lamToInt <$> normalBuild (chain intLambda expLambda) "1**0"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda expLambda) "0"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda expLambda) "2**2"
-- Result >< Just 4
-- >>> lamToInt <$> normalBuild (chain intLambda expLambda) "2**1**3"
-- Result >< Just 8
-- >>> lamToInt <$> normalBuild (chain intLambda expLambda) "1**"
-- Result >**< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda expLambda) "**2"
-- UnexpectedChar '*'
expLambda :: Parser (Builder -> Builder -> Builder)
expLambda = binaryToken "**" expBuilder

-- | Parses a single term using arithmeticExpression
-- >>> lamToInt <$> normalBuild arithmeticTerm "2"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild arithmeticTerm "(2)"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild arithmeticTerm "(1+1)"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild arithmeticTerm "(1-1)"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild arithmeticTerm "(1*1)"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild arithmeticTerm "(1**1)"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild arithmeticTerm "1+1"
-- Result >+1< Just 1
arithmeticTerm :: Parser Builder
arithmeticTerm = intLambda ||| bracket arithmeticExpression

-- | Parses basic arithmetic expressions (refer to LambdaParser for test cases)
basicArithmeticExpression :: Parser Builder
basicArithmeticExpression = foldl chain arithmeticTerm basicArithmeticPrecedence

-- | Parses all arithmetic expressions (refer to LambdaParser for test cases)
arithmeticExpression :: Parser Builder
arithmeticExpression = foldl chain arithmeticTerm arithmeticPrecedence
