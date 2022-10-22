module LambdaParser where
import           AdditionalParser    (token)
import           ArithmeticParser    (arithmeticExpression,
                                      basicArithmeticExpression)
import           ComparatorParser    (comparatorExpression)
import           Data.Builder        (build)
import           Data.Lambda         (Lambda)
import           LambdaBuilderParser (longLambdaExpression,
                                      shortLambdaExpression)
import           ListParser          (listExpression, listToken)
import           LogicBuilderParser  (logicalExpression)
import           Parser              (Parser, (|||))

-- $setup
-- >>> import Parser (parse)
-- >>> import Data.Lambda (lamToBool, lamToInt)

-- | Parses a string representing a lambda calculus expression in long form
-- >>> parse longLambdaP "(λx.x)"
-- Result >< \x.x
-- >>> parse longLambdaP "(λy.y)"
-- Result >< \y.y
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
-- >>> parse longLambdaP "(λx.(λy.xy))"
-- Result >< \xy.xy
-- >>> parse longLambdaP "(λx.(λy.(λz.xyz)))"
-- Result >< \xyz.xyz
-- >>> parse longLambdaP "(λx.(λy.(λz.xxx)))"
-- Result >< \xyz.xxx
-- >>> parse longLambdaP "(λx.x)(λy.y)"
-- Result >< (\x.x)\y.y
-- >>> parse longLambdaP "(λx.x)(λy.y)(λz.z)"
-- Result >< (\x.x)(\y.y)\z.z
-- >>> parse longLambdaP "(λx.x(xx))"
-- Result >< \x.x(xx)
-- >>> parse longLambdaP "(λx.x(xx)x)"
-- Result >< \x.x(xx)x
-- >>> parse longLambdaP "(λx.(λy.xy)(λz.xz))"
-- Result >< \x.(\y.xy)\z.xz
-- >>> parse longLambdaP "(λ x.x)"
-- Result >< \x.x
-- >>> parse longLambdaP "(λx .x)"
-- Result >< \x.x
-- >>> parse longLambdaP "(λx. x)"
-- Result >< \x.x
-- >>> parse longLambdaP "(λx.x )"
-- Result >< \x.x
-- >>> parse longLambdaP "(λ  x  .  x  )"
-- Result >< \x.x
longLambdaP :: Parser Lambda
longLambdaP = build <$> token longLambdaExpression

-- | Parses a string representing a lambda calculus expression in short form
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
shortLambdaP :: Parser Lambda
shortLambdaP = build <$> token shortLambdaExpression

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
-- >>> parse lambdaP "x"
-- Result >< *** Exception: The expression `_0` is malformed:
--   Error: The expression contains a free variable `x`
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
-- >>> parse logicP "True and"
-- Result >and< \xy.x
-- >>> parse logicP "not False"
-- Result >< (\xy.y)(\xy.y)\xy.x
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True
-- >>> lamToBool <$> parse logicP "if False then True else True and if True then False else False"
-- Result >< Just False
logicP :: Parser Lambda
logicP = build <$> token logicalExpression

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> token basicArithmeticExpression

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
arithmeticP :: Parser Lambda
arithmeticP = build <$> token arithmeticExpression

-- | Parses comparator and boolean expressions
-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
complexCalcP :: Parser Lambda
complexCalcP = build <$> token comparatorExpression


-- | Parses a list
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< \cn.c(\xy.x)((\ab.b)cn)
--
-- >>> parse listP "[0, 0]"
-- Result >< \cn.c(\fx.x)((\ab.a(\fx.x)((\de.e)ab))cn)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = build <$> token listToken

-- | Parses a list along with its operators
-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
listOpP :: Parser Lambda
listOpP = build <$> token listExpression

