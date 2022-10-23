module FunctionParser where
import           AdditionalBuilder  (ap3, ap4)
import           AdditionalParser   (binaryToken, binaryToken1, bracket, chain,
                                     ternaryToken1, token1, unaryToken1)
import           ArithmeticParser   (arithmeticExpression, intLambda)
import           Data.Builder       (Builder, ap)
import           FunctionHelper     (appendBuilder, applyBuilder,
                                     convertBuilder, factorialBuilder,
                                     fibonacciBuilder, filterBuilder,
                                     foldlBuilder, foldrBuilder, gcdBuilder,
                                     indexBuilder, intDivBuilder, lastBuilder,
                                     lenBuilder, mapBuilder, moduloBuilder,
                                     rangeBuilder, truncateBuilder)
import           ListParser         (listExpression, listPrecedence, listToken)
import           LogicBuilderParser (logical)
import           Parser             (Parser, (|||))

-- | Fibonacci sequence, the good stuff
fibonacci :: Parser Builder
fibonacci = unaryToken1 "fib" $ fibonacciBuilder <$> arithmeticExpression

-- | Integer division
intDiv :: Parser (Builder -> Builder -> Builder)
intDiv = binaryToken "//" intDivBuilder

-- | Modulo
modulo :: Parser (Builder -> Builder -> Builder)
modulo = binaryToken "%" moduloBuilder

-- | Greatest Common Denominator (GCD)
gcdLambda :: Parser (Builder -> Builder -> Builder)
gcdLambda = binaryToken1 "gcd" $ ap3 gcdBuilder

-- | Appends two lists together
append :: Parser (Builder -> Builder -> Builder)
append = binaryToken "++" $ ap3 appendBuilder

-- | Gets the length of a list
len :: Parser Builder
len = unaryToken1 "len" $ ap lenBuilder <$> listToken

-- | Gets the item on an index of a list
index :: Parser (Builder -> Builder -> Builder)
index = binaryToken "->" indexBuilder

-- | Gets the last item in the list
lastToken :: Parser Builder
lastToken = unaryToken1 "last" $ ap lastBuilder <$> listToken

-- | Gets everything except the last item
truncateToken :: Parser Builder
truncateToken = unaryToken1 "truncate" $ ap truncateBuilder <$> listToken

-- | Reverse the list
reverseToken :: Parser Builder
reverseToken = unaryToken1 "reverse" $ ap truncateBuilder <$> listToken

-- | Generate a list from two numbers
range :: Parser (Builder -> Builder -> Builder)
range = binaryToken1 "range" rangeBuilder

-- | Passes the elemnts of the list x to f
apply :: Parser (Builder -> Builder -> Builder)
apply = binaryToken1 "apply" $ ap3 applyBuilder

-- | Maps each item in the list
mapToken :: Parser (Builder -> Builder -> Builder)
mapToken = binaryToken1 "map" $ ap3 mapBuilder

-- | Filters the list by a boolean function
filterToken :: Parser (Builder -> Builder -> Builder)
filterToken = binaryToken1 "filter" $ ap3 filterBuilder

-- | Folds the list from the left
foldlToken :: Parser Builder
foldlToken = ternaryToken1 (ap4 foldlBuilder) ("foldl", token1 functionExpression) ("=>", token1 functionExpression) ("->", listExpression)

-- | Folds the list from the right
foldrToken :: Parser Builder
foldrToken = ternaryToken1 foldrBuilder ("foldr", token1 functionExpression) ("=>", token1 functionExpression) ("->", listExpression)

-- | The factorial function
factorial :: Parser Builder
factorial = unaryToken1 "factorial" $ factorialBuilder <$> arithmeticExpression

-- | Converts a natural number to a signed integer
convert :: Parser Builder
convert = unaryToken1 "convert" $ convertBuilder <$> arithmeticExpression

-- | All the non-binary operators
functionOperator :: Parser Builder
functionOperator = fibonacci ||| len ||| lastToken ||| truncateToken ||| reverseToken ||| foldlToken ||| foldrToken ||| factorial ||| convert

-- | Possible terms for functions
functionTerm :: Parser Builder
functionTerm = (listToken ||| intLambda ||| logical) ||| functionOperator ||| bracket functionExpression

-- | Precedence of function
functionPrecedence :: [Parser (Builder -> Builder -> Builder)]
functionPrecedence = listPrecedence ++ [intDiv, modulo, gcdLambda, append, index, mapToken, range, apply, mapToken, filterToken]

-- | The final function expression
functionExpression :: Parser Builder
functionExpression = foldl chain functionTerm functionPrecedence
