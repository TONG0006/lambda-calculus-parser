module FunctionParser where
import           AdditionalBuilder
import           AdditionalParser
import           ArithmeticParser
import           Data.Builder
import           FunctionHelper
import           ListParser
import           LogicBuilderParser
import           Parser
import           Prelude


-- $setup
-- >>> import AdditionalBuilder (normalBuild)
-- >>> import Data.Builder (build)
-- >>> import Data.Lambda (lamToInt, lamToBool, normal)
-- >>> import Parser (parse)
-- >>> import ArithmeticParser (intLambda)
-- >>> import AdditionalParser (chain)
-- >>> import ListHelper (isNullBuilder, headBuilder, tailBuilder)

-- | Fibonacci sequence, the good stuff
-- >>> lamToInt <$> normalBuild fibonacci "fib 1"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild fibonacci "fib 2"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild fibonacci "fib 3"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild fibonacci "fib 4"
-- Result >< Just 3
-- >>> lamToInt <$> normalBuild fibonacci "fib 5"
-- Result >< Just 5
-- >>> lamToInt <$> normalBuild fibonacci "fib 6"
-- Result >< Just 8
-- >>> lamToInt <$> normalBuild fibonacci "fib 7"
-- Result >< Just 13
-- >>> lamToInt <$> normalBuild fibonacci "fib 8"
-- Result >< Just 21
-- >>> lamToInt <$> normalBuild fibonacci "fib 9"
-- Result >< Just 34
-- >>> lamToInt <$> normalBuild fibonacci "fib 10"
-- Result >< Just 55
-- >>> lamToInt <$> normalBuild fibonacci "fib 5+6"
-- Result >< Just 89
-- >>> lamToInt <$> normalBuild fibonacci "fib (2**4-3**2-4)*2**2"
-- Result >< Just 144
fibonacci :: Parser Builder
fibonacci = unaryToken1 "fib" $ fibonacciBuilder <$> arithmeticExpression

-- | Integer division
-- >>> lamToInt <$> normalBuild (chain intLambda intDiv) "4//2"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild (chain intLambda intDiv) "6//2"
-- Result >< Just 3
-- >>> lamToInt <$> normalBuild (chain intLambda intDiv) "9//3"
-- Result >< Just 3
-- >>> lamToInt <$> normalBuild (chain intLambda intDiv) "7//2"
-- Result >< Just 3
-- >>> lamToInt <$> normalBuild (chain intLambda intDiv) "5//2"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild (chain intLambda intDiv) "5 // 2"
-- Result >< Just 2
intDiv :: Parser (Builder -> Builder -> Builder)
intDiv = binaryToken "//" intDivBuilder

-- | Modulo
-- >>> lamToInt <$> normalBuild (chain intLambda modulo) "4%2"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda modulo) "6%2"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda modulo) "9%3"
-- Result >< Just 0
-- >>> lamToInt <$> normalBuild (chain intLambda modulo) "7%2"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda modulo) "5%2"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda modulo) "5 % 2"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda modulo) "9 % 5"
-- Result >< Just 4
modulo :: Parser (Builder -> Builder -> Builder)
modulo = binaryToken "%" moduloBuilder

-- | Greatest Common Denominator (GCD)
-- >>> lamToInt <$> normalBuild (chain intLambda gcdLambda) "2 gcd 1"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild (chain intLambda gcdLambda) "4 gcd 2"
-- Result >< Just 2
-- >>> lamToInt <$> normalBuild (chain intLambda gcdLambda) "4 gcd 8"
-- Result >< Just 4
-- >>> lamToInt <$> normalBuild (chain intLambda gcdLambda) "12 gcd 3"
-- Result >< Just 3
-- >>> lamToInt <$> normalBuild (chain intLambda gcdLambda) "21 gcd 7"
-- Result >< Just 7
-- >>> lamToInt <$> normalBuild (chain intLambda gcdLambda) "23 gcd 7"
-- Result >< Just 1
gcdLambda :: Parser (Builder -> Builder -> Builder)
gcdLambda = binaryToken1 "gcd" $ ap3 gcdBuilder

-- | Appends two lists together
-- >>> lamToBool . build . isNullBuilder <$> parse (chain listToken append) "[]++[]"
-- Result >< Just True
-- >>> lamToInt . build .  headBuilder <$> parse (chain listToken append) "[]++[1]"
-- Result >< Just 1
-- >>> lamToInt . build .  headBuilder <$> parse (chain listToken append) "[]++[2]"
-- Result >< Just 2
-- >>> lamToInt . build .  headBuilder <$> parse (chain listToken append) "[3]++[1]"
-- Result >< Just 3
-- >>> lamToInt . build . headBuilder . tailBuilder . tailBuilder <$> parse (chain listToken append) "[3,7]++[2,1]"
-- Result >< Just 7
append :: Parser (Builder -> Builder -> Builder)
append = binaryToken "++" $ ap3 appendBuilder

len :: Parser Builder
len = unaryToken1 "len" $ ap lenBuilder <$> listToken

index :: Parser (Builder -> Builder -> Builder)
index = binaryToken "->" indexBuilder

lastToken :: Parser Builder
lastToken = unaryToken1 "last" $ ap lastBuilder <$> listToken

truncateToken :: Parser Builder
truncateToken = unaryToken1 "truncate" $ ap truncateBuilder <$> listToken

reverseToken :: Parser Builder
reverseToken = unaryToken1 "reverse" $ ap truncateBuilder <$> listToken

range :: Parser (Builder -> Builder -> Builder)
range = binaryToken1 "range" rangeBuilder

apply :: Parser (Builder -> Builder -> Builder)
apply = binaryToken1 "apply" $ ap3 applyBuilder

mapToken :: Parser (Builder -> Builder -> Builder)
mapToken = binaryToken1 "map" $ ap3 mapBuilder

filterToken :: Parser (Builder -> Builder -> Builder)
filterToken = binaryToken1 "filter" $ ap3 filterBuilder

foldlToken :: Parser Builder
foldlToken = ternaryToken1 (ap4 foldlBuilder) ("foldl", token1 functionExpression) ("=>", token1 functionExpression) ("->", listExpression)

foldrToken :: Parser Builder
foldrToken = ternaryToken1 foldrBuilder ("foldr", token1 functionExpression) ("=>", token1 functionExpression) ("->", listExpression)

factorial :: Parser Builder
factorial = unaryToken1 "factorial" $ factorialBuilder <$> arithmeticExpression

convert :: Parser Builder
convert = unaryToken1 "convert" $ convertBuilder <$> arithmeticExpression

functionOperator :: Parser Builder
functionOperator = fibonacci ||| len ||| lastToken ||| truncateToken ||| reverseToken ||| foldlToken ||| foldrToken ||| factorial ||| convert

functionTerm :: Parser Builder
functionTerm = (listToken ||| intLambda ||| logical) ||| functionOperator ||| bracket functionExpression

functionPrecedence :: [Parser (Builder -> Builder -> Builder)]
functionPrecedence = [intDiv, modulo, gcdLambda, append, index, mapToken, range, apply, mapToken, filterToken]

functionExpression :: Parser Builder
functionExpression = foldl chain functionTerm functionPrecedence
