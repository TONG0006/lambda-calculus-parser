module FunctionHelper where
import           AdditionalBuilder
import           ArithmeticHelper
import           Combinators
import           ComparatorHelper
import           Data.Builder
import           ListHelper
import           LogicHelper

-- Number

divBuilder :: Builder
divBuilder = yCombinator (lamBuild "gqab" $ ltBuilder (term 'a') (term 'b') `ap` pairBuilder (term 'q') (term 'a') `ap` (term 'g' `ap` succBuilder (term 'q') `ap` minusBuilder (term 'a') (term 'b') `ap` term 'b')) `ap` intToLam 0

intDivBuilder :: Builder -> Builder -> Builder
intDivBuilder x y = carBuilder $ divBuilder `ap` x `ap` y

moduloBuilder :: Builder -> Builder -> Builder
moduloBuilder x y = cdrBuilder $ divBuilder `ap` x `ap` y

gcdBuilder :: Builder
gcdBuilder = lamBuild "gmn" (leqBuilder (term 'm') (term 'n') `ap` ap3 (term 'g') (term 'n') (term 'm') `ap` ap3 (term 'g') (term 'm') (term 'n')) `ap` yCombinator (lamBuild "gxy" $ isZeroBuilder (term 'y') `ap` term 'x' `ap` (term 'g' `ap` term 'y' `ap` moduloBuilder (term 'x') (term 'y')))

factorialBuilder :: Builder -> Builder
factorialBuilder =  ap (yCombinator (lamBuild "gx" $ ap4 isZeroChurchEncoding (term 'x') (intToLam 1) $ multBuilder (term 'x') (term 'g' `ap` predBuilder (term 'x'))))

fibonacciBuilder :: Builder -> Builder
fibonacciBuilder n = ap5 n (lam 'f' $ lam 'a' $ lam 'b' $ ap3 (term 'f') (term 'b') $ addBuilder (term 'a') (term 'b')) kChurchEncoding (intToLam 0) $ intToLam 1

-- Pairs / signed integer

pairBuilder :: Builder -> Builder -> Builder
pairBuilder x y = lam 'f' $ ap3 (term 'f') x y

pairChurchEncoding :: Builder
pairChurchEncoding = lamBuild "xyf" $ ap3 (term 'f') (term 'x') (term 'y')

carBuilder :: Builder -> Builder
carBuilder = flip ap trueChurchEncoding

cdrBuilder :: Builder -> Builder
cdrBuilder = flip ap falseChurchEncoding

oneZeroBuilder :: Builder
oneZeroBuilder = yCombinator $ lamBuild "cx" $ isZeroBuilder (carBuilder $ term 'x') `ap` term 'x' `ap` (isZeroBuilder (cdrBuilder $ term 'x') `ap` term 'x' `ap` (term 'c' `ap` pairChurchEncoding `ap` predBuilder (carBuilder $ term 'x')) `ap` predBuilder (cdrBuilder $ term 'x'))

plusSignBuilder :: Builder
plusSignBuilder = lamBuild "xy" $ oneZeroBuilder `ap` pairBuilder (plusSignBuilder `ap` carBuilder (term 'x') `ap` carBuilder (term 'y')) (plusSignBuilder `ap` cdrBuilder (term 'x') `ap` cdrBuilder (term 'y'))

convertBuilder :: Builder -> Builder
convertBuilder x = pairBuilder x (intToLam 0)

negateBuilder :: Builder -> Builder
negateBuilder x = pairBuilder (cdrBuilder x) (carBuilder x)


-- List functions

appendBuilder :: Builder
appendBuilder = yCombinator $ lamBuild "gab" $ isNullBuilder (term 'a') `ap` term 'b' `ap` pairBuilder (carBuilder $ term 'a') (term 'g' `ap` cdrBuilder (term 'x'))

lenBuilder :: Builder
lenBuilder = yCombinator (lamBuild "gcx" (isNullBuilder (term 'x') `ap` term 'c' `ap` (term 'g' `ap` succBuilder (term 'c') `ap` cdrBuilder (term 'x')))) `ap` intToLam 0

indexBuilder :: Builder -> Builder -> Builder
indexBuilder x i = carBuilder $ i `ap` cdrBuilder x

lastBuilder :: Builder
lastBuilder = yCombinator $ lamBuild "gx" $ isNullBuilder (term 'x') `ap` (nullBuilder `ap` (isNullBuilder (cdrBuilder $ term 'x') `ap` carBuilder (term 'x') `ap` (term 'g' `ap` cdrBuilder (term 'x'))))

truncateBuilder :: Builder
truncateBuilder = yChurchEncoding `ap` lamBuild "gx" (isNullBuilder (term 'x') `ap` nullBuilder `ap` (isNullBuilder (cdrBuilder $ term 'x') `ap` nullBuilder `ap` pairBuilder (carBuilder $ term 'x') (term 'g' `ap` cdrBuilder (term 'x'))))

reverseBuilder :: Builder -> Builder
reverseBuilder = ap (yCombinator (lamBuild "gal" $ isNullBuilder (term 'l') `ap` term 'a' `ap` (term 'g' `ap` pairBuilder (carBuilder $ term 'l') (term 'a') `ap` cdrBuilder (term 'l'))) `ap` nullBuilder)

rangeBuilder :: Builder -> Builder -> Builder
rangeBuilder s e = yCombinator (lamBuild "gc" $ leqBuilder (term 'c') e `ap` pairBuilder (term 'c') (term 'g' `ap` succBuilder (term 'c') `ap` e) `ap` nullBuilder) `ap` s

listBuilder :: Builder
listBuilder = lam 'n' $ term 'n' `ap` lamBuild "gfx" (term 'f' `ap` pairBuilder (term 'x') (term 'a')) `ap` reverseBuilder nullBuilder

applyBuilder :: Builder
applyBuilder = yCombinator $ lamBuild "gfx" $ isNullBuilder (term 'x') `ap` term 'f' `ap` (term 'g' `ap` (term 'f' `ap` carBuilder (term 'x')) `ap` cdrBuilder (term 'x'))

mapBuilder :: Builder
mapBuilder = yCombinator $ lamBuild "gfx" $ ap4 isNullChurchEncoding (term 'x') nullBuilder (pairBuilder (term 'f' `ap` carBuilder (term 'x')) (ap3 (term 'g') (term 'f') (cdrBuilder $ term 'x')))

filterBuilder :: Builder
filterBuilder = yCombinator (lamBuild "gfx" $ isNullBuilder (term 'x') `ap` nullBuilder `ap` (term 'f' `ap` carBuilder (term 'x') `ap` (pairChurchEncoding `ap` carBuilder (term 'x')) `ap` iChurchEncoding `ap` (term 'g' `ap` term 'f' `ap` cdrBuilder (term 'x'))))

crossBuilder :: Builder -> Builder -> Builder -> Builder
crossBuilder f l m = foldlBuilder `ap` appendBuilder `ap` nullBuilder `ap` (mapBuilder `ap` lam 'x' (mapBuilder `ap` (f `ap` term 'x') `ap` m) `ap` l)

foldlBuilder :: Builder
foldlBuilder = yCombinator $ lamBuild "gfex" $ isNullBuilder (term 'x') `ap` term 'e' `ap` (term 'g' `ap` term 'f' `ap` (term 'f' `ap` term 'e' `ap` carBuilder (term 'x')) `ap` cdrBuilder (term 'x'))

foldrBuilder :: Builder -> Builder -> Builder -> Builder
foldrBuilder f e x = yCombinator (lamBuild "gy" $ isNullBuilder (term 'y') `ap` e `ap` (f `ap` carBuilder (term 'y') `ap` (term 'g' `ap` cdrBuilder (term 'y')))) `ap` x
