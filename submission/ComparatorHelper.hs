{-# OPTIONS_GHC -Wno-typed-holes #-}

module ComparatorHelper where
import           AdditionalBuilder (ap3)
import           ArithmeticHelper  (minusBuilder, succBuilder)
import           Data.Builder      (Builder, lam)
import           LogicHelper       (andBuilder, falseChurchEncoding, notBuilder,
                                    trueChurchEncoding)
import           Prelude           hiding (fail)

zeroBuilder :: Builder -> Builder
zeroBuilder n = ap3 n (lam 'x' falseChurchEncoding) trueChurchEncoding

leqBuilder :: Builder -> Builder -> Builder
leqBuilder m n = zeroBuilder $ minusBuilder m n

geqBuilder :: Builder -> Builder -> Builder
geqBuilder = flip leqBuilder

ltBuilder :: Builder -> Builder -> Builder
ltBuilder m = leqBuilder (succBuilder m)

gtBuilder :: Builder -> Builder -> Builder
gtBuilder m n = geqBuilder m $ succBuilder n

eqBuilder :: Builder -> Builder -> Builder
eqBuilder m n = andBuilder (leqBuilder m n) (leqBuilder n m)

neqBuilder :: Builder -> Builder -> Builder
neqBuilder m n = notBuilder $ eqBuilder m n
