module ComparatorHelper where
import           AdditionalBuilder (ap3)
import           ArithmeticHelper  (minusBuilder, succBuilder)
import           Data.Builder      (Builder, lam)
import           LogicHelper       (andBuilder, falseChurchEncoding, notBuilder,
                                    trueChurchEncoding)

-- Checks if the number is zero (λn.n(λx.false) true)
zeroBuilder :: Builder -> Builder
zeroBuilder n = ap3 n (lam 'x' falseChurchEncoding) trueChurchEncoding

-- Less than or equal to comparator (λmn.IsZero (minus m n))
leqBuilder :: Builder -> Builder -> Builder
leqBuilder m n = zeroBuilder $ minusBuilder m n

-- Less than or equal to comparator (λmn.LEQ n m)
geqBuilder :: Builder -> Builder -> Builder
geqBuilder = flip leqBuilder

-- Less than comparator (λmn.LEQ (succ m) n)
ltBuilder :: Builder -> Builder -> Builder
ltBuilder m = leqBuilder (succBuilder m)

-- Greater than comparator (λmn.LT n m)
gtBuilder :: Builder -> Builder -> Builder
gtBuilder = flip ltBuilder

-- Equal comparator (λmn.and (LEQ m n) (GEQ m n))
eqBuilder :: Builder -> Builder -> Builder
eqBuilder m n = andBuilder (leqBuilder m n) (geqBuilder m n)

-- Not equal comparator (λmn.not (EQ m n))
neqBuilder :: Builder -> Builder -> Builder
neqBuilder m n = notBuilder $ eqBuilder m n
