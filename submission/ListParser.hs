{-# OPTIONS_GHC -Wno-typed-holes #-}

module ListParser where
import           AdditionalBuilder (ap3, ap4)
import           Data.Builder      (Builder, ap, lam, term)
import           LogicHelper       (falseChurchEncoding, trueChurchEncoding)
import           Prelude           hiding (fail)

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)

-- null list (λcn.n)
nullBuilder :: Builder
nullBuilder = lam 'c' $ lam 'n' $ term 'n'

isNullBuilder :: Builder -> Builder
isNullBuilder l = ap3 l (lam 'h' $ lam 't' falseChurchEncoding) trueChurchEncoding

consBuilder :: Builder -> Builder -> Builder
consBuilder h t = lam 'c' $ lam 'n' $ ap3 (term 'c') h $ ap3 t (term 'c') (term 'n')

headBuilder :: Builder -> Builder
headBuilder l = ap3 l (lam 'h' $ lam 't' $ term 't') falseChurchEncoding

tailBuilder :: Builder -> Builder
tailBuilder l = lam 'c' $ lam 'n' $ ap4 l (lam 'h' $ lam 't' $ lam 'g' $ ap3 (term 'g') (term 'h') (term 't' `ap` term 'c')) (lam 't' $ term 'n') (lam 'h' $ lam 't' $ term 't')
