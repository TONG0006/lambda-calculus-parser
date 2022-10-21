{-# OPTIONS_GHC -Wno-typed-holes #-}

module ListHelper where
import           AdditionalBuilder (ap3, ap4)
import           Data.Builder      (Builder, ap, lam, term)
import           LogicHelper       (falseChurchEncoding, trueChurchEncoding)
import           Prelude           hiding (fail)

-- | [] = null = λcn.n
nullBuilder :: Builder
nullBuilder = lam 'c' $ lam 'n' $ term 'n'

-- | isNull = λl.l(λht.False) True
isNullBuilder :: Builder -> Builder
isNullBuilder l = ap3 l (lam 'h' $ lam 't' falseChurchEncoding) trueChurchEncoding

-- | cons = λhtcn.ch(tcn)
consBuilder :: Builder -> Builder -> Builder
consBuilder h t = lam 'c' $ lam 'n' $ ap3 (term 'c') h $ ap3 t (term 'c') (term 'n')

-- | head = λl.l(λht.h) False
headBuilder :: Builder -> Builder
headBuilder l = ap3 l (lam 'h' $ lam 't' $ term 'h') falseChurchEncoding

-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
tailBuilder :: Builder -> Builder
tailBuilder l = lam 'c' $ lam 'n' $ ap4 l (lam 'h' $ lam 't' $ lam 'g' $ ap3 (term 'g') (term 'h') (term 't' `ap` term 'c')) (lam 't' $ term 'n') (lam 'h' $ lam 't' $ term 't')
