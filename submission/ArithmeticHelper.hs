{-# OPTIONS_GHC -Wno-typed-holes #-}
module ArithmeticHelper where

import           Data.Builder

-- | Add two church-encoded number (λmnfx.mf(nfx))
addLambda :: Builder
addLambda = lam 'm' $ lam 'n' $ lam 'f' $ lam 'x' $ term 'm' `ap` term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x')

-- | Successor of a number, or in other words add one(λnfx.f(nfx))
succLambda :: Builder
succLambda = lam 'n' $ lam 'f' $ lam 'x' $ term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x')

-- | Multiply two church-encoded number (λmnfx.m(nf)x))
multLambda :: Builder
multLambda = lam 'm' $ lam 'n' $ lam 'f' $ lam 'x' $ term 'm' `ap` (term 'n' `ap` term 'f') `ap` term 'x'

-- | Exponentiate two church-encoded number (λmn.nm)
expLambda :: Builder
expLambda = lam 'm' $ lam 'n' $ term 'n' `ap` term 'm'

-- | Predecessor of a number, or in other words minus one (λnfx.n(λgh.h(gf))(λu.x)(λu.u))
predLambda :: Builder
predLambda = lam 'n' $ lam 'f' $ lam 'x' $ term 'n' `ap` lam 'g' (lam 'h' $ term 'h' `ap` (term 'g' `ap` term 'f')) `ap` lam 'u' (term 'x') `ap` lam 'u' (term 'u')

-- | Subtract two church-encoded number (λmn.(n pred)m)
minusLambda :: Builder
minusLambda = lam 'm' $ lam 'n' $ (term 'n' `ap` predLambda) `ap` term 'm'
