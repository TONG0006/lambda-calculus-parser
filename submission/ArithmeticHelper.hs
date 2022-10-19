{-# OPTIONS_GHC -Wno-typed-holes #-}
module ArithmeticHelper where

import           Data.Builder

-- | Add two church-encoded number (λmnfx.mf(nfx))
addChurchEncoding :: Builder
addChurchEncoding = lam 'm' $ lam 'n' $ lam 'f' $ lam 'x' $ term 'm' `ap` term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x')

-- | Successor of a number, or in other words add one(λnfx.f(nfx))
succChurchEncoding :: Builder
succChurchEncoding = lam 'n' $ lam 'f' $ lam 'x' $ term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x')

-- | Multiply two church-encoded number (λmnfx.m(nf)x))
multChurchEncoding :: Builder
multChurchEncoding = lam 'm' $ lam 'n' $ lam 'f' $ lam 'x' $ term 'm' `ap` (term 'n' `ap` term 'f') `ap` term 'x'

-- | Exponentiate two church-encoded number (λmn.nm)
expChurchEncoding :: Builder
expChurchEncoding = lam 'm' $ lam 'n' $ term 'n' `ap` term 'm'

-- | Predecessor of a number, or in other words minus one (λnfx.n(λgh.h(gf))(λu.x)(λu.u))
predChurchEncoding :: Builder
predChurchEncoding = lam 'n' $ lam 'f' $ lam 'x' $ term 'n' `ap` lam 'g' (lam 'h' $ term 'h' `ap` (term 'g' `ap` term 'f')) `ap` lam 'u' (term 'x') `ap` lam 'u' (term 'u')

-- | Subtract two church-encoded number (λmn.(n pred)m)
minusChurchEncoding :: Builder
minusChurchEncoding = lam 'm' $ lam 'n' $ (term 'n' `ap` predChurchEncoding) `ap` term 'm'

-- | Add two church-encoded number (λmnfx.mf(nfx))
addBuilder :: Builder -> Builder -> Builder
addBuilder m n = lam 'f' $ lam 'x' $ m `ap` term 'f' `ap` (n `ap` term 'f' `ap` term 'x')

-- | Successor of a number, or in other words add one(λnfx.f(nfx))
succBuilder :: Builder -> Builder
succBuilder n = lam 'f' $ lam 'x' $ term 'f' `ap` (n `ap` term 'f' `ap` term 'x')

-- | Multiply two church-encoded number (λmnfx.m(nf)x))
multBuilder :: Builder -> Builder -> Builder
multBuilder m n = lam 'f' $ lam 'x' $ m `ap` (n `ap` term 'f') `ap` term 'x'

-- | Exponentiate two church-encoded number (λmn.nm)
expBuilder :: Builder -> Builder -> Builder
expBuilder = flip ap

-- | Predecessor of a number, or in other words minus one (λnfx.n(λgh.h(gf))(λu.x)(λu.u))
predBuilder :: Builder -> Builder
predBuilder n = lam 'f' $ lam 'x' $ n `ap` lam 'g' (lam 'h' $ term 'h' `ap` (term 'g' `ap` term 'f')) `ap` lam 'u' (term 'x') `ap` lam 'u' (term 'u')

-- | Subtract two church-encoded number (λmn.(n pred)m)
minusBuilder :: Builder -> Builder -> Builder
minusBuilder m n = (n `ap` predChurchEncoding) `ap` m


