{-# OPTIONS_GHC -Wno-typed-holes #-}

module LogicHelper where
import           Data.Builder
import           Prelude      hiding (fail)

-- True (λxy.x)
trueBuilder :: Builder
trueBuilder = lam 'x' $ lam 'y' $ term 'x'

-- False (λxy.y)
falseBuilder :: Builder
falseBuilder = lam 'x' $ lam 'y' $ term 'y'

-- If (λbtf.btf)
ifBuilder :: Builder
ifBuilder = lam 'b' $ lam 't' $ lam 'f' $ term 'b' `ap` term 't' `ap` term 'f'

-- And (λxy. If x y False)
andBuilder :: Builder
andBuilder = lam 'x' $ lam 'y' $ ifBuilder `ap` term 'x' `ap` term 'y' `ap` falseBuilder

-- OR (λxy. If x True y)
orBuilder :: Builder
orBuilder = lam 'x' $ lam 'y' $ ifBuilder `ap` term 'x' `ap` falseBuilder `ap` term 'y'

-- NOT (λx. If x False True)
notBuilder :: Builder
notBuilder = lam 'x' $ ifBuilder `ap` term 'x' `ap` falseBuilder `ap` trueBuilder
