{-# OPTIONS_GHC -Wno-typed-holes #-}

module LogicHelper where
import           AdditionalBuilder (ap3)
import           Data.Builder      (Builder, lam, term)
import           Prelude           hiding (fail)

-- True (λxy.x)
trueChurchEncoding :: Builder
trueChurchEncoding = lam 'x' $ lam 'y' $ term 'x'

-- False (λxy.y)
falseChurchEncoding :: Builder
falseChurchEncoding = lam 'x' $ lam 'y' $ term 'y'

-- if (λbtf.btf)
ifBuilder :: Builder -> Builder -> Builder -> Builder
ifBuilder = ap3

-- and (λxy. if x y False)
andBuilder :: Builder -> Builder -> Builder
andBuilder x y = ifBuilder x y falseChurchEncoding

-- or (λxy. if x True y)
orBuilder :: Builder -> Builder -> Builder
orBuilder x = ifBuilder x trueChurchEncoding

-- not (λx. If x False True)
notBuilder :: Builder -> Builder
notBuilder x = ifBuilder x falseChurchEncoding trueChurchEncoding
