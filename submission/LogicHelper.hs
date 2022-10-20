{-# OPTIONS_GHC -Wno-typed-holes #-}

module LogicHelper where
import           Data.Builder
import           Prelude      hiding (fail)

-- True (λxy.x)
trueChurchEncoding :: Builder
trueChurchEncoding = lam 'x' $ lam 'y' $ term 'x'

-- False (λxy.y)
falseChurchEncoding :: Builder
falseChurchEncoding = lam 'x' $ lam 'y' $ term 'y'

-- if (λbtf.btf)
ifChurchEncoding :: Builder
ifChurchEncoding = lam 'b' $ lam 't' $ lam 'f' $ term 'b' `ap` term 't' `ap` term 'f'

-- and (λxy. if x y False)
andChurchEncoding :: Builder
andChurchEncoding = lam 'x' $ lam 'y' $ ifChurchEncoding `ap` term 'x' `ap` term 'y' `ap` falseChurchEncoding

-- or (λxy. if x True y)
orChurchEncoding :: Builder
orChurchEncoding = lam 'x' $ lam 'y' $ ifChurchEncoding `ap` term 'x' `ap` trueChurchEncoding `ap` term 'y'

-- not (λx. If x False True)
notChurchEncoding :: Builder
notChurchEncoding = lam 'x' $ ifChurchEncoding `ap` term 'x' `ap` falseChurchEncoding `ap` trueChurchEncoding

ifBuilder :: Builder -> Builder -> Builder -> Builder
ifBuilder b t f = ifChurchEncoding `ap` b `ap` t `ap` f

andBuilder :: Builder -> Builder -> Builder
andBuilder x y = andChurchEncoding `ap` x `ap` y

orBuilder :: Builder -> Builder -> Builder
orBuilder x y = orChurchEncoding `ap` x `ap` y

notBuilder :: Builder -> Builder
notBuilder x = notChurchEncoding `ap` x
