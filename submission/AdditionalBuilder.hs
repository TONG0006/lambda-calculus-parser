{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AdditionalBuilder where
import           Data.Builder (Builder, ap)
import           Prelude      hiding (fail)


ap3 :: Builder -> Builder -> Builder -> Builder
ap3 b t f = b `ap` t `ap` f
