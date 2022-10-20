{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AdditionalBuilder where
import           Data.Builder (Builder, ap)
import           Prelude      hiding (fail)


ap3 :: Builder -> Builder -> Builder -> Builder
ap3 a b c = a `ap` b `ap` c

ap4 :: Builder -> Builder -> Builder -> Builder -> Builder
ap4 a b c d = a `ap` b `ap` c `ap` d
