module AdditionalBuilder where
import           Data.Builder (Builder, ap, build)
import           Data.Lambda  (Lambda, normal)
import           Parser       (Input, ParseResult, Parser, parse)

-- $setup
-- >>> import Data.Builder (lam, term)
-- >>> import Control.Applicative (liftA3)
-- >>> import Parser (parse)
-- >>> a = term 'a'
-- >>> b = term 'b'
-- >>> c = term 'c'
-- >>> d = term 'd'

-- | Concatenates 3 builders using ap
-- >>> lam 'a' $ lam 'b' $ lam 'c' $ ap3 a b c
-- \abc.abc
-- >>> lam 'a' . lam 'b' . lam 'c' <$> liftA3 ap3 (pure a) (pure b) (pure c)
-- \abc.abc
ap3 :: Builder -> Builder -> Builder -> Builder
ap3 a b c = a `ap` b `ap` c

-- | Concatenates 4 builders using ap
-- >>> lam 'a' $ lam 'b' $ lam 'c' $ lam 'd' $ ap4 a b c d
-- \abcd.abcd
ap4 :: Builder -> Builder -> Builder -> Builder -> Builder
ap4 a b c d = a `ap` b `ap` c `ap` d

-- | A function that parses, builds and then normalises (debugging)
normalBuild :: Parser Builder -> Input -> ParseResult Lambda
normalBuild p str = normal . build <$> parse p str
