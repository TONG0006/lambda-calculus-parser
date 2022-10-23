module AdditionalBuilder where
import           Data.Builder (Builder, ap, build, lam)
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
-- >>> e = term 'e'

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

-- | Concatenates 5 builders using ap
-- >>> lam 'a' $ lam 'b' $ lam 'c' $ lam 'd' $ lam 'e' $ ap5 a b c d e
-- \abcde.abcde
ap5 :: Builder -> Builder -> Builder -> Builder -> Builder -> Builder
ap5 a b c d e = a `ap` b `ap` c `ap` d `ap` e

apReplicate2 :: Builder -> Builder
apReplicate2 x = x `ap` x

-- | A function that parses, builds and then normalises (debugging)
normalBuild :: Parser Builder -> Input -> ParseResult Lambda
normalBuild p str = normal . build <$> parse p str

lamBuild :: String -> Builder -> Builder
lamBuild [] _             = undefined
lamBuild list body = foldl (flip lam) initial terms
    where
        vars = reverse list
        terms = tail vars
        initial = lam (head vars) body
