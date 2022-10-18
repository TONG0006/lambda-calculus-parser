{-# OPTIONS_GHC -Wno-typed-holes #-}

module LambdaBuilderParser where
import           Control.Applicative
import           Data.Builder
import           Parser
import           ParserHelper
import           Prelude             hiding (fail)

-- | Parses a sequence of variables
--
-- >>> parse variables "x"
-- *** Exception: The expression `_0` is malformed:
--   Error: The expression contains a free variable `x`
--
-- >>> parse variables "x "
-- Result >< *** Exception: The expression `_0` is malformed:
--   Error: The expression contains a free variable `x`
--
-- >>> parse variables "xy"
-- Result >< *** Exception: The expression `(_0 _0)` is malformed:
--   Error: The expression contains a free variable `x`
--   Error: The expression contains a free variable `y`
--
-- >>> parse variables "x y "
-- Result >< *** Exception: The expression `(_0 _0)` is malformed:
--   Error: The expression contains a free variable `x`
--   Error: The expression contains a free variable `y`
variables :: Parser Builder
variables = foldl1 ap <$> ((term <$>) <$> stringVariables)

longLambda :: Parser Builder
longLambda = liftA2 lam longLambdaParameter longLambdaExpression

longLambdaTerms :: Parser Builder
longLambdaTerms = variables ||| bracket variables ||| bracket longLambda

longLambdaExpression :: Parser Builder
longLambdaExpression = foldl1 ap <$> list1 longLambdaTerms


shortLambda :: Parser Builder
shortLambda = liftA2 (flip $ foldr lam) shortLambdaParameter shortLambdaExpression

shortLambdaTerms :: Parser Builder
shortLambdaTerms = variables ||| bracket variables ||| shortLambda ||| bracket shortLambda

shortLambdaExpression :: Parser Builder
shortLambdaExpression = foldl1 ap <$> list1 shortLambdaTerms
