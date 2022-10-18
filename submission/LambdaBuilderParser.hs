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
variables = foldr1 ap <$> ((term <$>) <$> stringVariables)

longLambda :: Parser Builder
longLambda = liftA2 lam longLambdaParameter longLambdaExpression

longLambdaTerms :: Parser Builder
longLambdaTerms = variables ||| bracket variables ||| bracket longLambda

longLambdaExpression :: Parser Builder
longLambdaExpression = foldl1 ap <$> list1 longLambdaTerms


rawVarExpr :: Parser Builder
rawVarExpr = foldr1 ap <$> ((term <$>) <$> stringVariables)

rawLamExpr :: Parser Builder
rawLamExpr = liftA2 lam (is 'λ' *> charVariable <* is '.') multExpr

rawVarLamExpr :: Parser Builder
rawVarLamExpr = rawVarExpr ||| rawLamExpr

brExpr :: Parser Builder -> Parser Builder
brExpr =  between (is '(') (is ')')

brLamExpr :: Parser Builder
brLamExpr = brExpr rawLamExpr

brVarLamExpr :: Parser Builder
brVarLamExpr = brExpr rawVarLamExpr

expr :: Parser Builder -> Parser Builder
expr p =  brExpr p ||| p

varLamExpr :: Parser Builder
varLamExpr = expr rawVarLamExpr

multLamExpr :: Parser Builder
multLamExpr = foldl1 ap <$> list1 varLamExpr

multBrLamExpr :: Parser Builder
multBrLamExpr = foldl1 ap <$> list1 brLamExpr

multExpr :: Parser Builder
multExpr = foldl1 ap <$> list1 varLamExpr
