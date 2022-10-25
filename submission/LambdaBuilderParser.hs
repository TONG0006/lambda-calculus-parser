{-# OPTIONS_GHC -Wno-typed-holes #-}

module LambdaBuilderParser where
import           AdditionalParser    (betweenSpaces, bracket)
import           Control.Applicative (Applicative (liftA2))
import           Data.Builder        (Builder, ap, lam, term)
import           LambdaHelper        (longLambdaParameter, shortLambdaParameter,
                                      stringVariables)
import           Parser              (Parser, list1, (|||))
import           Prelude             hiding (fail)

-- $setup
-- >>> import Parser (parse)
-- >>> import AdditionalBuilder (normalBuild)

-- | Parses a sequence of variables
-- >>> lam 'x' <$> parse variables "x"
-- Result >< \x.x
-- >>> lam 'x' . lam 'y' <$> parse variables "xy"
-- Result >< \xy.xy
-- >>> lam 'x' . lam 'y' . lam 'z' <$> parse variables "xyz"
-- Result >< \xyz.xyz
-- >>> lam 'x' . lam 'y' <$> parse variables "x y"
-- Result >< \xy.xy
-- >>> lam 'x' . lam 'y' <$> parse variables "x y "
-- Result >< \xy.xy
-- >>> lam 'x' <$> parse variables "x "
-- Result >< \x.x
-- >>> lam 'x' <$> parse variables " x"
-- UnexpectedChar ' '
variables :: Parser Builder
variables = foldl1 ap <$> ((term <$>) <$> stringVariables)

-- | Parses a single short lambda term (can be nested but not multiplied, first depth can't have brackets)
-- >>> normalBuild longLambda "λx.x"
-- Result >< \x.x
-- >>> normalBuild longLambda "λx.(λy.yx)"
-- Result >< \xy.yx
-- >>> normalBuild longLambda "λx.(λy.(λz.zyx))"
-- Result >< \xyz.zyx
-- >>> normalBuild longLambda "λx.xλy.y"
-- Result >λy.y< \x.x
-- >>> normalBuild longLambda "(λx.x)"
-- UnexpectedChar '('
longLambda :: Parser Builder
longLambda = liftA2 lam longLambdaParameter longLambdaExpression

-- | Parses the terms possible inside a long lambda expression
longLambdaTerms :: Parser Builder
longLambdaTerms = variables ||| bracket longLambda ||| bracket longLambdaExpression

-- | Parses a chain of long lambda terms (refer to LambdaParser for test cases)
longLambdaExpression :: Parser Builder
longLambdaExpression = foldl1 ap <$> list1 (betweenSpaces longLambdaTerms)

-- | Parses a single short lambda term similar to long lambda
shortLambda :: Parser Builder
shortLambda = liftA2 (flip $ foldr lam) shortLambdaParameter shortLambdaExpression

-- | Parses the terms possible inside a short lambda expression
shortLambdaTerms :: Parser Builder
shortLambdaTerms = variables ||| shortLambda ||| bracket shortLambdaExpression

-- | Parses a chain of short lambda terms (refer to LambdaParser for test cases)
shortLambdaExpression :: Parser Builder
shortLambdaExpression = foldl1 ap <$> list1 (betweenSpaces shortLambdaTerms)
