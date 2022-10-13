{-# OPTIONS_GHC -Wno-typed-holes #-}

module LambdaHelperParser where
import           Control.Applicative
import           Data.Builder
import           Parser
import           Prelude             hiding (fail)


isTerm :: Parser Char
isTerm = oneof ['a'..'z']

rawVarExpr :: Parser Builder
rawVarExpr = foldr1 ap <$> ((term <$>) <$> munch1 (`elem` ['a'..'z']))

rawLamExpr :: Parser Builder
rawLamExpr = liftA2 lam (is 'λ' *> isTerm <* is '.') multExpr

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
multLamExpr = foldl1 ap <$> list1 brLamExpr

multExpr :: Parser Builder
multExpr = foldl1 ap <$> list1 varLamExpr
