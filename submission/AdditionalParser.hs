{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AdditionalParser where
import           Parser
import           Prelude hiding (fail)

spaces1 :: Parser String
spaces1 = list1 space

-- | Parses a token, ignoring the spaces in front of it
--
-- >>> parse (token (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (token (is 'a')) "abc"
-- Result >bc< 'a'
token :: Parser a -> Parser a
token p = p <* spaces

-- | Same as token but requires at least 1 space in front of it
--
-- >>> parse (token1 (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (token1 (is 'a')) "abc"
-- UnexpectedChar 'b'
token1 :: Parser a -> Parser a
token1 p = p <* spaces1

-- | Parses a char token
--
-- >>> parse (charToken 'a') "abc"
-- Result >bc< 'a'
--
-- >>> parse (charToken 'a') "dabc"
-- UnexpectedChar 'd'
charToken :: Char -> Parser Char
charToken c = is c <* spaces

-- | Parses a ( token
--
-- >>> parse openBracketToken "(ab"
-- Result >ab< '('
--
-- >>> parse openBracketToken "dabc"
-- UnexpectedChar 'd'
openBracketToken :: Parser Char
openBracketToken = is '(' <* spaces

-- | Parses a ) token
--
-- >>> parse closeBracketToken ")ab"
-- Result >ab< ')'
--
-- >>> parse closeBracketToken "dabc"
-- UnexpectedChar 'd'
closeBracketToken :: Parser Char
closeBracketToken = is ')' <* spaces

-- | Parses a bracketed expression
--
-- >>> parse closeBracketToken ")ab"
-- Result >ab< ')'
--
-- >>> parse closeBracketToken "dabc"
-- UnexpectedChar 'd'
bracket :: Parser a -> Parser a
bracket = between openBracketToken closeBracketToken

-- | `chain p op` parses 1 or more instances of `p` separated by `op`
-- | (see chainl1 from Text.Parsec)
-- | This can be a very useful parser combinator
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
  where
    rest a =
      (do
        f <- op
        b <- p
        rest $ f a b
      ) ||| pure a

-- | Parses a parser in between spaces
betweenSpaces :: Parser a -> Parser a
betweenSpaces p = do
    spaces
    result <- p
    spaces
    return result

-- | Parses a parser in between spaces
betweenSpaces1 :: Parser a -> Parser a
betweenSpaces1 p = do
    spaces1
    result <- p
    spaces1
    return result
