{-# OPTIONS_GHC -Wno-typed-holes #-}

module AdditionalParser where
import           Parser
import           Prelude hiding (fail)

-- | Parses a token, ignoring the spaces in front of it
--
-- >>> parse (token (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (token (is 'a')) "abc"
-- Result >bc< 'a'
token :: Parser a -> Parser a
token p = p <* spaces

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

-- | Parses the given string (fails otherwise).
--
-- >>> parse (string "hello") "hello bob"
-- Just (" bob","hello")
-- >>> parse (string "hey") "hello bob"
-- Nothing
string :: String -> Parser String
string = traverse is
