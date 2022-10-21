{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module AdditionalParser where
import           Control.Applicative (Applicative (liftA2), liftA3)
import           Data.Functor        (($>))
import           Parser              (ParseError (UnexpectedChar, UnexpectedEof),
                                      ParseResult (Error, Result), Parser (P),
                                      between, is, list, list1, space, spaces,
                                      string, (|||))
import           Prelude             hiding (fail)

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

readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _           -> Nothing

-- | Parse numbers as int until non-digit
--
---- >>> parse int "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse int "")
-- True
--
-- >>> isErrorResult (parse int "a")
-- True
int :: Parser Int
int = P f
 where
  -- This is okay because the case statement is small
  f "" = Error UnexpectedEof
  f x  = case readInt x of
    Just (v, rest) -> Result rest v
    Nothing        -> Error $ UnexpectedChar (head x)

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p s =  liftA2 (:) p (list $ s *> p)

-- | Write a function that produces a list of values from repeating the given
-- parser, separated by the second given parser.
--
-- /Hint/: If `sepby1` fails, that means no values were parsed
--
-- >>> parse (sepby char (is ',')) ""
-- Result >< ""
--
-- >>> parse (sepby char (is ',')) "a"
-- Result >< "a"
--
-- >>> parse (sepby char (is ',')) "a,b,c"
-- Result >< "abc"
--
-- >>> parse (sepby char (is ',')) "a,b,c,,def"
-- Result >def< "abc,"
sepby :: Parser a -> Parser s -> Parser [a]
sepby p s = sepby1 p s ||| pure []

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- /Hint/: We just implemented `charTok`
--
-- >>> parse commaTok ",123"
-- Result >123< ','
--
-- >>> isErrorResult( parse commaTok "1,23")
-- True
commaToken :: Parser Char
commaToken = is ',' <* spaces

-- | Write a function that produces a parser for an array (list)
--
-- /Hint/: This uses a similar idea to all the other parsers in this section
--
-- /Hint 2/: This parser can be written in 3 parts:
-- (1) parse the open bracket
-- (2) parse the array contents
-- (3) parse the closing bracket
-- Then return the result of (2)
--
-- /Hint 3/: Array contents are either empty (empty array "[]"), or comma separated values.
-- Do we have something that repeats a parser, separated by some other parser?
--
-- >>> parse (array $ tok int) "[1,2,3,4,5]"
-- Result >< [1,2,3,4,5]
--
-- >>> parse (array $ tok int) "[ 1,2,3,4,5]"
-- Result >< [1,2,3,4,5]
--
-- >>> parse (array $ tok int) "[]"
-- Result >< []
--
-- >>> parse (array $ tok int) "[ ] "
-- Result >< []
--
-- >>> isErrorResult (parse (array $ tok int) "1")
-- True
array :: Parser a -> Parser [a]
array p = charToken '[' *> sepby p commaToken <* charToken ']'


constToken :: String -> a -> Parser a
constToken str = ($>) $ string str

unaryToken :: String -> Parser b -> Parser b
unaryToken str = (*>) $ token (string str)

unaryToken1 :: String -> Parser b -> Parser b
unaryToken1 str = (*>) $ token1 (string str)

binaryToken :: String -> a -> Parser a
binaryToken str = ($>) (betweenSpaces $ string str)

binaryToken1 :: String -> a -> Parser a
binaryToken1 str = ($>) (betweenSpaces1 $ string str)

ternaryToken1 :: (a -> b -> c -> d) -> (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> Parser d
ternaryToken1 joiner (strA, parserA) (strB, parserB) (strC, parserC) = liftA3 joiner
    (unaryToken1 strA parserA)
    (unaryToken1 strB parserB)
    (unaryToken1 strC parserC)
