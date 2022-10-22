module AdditionalParser where
import           Control.Applicative (Applicative (liftA2), liftA3)
import           Data.Functor        (($>))
import           Parser              (ParseError (UnexpectedChar, UnexpectedEof),
                                      ParseResult (Error, Result), Parser (P),
                                      between, is, list, list1, space, spaces,
                                      string, (|||))

-- $setup
-- >>> import Parser (parse)
-- >>> bool = constToken "True" True ||| constToken "False" False
-- >>> bool1 = (constToken "True" True <* space) ||| (constToken "False" False <* space)
-- >>> boolToken = (stringToken "True" $> True) ||| (stringToken "False" $> False)
-- >>> ifThenElse x y z = if x then y else z

--
-- Tokens
--
-- | Parses a token, ignoring the spaces in front of it
token :: Parser a -> Parser a
token p = p <* spaces

-- | Same as token but requires at least 1 space in front of it
token1 :: Parser a -> Parser a
token1 = flip (<*) spaces1

-- | Parses a char token
charToken :: Char -> Parser Char
charToken = token . is

-- | Parses a string token
stringToken :: String -> Parser String
stringToken = token . string

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
commaToken :: Parser Char
commaToken = betweenSpaces $ is ','

-- | Parses an lambda 'λ' followed by 0 or more spaces.
lambdaToken :: Parser Char
lambdaToken = charToken 'λ'

-- | Parses an dot '.' followed by 0 or more spaces.
dotToken :: Parser Char
dotToken = charToken '.'

-- | Parses an open bracket '(' followed by 0 or more spaces.
openParenthesisToken :: Parser Char
openParenthesisToken = charToken '('

-- | Parses a close bracket ')' followed by 0 or more spaces.
closeParenthesisToken :: Parser Char
closeParenthesisToken = is ')'

-- | Parses an open bracket '[' followed by 0 or more spaces.
openBracketToken :: Parser Char
openBracketToken = charToken '['

-- | Parses a close bracket ']' followed by 0 or more spaces.
closeBracketToken :: Parser Char
closeBracketToken = spaces *> is ']'

-- | Parses a bracketed expression
-- >>> parse (bracket int) "(321)"
-- Result >< 321
-- >>> parse (bracket $ string "VIM") "(VIM)"
-- Result >< "VIM"
-- >>> parse (bracket int) "(321"
-- UnexpectedEof
-- >>> parse (bracket int) "321)"
-- UnexpectedChar '3'
-- >>> parse (bracket int) "321"
-- UnexpectedChar '3'
bracket :: Parser a -> Parser a
bracket = between openParenthesisToken closeParenthesisToken

-- | Parses an array
arrayToken :: Parser a -> Parser [a]
arrayToken p = between openBracketToken closeBracketToken $ sepby p commaToken

-- | Parses a constant function parser
-- >>> parse (constToken "const" 'd') "const"
-- Result >< 'd'
-- >>> parse (constToken "token" 3) "token"
-- Result >< 3
-- >>> parse (constToken "const" 'd') "bc"
-- UnexpectedChar 'b'
constToken :: String -> a -> Parser a
constToken str = ($>) $ string str

-- | Parses a unary function parser
-- >>> parse (unaryToken "-" $ (*(-1)) <$> int) "-3"
-- Result >< -3
-- >>> parse (unaryToken "!" $ not <$> boolToken) "!True"
-- Result >< False
-- >>> parse (unaryToken "!" $ not <$> boolToken) "! True"
-- Result >< False
-- >>> parse (unaryToken "!" $ not <$> boolToken) "!false"
-- UnexpectedChar 'f'
unaryToken :: String -> Parser b -> Parser b
unaryToken str = (*>) $ token (string str)

-- | Same as unaryToken but requires at least one space in between
-- >>> parse (unaryToken1 "not" $ not <$> boolToken) "not True"
-- Result >< False
-- >>> parse (unaryToken1 "abs" $ abs <$> int) "abs -3"
-- Result >< 3
-- >>> parse (unaryToken1 "not" $ constToken "True" "False") "notTrue"
-- UnexpectedChar 'T'
unaryToken1 :: String -> Parser b -> Parser b
unaryToken1 str = (*>) $ token1 (string str)

-- | Parses a binary function parser (made with chain in mind, has slightly different format)
-- >>> parse (chain int $ binaryToken "+" (+)) "1+2"
-- Result >< 3
-- >>> parse (chain int $ binaryToken "+" (+)) "1+2+3"
-- Result >< 6
-- >>> parse (chain int $ binaryToken "+" (+)) "1+2+3+4"
-- Result >< 10
-- >>> parse (chain int $ binaryToken "*" (*)) "1*2*3"
-- Result >< 6
-- >>> parse (chain int $ binaryToken "+" (+)) "1"
-- Result >< 1
-- >>> parse (chain int $ binaryToken "+" (+)) "1+"
-- Result >+< 1
-- >>> parse (chain int $ binaryToken "+" (+)) "+2"
-- UnexpectedChar '+'
-- >>> parse (chain int $ binaryToken "+" (+)) ""
-- UnexpectedEof
binaryToken :: String -> a -> Parser a
binaryToken str = ($>) (betweenSpaces $ string str)

-- | Same as binaryToken but with a required spacing in between the operators
-- >>> parse (chain bool $ binaryToken1 "and" (&&)) "True and False"
-- Result >< False
-- >>> parse (chain bool $ binaryToken1 "and" (&&)) "True and True"
-- Result >< True
-- >>> parse (chain int $ binaryToken1 "+" (+)) "1+2"
-- Result >+2< 1
binaryToken1 :: String -> a -> Parser a
binaryToken1 str = ($>) (betweenSpaces1 $ string str)

-- | Parses a ternary function parser (made with operator functions before each term)
-- >>> parse (ternaryToken ifThenElse ("|", boolToken) ("?", boolToken) (":", boolToken)) "|True?True:False"
-- Result >< True
-- >>> parse (ternaryToken ifThenElse ("|", boolToken) ("?", boolToken) (":", boolToken)) "|False?True:False"
-- Result >< False
-- >>> parse (ternaryToken (\x y z -> -x+y*z) ("-", int) ("+", int) ("*", int)) "-2+3*4"
-- Result >< 10
-- >>> parse (ternaryToken ifThenElse ("|", boolToken) ("?", boolToken) (":", boolToken)) "| True? True: False"
-- Result >< True
-- >>> parse (ternaryToken ifThenElse ("|", boolToken) ("?", boolToken) (":", boolToken)) "| True ? True : False"
-- Result >< True
-- >>> parse (ternaryToken ifThenElse ("|", boolToken) ("?", boolToken) (":", boolToken)) "True?True:False"
-- UnexpectedChar 'T'
-- >>> parse (ternaryToken ifThenElse ("|", boolToken) ("?", boolToken) (":", boolToken)) "|TrueTrue:False"
-- UnexpectedChar 'T'
-- >>> parse (ternaryToken ifThenElse ("|", boolToken) ("?", boolToken) (":", boolToken)) "|True?TrueFalse"
-- UnexpectedChar 'F'
ternaryToken :: (a -> b -> c -> d) -> (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> Parser d
ternaryToken joiner (strA, parserA) (strB, parserB) (strC, parserC) = liftA3 joiner
    (unaryToken strA parserA)
    (unaryToken strB parserB)
    (unaryToken strC parserC)

-- | Same as ternaryToken but with at least one space after each operator
-- >>> parse (ternaryToken1 ifThenElse ("|", bool1) ("?", bool1) (":", boolToken)) "| True ? True : False"
-- Result >< True
-- >>> parse (ternaryToken1 ifThenElse ("|", boolToken) ("?", boolToken) (":", boolToken)) "| True? True: False"
-- Result >< True
-- >>> parse (ternaryToken1 ifThenElse ("if", bool1) ("then", bool1) ("else", boolToken)) "if True then True else False"
-- Result >< True
-- >>> parse (ternaryToken1 ifThenElse ("|", bool1) ("?", bool1) (":", boolToken)) "| True? True: False"
-- UnexpectedChar 'T'
-- >>> parse (ternaryToken1 ifThenElse ("|", bool1) ("?", bool1) (":", boolToken)) "|True ?True :False "
-- UnexpectedChar 'T'
-- >>> parse (ternaryToken1 ifThenElse ("|", bool1) ("?", bool1) (":", boolToken)) "|True?True:False"
-- UnexpectedChar 'T'
ternaryToken1 :: (a -> b -> c -> d) -> (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> Parser d
ternaryToken1 joiner (strA, parserA) (strB, parserB) (strC, parserC) = liftA3 joiner
    (unaryToken1 strA parserA)
    (unaryToken1 strB parserB)
    (unaryToken1 strC parserC)

--
-- Basic parsers
--
-- | Attempts to cast a string into an integer
readInt :: String -> Maybe (Int, String)
readInt s = case reads s of
    [(x, rest)] -> Just (x, rest)
    _           -> Nothing

-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
char :: Parser Char
char = P f
    where
        f ""       = Error UnexpectedEof
        f (x : xs) = Result xs x

-- | Parse numbers as int until non-digit
int :: Parser Int
int = P f
    where
        -- This is okay because the case statement is small
        f "" = Error UnexpectedEof
        f x  = case readInt x of
            Just (v, rest) -> Result rest v
            Nothing        -> Error $ UnexpectedChar (head x)

-- | Parses at least one space
-- >>> parse spaces1 " "
-- Result >< " "
-- >>> parse spaces1 "  "
-- Result >< "  "
-- >>> parse spaces1 "  a"
-- Result >a< "  "
-- >>> parse spaces1 "a"
-- UnexpectedChar 'a'
spaces1 :: Parser String
spaces1 = list1 space

-- | Produces a list of values from repeating the given parser, separated by the second given parser.
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p s =  liftA2 (:) p (list $ s *> p)

-- | Write a function that produces a list of values from repeating the given parser, separated by the second given parser.
sepby :: Parser a -> Parser s -> Parser [a]
sepby p s = sepby1 p s ||| pure []

-- | Parses a parser in between spaces
-- >>> parse (betweenSpaces $ is 'c') "c"
-- Result >< 'c'
-- >>> parse (betweenSpaces $ is 'c') " c"
-- Result >< 'c'
-- >>> parse (betweenSpaces $ is 'c') "c "
-- Result >< 'c'
-- >>> parse (betweenSpaces $ is 'c') " c "
-- Result >< 'c'
-- >>> parse (betweenSpaces $ is 'c') "  c  "
-- Result >< 'c'
-- >>> parse (betweenSpaces $ is 'c') "  c "
-- Result >< 'c'
-- >>> parse (betweenSpaces $ is 'c') ""
-- UnexpectedEof
betweenSpaces :: Parser a -> Parser a
betweenSpaces p = spaces *> p <* spaces

-- | Same as betweenSpaces but requires at least one space on either side
-- parse (betweenSpaces $ is 'c') " c "
-- Result >< 'c'
-- parse (betweenSpaces $ is 'c') "  c "
-- Result >< 'c'
-- parse (betweenSpaces $ is 'c') " c  "
-- Result >< 'c'
-- parse (betweenSpaces $ is 'c') "c"
-- UnexpectedChar 'c'
-- parse (betweenSpaces $ is 'c') "c "
-- UnexpectedChar 'c'
-- parse (betweenSpaces $ is 'c') " c"
-- UnexpectedEof
-- parse (betweenSpaces $ is 'c') ""
-- UnexpectedEof
betweenSpaces1 :: Parser a -> Parser a
betweenSpaces1 p = spaces1 *> p <* spaces1

--
-- Other helpers
--
-- | Checks if a parse result returns an error
isErrorResult :: ParseResult a -> Bool
isErrorResult (Error _) = True
isErrorResult _         = False

-- | `chain p op` parses 1 or more instances of `p` separated by `op`
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
    where
        rest a = (do
            f <- op
            b <- p
            rest $ f a b) ||| pure a
