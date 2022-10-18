{-# OPTIONS_GHC -Wno-typed-holes #-}

module ParserHelper where
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

-- | Parses a lambda token
--
-- >>> parse lambdaToken "λab"
-- Result >ab< '\955'
--
-- >>> parse lambdaToken "dabc"
-- UnexpectedChar 'd'
lambdaToken :: Parser Char
lambdaToken = is 'λ' <* spaces

-- | Parses a dot token
--
-- >>> parse dotToken ".ab"
-- Result >ab< '.'
--
-- >>> parse dotToken "dabc"
-- UnexpectedChar 'd'
dotToken :: Parser Char
dotToken = is '.' <* spaces

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

-- | Parses a long lambda parameter expression
--
-- >>> parse longLambdaParameter "λx.x"
-- Result >x< 'x'
--
-- >>> parse longLambdaParameter "λxy.x"
-- UnexpectedChar 'y'
longLambdaParameter :: Parser Char
longLambdaParameter = between lambdaToken dotToken charVariable

-- | Parses a short lambda parameter expression
--
-- >>> parse shortLambdaParameter "λx.x"
-- Result >x< 'x'
--
-- >>> parse shortLambdaParameter "λxyz.x"
-- Result >x< "xyz"
shortLambdaParameter :: Parser [Char]
shortLambdaParameter = between lambdaToken dotToken (list1 charVariable)



-- | List of valid variable chars
validVariables :: [Char]
validVariables = ['a'..'z']

-- | Parses a single variable
--
-- >>> parse charVariable "x"
-- Result >< 'x'
--
-- >>> parse charVariable "x "
-- Result >< 'x'
--
-- >>> parse charVariable "yz"
-- Result >z< 'y'
--
-- >>> parse charVariable "y z"
-- Result >z< 'y'
--
-- >>> parse charVariable "1"
-- UnexpectedChar '1'
charVariable :: Parser Char
charVariable = token $ oneof validVariables

-- | Parses a sequence of variables
--
--
-- >>> parse stringVariables "x"
-- Result >< "x"
--
-- >>> parse stringVariables "x "
-- Result >< "x"
--
-- >>> parse stringVariables "xyz"
-- Result >< "xyz"
-- >>> parse stringVariables "xyz "
-- Result >< "xyz"
--
-- >>> parse stringVariables "xyz12"
-- Result >12< "xyz"
--
-- >>> parse stringVariables "12"
-- UnexpectedChar '1'
stringVariables :: Parser String
stringVariables =  list1 charVariable
