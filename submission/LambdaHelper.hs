module LambdaHelper where
import           AdditionalParser (dotToken, lambdaToken, token)
import           Parser           (Parser, between, list1, oneof)

-- $setup
-- >>> import Parser (parse)

-- | Parses a long lambda parameter expression
-- >>> parse longLambdaParameter "λx.x"
-- Result >x< 'x'
-- >>> parse longLambdaParameter "λxy.x"
-- UnexpectedChar 'y'
longLambdaParameter :: Parser Char
longLambdaParameter = between lambdaToken dotToken charVariable

-- | Parses a short lambda parameter expression
-- >>> parse shortLambdaParameter "λx.x"
-- Result >x< "x"
-- >>> parse shortLambdaParameter "λxyz.x"
-- Result >x< "xyz"
shortLambdaParameter :: Parser [Char]
shortLambdaParameter = between lambdaToken dotToken (list1 charVariable)

-- | List of valid variable chars
validVariables :: [Char]
validVariables = '_':['a'..'z']

-- | Parses a single variable
-- >>> parse charVariable "x"
-- Result >< 'x'
-- >>> parse charVariable "x "
-- Result >< 'x'
-- >>> parse charVariable "yz"
-- Result >z< 'y'
-- >>> parse charVariable "y z"
-- Result >z< 'y'
-- >>> parse charVariable "1"
-- UnexpectedChar '1'
charVariable :: Parser Char
charVariable = token $ oneof validVariables

-- | Parses a sequence of variables
-- >>> parse stringVariables "x"
-- Result >< "x"
-- >>> parse stringVariables "x "
-- Result >< "x"
-- >>> parse stringVariables "xyz"
-- Result >< "xyz"
-- >>> parse stringVariables "xyz "
-- Result >< "xyz"
-- >>> parse stringVariables "xyz12"
-- Result >12< "xyz"
-- >>> parse stringVariables "12"
-- UnexpectedChar '1'
stringVariables :: Parser String
stringVariables =  list1 charVariable
