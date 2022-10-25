{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module LogicBuilderParser where
import           AdditionalParser (binaryToken1, bracket, chain, constToken,
                                   ternaryToken1, token1, unaryToken1)
import           Data.Builder     (Builder)
import           LogicHelper      (andBuilder, falseChurchEncoding, ifBuilder,
                                   notBuilder, orBuilder, trueChurchEncoding)
import           Parser           (Parser, (|||))

-- $setup
-- >>> import Data.Lambda (lamToBool)
-- >>> import AdditionalBuilder (normalBuild)

-- | Parses a True term
-- >>> lamToBool <$> normalBuild logicalTrue "True"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild logicalTrue "True and False"
-- Result > and False< Just True
-- >>> lamToBool <$> normalBuild logicalTrue "true"
-- UnexpectedChar 't'
logicalTrue :: Parser Builder
logicalTrue = constToken "True" trueChurchEncoding

-- | Parses a False term
-- >>> lamToBool <$> normalBuild logicalFalse "False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild logicalFalse "False and True"
-- Result > and True< Just False
-- >>> lamToBool <$> normalBuild logicalFalse "false"
-- UnexpectedChar 'f'
logicalFalse :: Parser Builder
logicalFalse = constToken "False" falseChurchEncoding

-- | Parses a logical value
logical :: Parser Builder
logical = logicalTrue ||| logicalFalse

-- | Parses the and operator
-- >>> lamToBool <$> normalBuild (chain logical andToken) "True and True"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain logical andToken) "True and False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain logical andToken) "False and True"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain logical andToken) "False and False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain logical andToken) "True and True and True"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain logical andToken) "True and True and False and True"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain logical andToken) "Trueand True"
-- Result >and True< Just True
-- >>> lamToBool <$> normalBuild (chain logical andToken) "True andTrue"
-- Result > andTrue< Just True
andToken :: Parser (Builder -> Builder -> Builder)
andToken = binaryToken1 "and" andBuilder

-- | Parses the or operator
-- >>> lamToBool <$> normalBuild (chain logical orToken) "True or True"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain logical orToken) "True or False"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain logical orToken) "False or True"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain logical orToken) "False or False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain logical orToken) "True or True or False"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain logical orToken) "True or True or True or True"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain logical orToken) "Trueor True"
-- Result >or True< Just True
-- >>> lamToBool <$> normalBuild (chain logical orToken) "True orTrue"
-- Result > orTrue< Just True
orToken :: Parser (Builder -> Builder -> Builder)
orToken = binaryToken1 "or" orBuilder

-- | The precedence of logical operators
logicalPrecedence :: [Parser (Builder -> Builder -> Builder)]
logicalPrecedence = [andToken, orToken]

-- | Parses an if statement
-- >>> lamToBool <$> normalBuild logicalIf "if True then True else False"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild logicalIf "if False then True else False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild logicalIf "if True and True then True else False"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild logicalIf "if True or False then True else False"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild logicalIf "if True and False then True else False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild logicalIf "if True or True and False then True else False"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild logicalIf "if (True or True) and False then True else False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild logicalIf "if True then True or False else False"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild logicalIf "if False then True else True and False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild logicalIf "if if True then False else True then True else False"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild logicalIf "ifTrue else True and False"
-- UnexpectedChar 'T'
-- >>> lamToBool <$> normalBuild logicalIf "if Trueelse True and False"
-- UnexpectedChar 'e'
-- >>> lamToBool <$> normalBuild logicalIf "if True elseTrue and False"
-- UnexpectedChar 'e'
-- >>> lamToBool <$> normalBuild logicalIf "if True else Trueand False"
-- UnexpectedChar 'e'
-- >>> lamToBool <$> normalBuild logicalIf "if True else True andFalse"
-- UnexpectedChar 'e'
-- >>> lamToBool <$> normalBuild logicalIf "if True else True and False"
-- UnexpectedChar 'e'
-- >>> lamToBool <$> normalBuild logicalIf "if True else True and False "
-- UnexpectedChar 'e'
logicalIf :: Parser Builder
logicalIf = ternaryToken1 ifBuilder
    ("if", token1 logicalExpression)
    ("then", token1 logicalExpression)
    ("else", logicalExpression)

-- | Parses a not statement
-- >>> lamToBool <$> normalBuild logicalNot "not True"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild logicalNot "not False"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild logicalNot "not not True"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild logicalNot "not not False"
-- Result >< Just False
logicalNot :: Parser Builder
logicalNot = unaryToken1 "not" $ notBuilder <$> logicalTerm

-- | Logical Operators that doesn't require binding
logicalOperator :: Parser Builder
logicalOperator = logicalIf ||| logicalNot

-- | The possible terms that return a logical value
logicalTerm :: Parser Builder
logicalTerm = logical ||| logicalOperator ||| bracket logicalExpression

-- | Parses a logical operation (refer to LambdaParser for test cases)
logicalExpression :: Parser Builder
logicalExpression = foldl chain logicalTerm logicalPrecedence
