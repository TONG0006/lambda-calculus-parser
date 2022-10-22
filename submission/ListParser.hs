module ListParser where
import           AdditionalParser   (arrayToken, chain, token, unaryToken1)
import           ArithmeticParser   (intLambda)
import           Data.Builder       (Builder)
import           ListHelper         (consBuilder, headBuilder, isNullBuilder,
                                     nullBuilder, tailBuilder)
import           LogicBuilderParser (logicalTerm)
import           Parser             (Parser, (|||))

-- $setup
-- >>> import AdditionalBuilder (normalBuild)
-- >>> import Data.Lambda (lamToInt, lamToBool, normal)
-- >>> import Data.Builder (build)
-- >>> import Parser (parse)

-- | The possible datatypes implemented in lambda calculus
datatypeTerm :: Parser Builder
datatypeTerm = intLambda ||| logicalTerm

-- | Parses a list using conslist approach
-- >>> normalBuild consToken "[]"
-- Result >< \cn.n
-- >>> normalBuild consToken "[1]"
-- Result >< \c.c\f.f
-- >>> normalBuild consToken "[1,2]"
-- Result >< \cn.c(\f.f)(c(\fx.f(fx))n)
-- >>> normalBuild consToken "[1,2,3]"
-- Result >< \cn.c(\f.f)(c(\fx.f(fx))(c(\fx.f(f(fx)))n))
-- >>> normalBuild consToken "[True]"
-- Result >< \c.c\xy.x
-- >>> normalBuild consToken "[True, True]"
-- Result >< \cn.c(\xy.x)(c(\xy.x)n)
-- >>> normalBuild consToken "[True, False, True]"
-- Result >< \cn.c(\xy.x)(c(\xy.y)(c(\xy.x)n))
-- >>> normalBuild consToken "[1, True, 2, False]"
-- Result >< \cn.c(\f.f)(c(\xy.x)(c(\fx.f(fx))(c(\xy.y)n)))
-- >>> normalBuild consToken "[ 1, 2, 3]"
-- Result >< \cn.c(\f.f)(c(\fx.f(fx))(c(\fx.f(f(fx)))n))
-- >>> normalBuild consToken "[1 ,2 ,3 ]"
-- Result >< \cn.c(\f.f)(c(\fx.f(fx))(c(\fx.f(f(fx)))n))
-- >>> normalBuild consToken "[ 1 , 2 , 3 ]"
-- Result >< \cn.c(\f.f)(c(\fx.f(fx))(c(\fx.f(f(fx)))n))
-- >>> normalBuild consToken "[1,,3]"
-- UnexpectedChar ','
-- >>> normalBuild consToken "[1,2,3"
-- UnexpectedEof
consToken :: Parser Builder
consToken = foldr consBuilder nullBuilder <$> arrayToken (token datatypeTerm)

-- | Parses a head operation
-- >>> lamToInt <$> normalBuild headToken "head [1]"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild headToken "head [2,3]"
-- Result >< Just 2
-- >>> lamToBool <$> normalBuild headToken "head [True]"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild headToken "head [False, True]"
-- Result >< Just False
-- >>> lamToInt <$> normalBuild headToken "head [3, False, 4, True]"
-- Result >< Just 3
-- >>> lamToInt <$> normalBuild headToken "head []"
-- Result >< Just 0
-- >>> lamToBool <$> normalBuild headToken "head []"
-- Result >< Just False
-- >>> lamToBool . normal . build . isNullBuilder <$> parse listToken "[]"
-- Result >< Just True
-- >>> normalBuild headToken "head[]"
-- UnexpectedChar '['
-- >>> normalBuild headToken "head "
-- UnexpectedEof
-- >>> normalBuild headToken "head"
-- UnexpectedEof
headToken :: Parser Builder
headToken = unaryToken1 "head" $ headBuilder <$> listToken

-- | Parses a tail operation
-- >>> lamToInt . normal . build . headBuilder <$> parse tailToken "rest [1,2,3]"
-- Result >< Just 2
-- >>> lamToInt . normal . build . headBuilder <$> parse tailToken "rest [2,3,4]"
-- Result >< Just 3
-- >>> lamToBool . normal . build . headBuilder <$> parse tailToken "rest [True,False,True]"
-- Result >< Just False
-- >>> lamToBool . normal . build . headBuilder <$> parse tailToken "rest [False,True,False]"
-- Result >< Just True
-- >>> lamToBool . normal . build . isNullBuilder <$> parse listToken "rest [15]"
-- Result >< Just True
-- >>> lamToBool . normal . build . isNullBuilder <$> parse listToken "rest []"
-- Result >< Just True
-- >>> normalBuild tailToken "rest[]"
-- UnexpectedChar '['
-- >>> normalBuild tailToken "rest "
-- UnexpectedEof
-- >>> normalBuild tailToken "rest"
-- UnexpectedEof
tailToken :: Parser Builder
tailToken = unaryToken1 "rest" $ tailBuilder <$> listToken

-- | Parses a isNull operation
-- >>> lamToBool <$> normalBuild isNullToken "isNull []"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild isNullToken "isNull [1]"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild isNullToken "isNull [1,2]"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild isNullToken "isNull [True]"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild isNullToken "isNull [True, False]"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild isNullToken "isNull [1, True, 2, False]"
-- Result >< Just False
-- >>> normalBuild isNullToken "isNull[]"
-- UnexpectedChar '['
-- >>> normalBuild isNullToken "isNull "
-- UnexpectedEof
-- >>> normalBuild isNullToken "isNull"
-- UnexpectedEof
isNullToken :: Parser Builder
isNullToken = unaryToken1 "isNull" $ isNullBuilder <$> listToken

-- | Token for parsing a list returning operation
listToken :: Parser Builder
listToken = tailToken ||| consToken

-- | Token for parsing an item returning operation
listOperator :: Parser Builder
listOperator = listToken ||| headToken ||| isNullToken

-- | precedence for binary operations (currently unused and kept for design)
listPrecedence :: [Parser (Builder -> Builder -> Builder)]
listPrecedence = []

-- | Parses list expressions and their operations (refer to LambdaParser for test cases)
listExpression :: Parser Builder
listExpression = foldl chain listOperator listPrecedence
