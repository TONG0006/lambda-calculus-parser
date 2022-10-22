module ComparatorParser where
import           AdditionalParser   (binaryToken, bracket, chain)
import           ArithmeticParser   (arithmeticExpression)
import           ComparatorHelper   (eqBuilder, geqBuilder, gtBuilder,
                                     leqBuilder, ltBuilder, neqBuilder)
import           Data.Builder       (Builder)
import           LogicBuilderParser (logicalPrecedence, logicalTerm)
import           Parser             (Parser, (|||))

-- $setup
-- >>> import Data.Lambda (lamToBool, lamToInt)
-- >>> import AdditionalBuilder (normalBuild)
-- >>> import ArithmeticParser (intLambda)

-- | Parses a leq term
-- >>> lamToBool <$> normalBuild (chain intLambda leqLambda) "2<=3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda leqLambda) "2<=2"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda leqLambda) "3<=2"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda leqLambda) "2 <=3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda leqLambda) "2<= 3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda leqLambda) "2 <= 3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda leqLambda) "1<=2<=3"
-- Result >< Nothing
-- >>> lamToBool <$> normalBuild (chain intLambda leqLambda) "3<=2<=1"
-- Result >< Just True
leqLambda :: Parser (Builder -> Builder -> Builder)
leqLambda = binaryToken "<=" leqBuilder

-- | Parses a geq term
-- >>> lamToBool <$> normalBuild (chain intLambda geqLambda) "3>=2"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda geqLambda) "2>=2"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda geqLambda) "2>=3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda geqLambda) "2 >=3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda geqLambda) "2>= 3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda geqLambda) "2 >= 3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda geqLambda) "3>=2>=1"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda geqLambda) "1>=2>=3"
-- Result >< Just False
geqLambda :: Parser (Builder -> Builder -> Builder)
geqLambda = binaryToken ">=" geqBuilder

-- | Parses a lt term
-- >>> lamToBool <$> normalBuild (chain intLambda ltLambda) "2<3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda ltLambda) "2<2"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda ltLambda) "3<2"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda ltLambda) "2 <3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda ltLambda) "2< 3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda ltLambda) "2 < 3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda ltLambda) "1<2<3"
-- Result >< Nothing
-- >>> lamToBool <$> normalBuild (chain intLambda ltLambda) "3<2<1"
-- Result >< Just True
ltLambda :: Parser (Builder -> Builder -> Builder)
ltLambda = binaryToken "<" ltBuilder

-- | Parses a gt term
-- >>> lamToBool <$> normalBuild (chain intLambda gtLambda) "3>2"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda gtLambda) "2>2"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda gtLambda) "2>3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda gtLambda) "2 >3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda gtLambda) "2> 3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda gtLambda) "2 > 3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda gtLambda) "3>2>1"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda gtLambda) "1>2>3"
-- Result >< Just False
gtLambda :: Parser (Builder -> Builder -> Builder)
gtLambda = binaryToken ">" gtBuilder

-- | Parses an eq term
-- >>> lamToBool <$> normalBuild (chain intLambda eqLambda) "3==2"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda eqLambda) "2==2"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda eqLambda) "2==3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda eqLambda) "2 ==3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda eqLambda) "2== 3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda eqLambda) "2 == 3"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda eqLambda) "3==2==1"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda eqLambda) "1==2==3"
-- Result >< Just False
eqLambda :: Parser (Builder -> Builder -> Builder)
eqLambda = binaryToken "==" eqBuilder

-- | Parses a neq term
-- >>> lamToBool <$> normalBuild (chain intLambda neqLambda) "3!=2"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda neqLambda) "2!=2"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild (chain intLambda neqLambda) "2!=3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda neqLambda) "2 !=3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda neqLambda) "2!= 3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda neqLambda) "2 != 3"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda neqLambda) "3!=2!=1"
-- Result >< Just True
-- >>> lamToBool <$> normalBuild (chain intLambda neqLambda) "1!=2!=3"
-- Result >< Just True
neqLambda :: Parser (Builder -> Builder -> Builder)
neqLambda = binaryToken "!=" neqBuilder

-- | Comparator precedence (Doesn't actually matter right now since they're all the same level, but could add custom ones)
comparatorPrecedence :: [Parser (Builder -> Builder -> Builder)]
comparatorPrecedence = [leqLambda ||| geqLambda ||| ltLambda ||| gtLambda ||| eqLambda ||| neqLambda]

-- | Parses a single expression that prioritises arithmetic and then comparators
-- >>> lamToInt <$> normalBuild comparatorTerm "3"
-- Result >< Just 3
-- >>> lamToInt <$> normalBuild comparatorTerm "3-2"
-- Result >< Just 1
-- >>> lamToInt <$> normalBuild comparatorTerm "3*2-1*(2+1)"
-- Result >< Just 3
-- >>> lamToBool <$> normalBuild comparatorTerm "(False)"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild comparatorTerm "(False and True)"
-- Result >< Just False
-- >>> lamToBool <$> normalBuild comparatorTerm "(2<3) and True"
-- Result > and True< Just True
-- >>> lamToBool <$> normalBuild comparatorTerm "2<3"
-- Result ><3< Nothing
-- >>> lamToBool <$> normalBuild comparatorTerm "2<3 and True"
-- Result ><3 and True< Nothing
comparatorTerm :: Parser Builder
comparatorTerm = arithmeticExpression ||| bracket comparatorExpression

-- | Chain together all the comparator terms (multiple)
comparatorExpression :: Parser Builder
comparatorExpression = foldl chain (comparatorTerm ||| logicalTerm) (comparatorPrecedence ++ logicalPrecedence)
