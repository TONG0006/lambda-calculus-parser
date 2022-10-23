module Combinators where
import           AdditionalBuilder (apReplicate2)
import           Data.Builder      (Builder, ap, lam, term)

-- | The K-Combinator Church Encoding
kChurchEncoding :: Builder
kChurchEncoding = lam 'x' $ lam 'y' $ term 'x'

-- | The K-Combinator
kCombinator :: Builder -> Builder -> Builder
kCombinator x _ = x

-- | The Y-Combinator
yChurchEncoding :: Builder
yChurchEncoding = lam 'g' $ apReplicate2 $ lam 'x' $ term 'g' `ap` apReplicate2 (term 'x')

-- | The Y-Combinator
yCombinator :: Builder -> Builder
yCombinator g = apReplicate2 $ lam 'x' $ g `ap` apReplicate2 (term 'x')

iChurchEncoding :: Builder
iChurchEncoding = lam 'x' $ term 'x'
