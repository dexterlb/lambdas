module Cache where
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import qualified NamelessLambdas as UL

type Identifier = String

data Cache = Cache
    { forward   :: HashMap Identifier     UL.Expression
    , backward  :: HashMap UL.Expression  Identifier
    }


empty :: Cache
empty = Cache { forward = H.empty, backward = H.empty }

put :: Identifier -> UL.Expression -> Cache -> Cache
put f term c = c { forward = H.insert f term (forward c), backward = updatedBackward }
    where
        updatedBackward
            | UL.numFV term == 0 = H.insert term f (backward c)
            | otherwise          = (backward c)

get :: Identifier -> Cache -> Maybe UL.Expression
get f c = H.lookup f (forward c)

getBackward :: UL.Expression -> Cache -> Maybe Identifier
getBackward term c = H.lookup term (backward c)

toList :: Cache -> [(Identifier, UL.Expression)]
toList c = H.toList (forward c)