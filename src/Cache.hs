module Cache where
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import qualified NamelessLambdas as UL

type Identifier = String

data Cache = Cache
    { forward   :: HashMap Identifier     UL.Term
    , backward  :: HashMap UL.Term  Identifier
    }


empty :: Cache
empty = Cache { forward = H.empty, backward = H.empty }

put :: Identifier -> UL.Term -> Cache -> Cache
put f term c = c { forward = H.insert f term (forward c), backward = updatedBackward }
    where
        updatedBackward
            | UL.numFV term == 0 = H.insert term f (backward c)
            | otherwise          = (backward c)

get :: Identifier -> Cache -> Maybe UL.Term
get f c = H.lookup f (forward c)

getBackward :: UL.Term -> Cache -> Maybe Identifier
getBackward term c = H.lookup term (backward c)

toList :: Cache -> [(Identifier, UL.Term)]
toList c = H.toList (forward c)