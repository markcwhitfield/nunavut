module Nunavut.Util.NonEmpty where

import           Control.Lens       (Lens', lens)
import           Data.List.NonEmpty (NonEmpty (..), fromList, toList, (<|))

_head :: Lens' (NonEmpty a) a
_head                         = lens (head . toList) setHead
  where setHead (_ :| ls) v   = v :| ls

_last :: Lens' (NonEmpty a) a
_last                         = lens (last . toList) setLast
  where setLast (_ :| []) v   = v :| []
        setLast (l :| ls) v   = l <| setLast (fromList ls) v
