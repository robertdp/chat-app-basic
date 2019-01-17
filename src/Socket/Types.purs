module Socket.Types where

import Data.Symbol (class IsSymbol)
import Foreign.Class (class Decode, class Encode)


class (Decode msg, Encode msg, IsSymbol channel)
  <= ServerMessage channel msg | msg -> channel

class (Decode msg, Encode msg, IsSymbol channel)
  <= ClientMessage channel msg | msg -> channel
