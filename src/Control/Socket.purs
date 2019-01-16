module Control.Socket where

import Data.Symbol (class IsSymbol)
import Foreign.Class (class Decode, class Encode)


class (Decode msg, Encode msg, IsSymbol event) <= ServerEvent event msg | msg -> event

class (Decode msg, Encode msg, IsSymbol event) <= ClientEvent event msg | msg -> event
