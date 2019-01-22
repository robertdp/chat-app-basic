module Client.Messages where

import Data.Generic.Rep (class Generic)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Socket.Types (class ClientMessage)
import Types


data Connect
  = Connect User

instance clientMessageConnect :: ClientMessage "chat.connect" Connect
derive instance genericConnect :: Generic Connect _
instance decodeConnect :: Decode Connect where decode = genericDecode defaultOptions
instance encodeConnect :: Encode Connect where encode = genericEncode defaultOptions

data Chat
  = CreateRoom Room
  | SendMessage Room MessageText
  
instance clientMessageChat :: ClientMessage "chat.message" Chat
derive instance genericChat :: Generic Chat _
instance decodeChat :: Decode Chat where decode = genericDecode defaultOptions
instance encodeChat :: Encode Chat where encode = genericEncode defaultOptions