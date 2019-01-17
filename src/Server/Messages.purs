module Server.Messages where

import Data.Generic.Rep (class Generic)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Socket.Types (class ServerMessage)
import Types

data Connect
  = UserAlreadyExists User
  | Connected User

instance serverMessageConnect :: ServerMessage "chat.connect" Connect
derive instance genericConnect :: Generic Connect _
instance decodeConnect :: Decode Connect where decode = genericDecode defaultOptions
instance encodeConnect :: Encode Connect where encode = genericEncode defaultOptions

data Chat
  = RoomCreated Room
  | UserJoined User Time
  | UserLeft User Time
  | Message User Room MessageText Time

instance serverMessageChat :: ServerMessage "chat.message" Chat
derive instance genericChat :: Generic Chat _
instance decodeChat :: Decode Chat where decode = genericDecode defaultOptions
instance encodeChat :: Encode Chat where encode = genericEncode defaultOptions