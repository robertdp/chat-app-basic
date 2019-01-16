module Server where

import Prelude
import Control.Socket.Server (Handler)
import Control.Socket.Server as Socket
import Effect (Effect)
import Node.HTTP as HTTP

main :: Effect Unit
main = do
  server <- HTTP.createServer \_ _ -> mempty
  socket <- Socket.upgradeServer server
  Socket.onConnection socket handler
  HTTP.listenSocket server "3001" mempty

handler :: Handler Unit
handler = pure unit