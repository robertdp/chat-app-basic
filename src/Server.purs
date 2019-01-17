module Server where

import Prelude
import Client.Messages (Connect(..))
import Control.Server (Handler, receive)
import Control.Server as Socket
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty (fromString, toString)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.HTTP as HTTP
import Partial.Unsafe (unsafePartial)
import Types

main :: Effect Unit
main = do
  server <- Socket.createServer 3001
  state <- initialState
  Socket.onConnection server $ handler state

type State =
  { users :: Ref (Set User)
  , rooms :: Ref (Set Room)
  }

initialState :: Effect State
initialState = ado
  users <- Ref.new Set.empty
  rooms <- Ref.new $ Set.singleton $ defaultRoom
  in { users, rooms }

defaultRoom :: Room
defaultRoom = Room $ unsafePartial $ fromJust $ fromString "General"

handler :: State -> Handler Unit
handler _ = do
  receive case _ of
    Connect (User user) ->
      liftEffect $ Console.log $ toString user

