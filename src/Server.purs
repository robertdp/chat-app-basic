module Server where

import Prelude

import Client.Messages (Connect(..))
import Client.Messages as Client
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty (fromString)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Server.Messages (Chat(..), Connect(..))
import Socket.Server (Handler, broadcast, createServer, onConnection, onDisconnect, receive, send)
import Types (Room(..), Time(..), User)

main :: Effect Unit
main = do
  server <- createServer 3001
  state <- initialState
  onConnection server $ handler state

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
handler { users, rooms } = do
  userRef <- liftEffect $ Ref.new Nothing

  receive case _ of
    Client.Connect user -> do
      let usernameTaken = liftEffect $ Set.member user <$> Ref.read users
      ifM usernameTaken (send $ UserAlreadyExists user) do
        liftEffect $ Ref.modify_ (Set.insert user) users
        liftEffect $ Ref.write (Just user) userRef
        time <- getCurrentTime
        rooms' <- liftEffect $ Ref.read rooms
        send $ Connected user $ Set.toUnfoldable rooms'
        broadcast $ UserJoined user time

  receive \msg -> do
    user' <- liftEffect $ Ref.read userRef
    for_ user' \user -> case msg of
      Client.CreateRoom room -> do
        liftEffect $ Ref.modify_ (Set.insert room) rooms
        time <- getCurrentTime
        broadcast $ RoomCreated user room time
      Client.SendMessage room text -> do
        time <- getCurrentTime
        broadcast $ Message user room text time

  onDisconnect do
    user' <- liftEffect $ Ref.read userRef
    for_ user' \user -> do
      liftEffect $ Ref.write Nothing userRef
      time <- getCurrentTime
      broadcast $ UserLeft user time

getCurrentTime :: forall m. MonadEffect m => m Time
getCurrentTime = liftEffect $ Time <$> Now.nowDateTime