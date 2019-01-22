module Server where

import Prelude

import Client.Messages as Client
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.NonEmpty (fromString)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import Server.Messages (Chat(..), Connect(..))
import Socket.Server (Handler)
import Socket.Server as Socket
import Types (Room(..), Time(..), User)

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
handler { users, rooms } = do
  userRef <- liftEffect $ Ref.new Nothing

  let roomExists room = Set.member room <$> (liftEffect $ Ref.read rooms)
      userConnected = isJust <$> (liftEffect $ Ref.read userRef)
      withUser f = (liftEffect $ Ref.read userRef) >>= maybe (pure unit) f

  Socket.listen case _ of
    Client.Connect user -> do
      let usernameTaken = Set.member user <$> (liftEffect $ Ref.read users)
      unlessM userConnected do
        ifM usernameTaken (Socket.send $ UserAlreadyExists user) do
          liftEffect $ Ref.modify_ (Set.insert user) users
          liftEffect $ Ref.write (Just user) userRef
          time <- getCurrentTime
          rooms' <- liftEffect $ Ref.read rooms
          Socket.send $ Connected user $ Set.toUnfoldable rooms'
          Socket.broadcast $ UserJoined user time

  Socket.listen \msg -> do
    withUser \user -> case msg of
      Client.CreateRoom room -> do
        unlessM (roomExists room) do
          liftEffect $ Ref.modify_ (Set.insert room) rooms
          time <- getCurrentTime
          Socket.broadcast $ RoomCreated user room time
      Client.SendMessage room text -> do
        unlessM (roomExists room) do
          time <- getCurrentTime
          Socket.broadcast $ Message user room text time

  Socket.onDisconnect do
    withUser \user -> do
      liftEffect $ Ref.write Nothing userRef
      liftEffect $ Ref.modify_ (Set.delete user) users
      time <- getCurrentTime
      Socket.broadcast $ UserLeft user time

getCurrentTime :: forall m. MonadEffect m => m Time
getCurrentTime = liftEffect $ Time <$> JSDate.now