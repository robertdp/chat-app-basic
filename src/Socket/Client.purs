module Socket.Client
  ( Socket
  , Handler
  , createSocket
  , runClient
  , onDisconnect
  , onReconnect
  , send
  , receive
  )
  where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Foreign.Generic (decodeJSON, encodeJSON)
import Socket.Types (class ClientMessage, class ServerMessage)


-- | The Socket instance for the current connection.
foreign import data Socket :: Type

-- | This is our server-side socket handling context.
newtype Handler a = Handler (ReaderT Socket Effect a)
derive newtype instance functorHandler :: Functor Handler
derive newtype instance applyHandler :: Apply Handler
derive newtype instance applicativeHandler :: Applicative Handler
derive newtype instance bindHandler :: Bind Handler
derive newtype instance monadHandler :: Monad Handler
derive newtype instance monadEffectHandler :: MonadEffect Handler

foreign import createSocket :: String -> Effect Socket

foreign import _on :: forall a b. EffectFn3 a String (EffectFn1 b Unit) Unit

runClient :: forall a. Socket -> Handler a -> Effect a
runClient socket (Handler handler) = runReaderT handler socket

-- | Logic to run when the client disconnects.
onDisconnect :: Handler Unit -> Handler Unit
onDisconnect (Handler handler) = Handler do
  socket <- ask
  liftEffect $ runEffectFn3 _on socket "disconnect" $ mkEffectFn1 \_ -> runReaderT handler socket

-- | Logic to run when the client disconnects.
onReconnect :: Handler Unit -> Handler Unit
onReconnect (Handler handler) = Handler do
  socket <- ask
  liftEffect $ runEffectFn3 _on socket "reconnect" $ mkEffectFn1 \_ -> runReaderT handler socket

foreign import _emit :: EffectFn3 Socket String String Unit

-- | Sends a message to the server in the pre-specified channel.
send :: forall channel msg. ClientMessage channel msg => msg -> Handler Unit
send a = Handler do
  socket <- ask
  liftEffect $ runEffectFn3 _emit socket channel (encodeJSON a)
  where
    channel = reflectSymbol (SProxy :: SProxy channel)

-- | Listens for server messages in the pre-specified channel.
receive :: forall channel msg. ServerMessage channel msg => (msg -> Handler Unit) -> Handler Unit
receive handler = Handler do
  let channel = reflectSymbol (SProxy :: SProxy channel)
  socket <- ask
  liftEffect $ runEffectFn3 _on socket channel $ mkEffectFn1 $ runHandler socket
  where
    runHandler :: Socket -> String -> Effect Unit
    runHandler socket msg =
      decodeJSON msg
        # runExcept
        # case _ of
          Left errors -> Console.log $ "Invalid message: " <> msg
          Right value -> runClient socket $ handler value
