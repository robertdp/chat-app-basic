module Control.Socket.Server
  ( Socket
  , Handler
  , onDisconnect
  , onReconnect
  , send
  , receive
  )
  where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Socket (class ClientEvent, class ServerEvent)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Foreign.Generic (decodeJSON, encodeJSON)


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

foreign import createSocket :: Effect Socket

foreign import _on :: forall a b. EffectFn3 a String (EffectFn1 b Unit) Unit

-- | Logic to run when the client disconnects.
onDisconnect :: Handler Unit -> Handler Unit
onDisconnect (Handler handler) = Handler do
  socket <- ask
  liftEffect $ runEffectFn3 _on socket "disconnect" $ mkEffectFn1 $ runReaderT handler

-- | Logic to run when the client disconnects.
onReconnect :: Handler Unit -> Handler Unit
onReconnect (Handler handler) = Handler do
  socket <- ask
  liftEffect $ runEffectFn3 _on socket "reconnect" $ mkEffectFn1 $ runReaderT handler

foreign import _emit :: EffectFn3 Socket String String Unit

-- | Sends a message to the server in the pre-specified event channel.
send :: forall event msg. ClientEvent event msg => msg -> Handler Unit
send a = Handler do
  socket <- ask
  liftEffect $ runEffectFn3 _emit socket event (encodeJSON a)
  where
    event = reflectSymbol (SProxy :: SProxy event)

-- | Listens for server messages in the pre-specified channel.
receive :: forall event msg. ServerEvent event msg => (msg -> Handler Unit) -> Handler Unit
receive f = Handler do
  socket <- ask
  liftEffect $ runEffectFn3 _on socket event $ mkEffectFn1 handler
  where
    event = reflectSymbol (SProxy :: SProxy event)
    handler msg =
      decodeJSON msg
        # runExcept
        # case _ of
          Left _ -> Console.log $ "Invalid message: " <> msg
          Right message -> handler message
