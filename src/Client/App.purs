module Client.App where

import Prelude

import Client.Components.Alert (alert)
import Client.Components.Button (button)
import Client.Components.Input (input)
import Client.Components.Modal (modal)
import Client.Messages (Connect(..))
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Set (Set)
import Data.String.NonEmpty (fromString)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (Component, JSX, ReactComponentInstance, Self, StateUpdate(..), capture, capture_, createComponent, make)
import React.Basic as React
import React.Basic.DOM (text)
import React.Basic.DOM.Events (targetValue)
import Server.Messages (Connect(..))
import Socket.Client (Socket, createSocket, handle, send)
import Types (MessageText, Room, Time, User(..))


component :: Component {}
component = createComponent "App"

data State
  = EnterUsername
    { username :: String
    , error :: Maybe UserError
    }
  | TryingToJoin
    { username :: String
    }
  | Joined
    { user :: User
    , selectedRoom :: Room
    , allRooms :: Set Room
    , messages :: Array Message
    }

data UserError = UsernameTaken String

data Message
  = UserJoined
    { user :: User
    , time :: Time
    }
  | UserLeft
    { user :: User
    , time :: Time
    }
  | Message
    { user :: User
    , room :: Room
    , text :: MessageText
    , time :: Time
    }

data Action
  = UpdateUsername String
  | JoinChat

socket :: Socket
socket = unsafePerformEffect $ createSocket "http://localhost:3001"

app :: JSX
app = make component { initialState, update, render } {}
  where
  initialState = EnterUsername { username: "", error: Nothing }

  update self@{ state } action = case state, action of
    EnterUsername _, UpdateUsername username ->
      Update $ EnterUsername { username, error: Nothing }

    EnterUsername { username }, JoinChat ->
      UpdateAndSideEffects (TryingToJoin { username }) \self' -> do
        handle socket do
          fromString username
            # maybe (pure unit) (send <<< Connect <<< User)

    _, _ -> NoUpdate

  render self@{ state } =
    case state of
      EnterUsername { username, error } ->
        enterUsernameScreen { self, username, error, disableForm: false }
      TryingToJoin { username } ->
        enterUsernameScreen { self, username, error: Nothing, disableForm: true }
      _ -> mempty


enterUsernameScreen
  :: forall props state
   . { self :: Self props state ReactComponentInstance
     , username :: String
     , error :: Maybe UserError
     , disableForm :: Boolean
     }
   -> JSX
enterUsernameScreen { self, username, error, disableForm } =
  modal
    { visible: true
    , closable: false
    , title: "Enter a username"
    , footer:
      [ button
        { key: "join"
        , "type": "primary"
        , disabled: username == "" || isJust error || disableForm
        , onClick: capture_ self JoinChat
        }
        [ text "Join"
        ]
      ]
    }
    [ input
      { key: "username"
      , value: username
      , disabled: disableForm
      , onChange: capture self targetValue $ maybe "" identity >>> UpdateUsername
      }
      , error
        # maybe mempty case _ of
          UsernameTaken username' ->
            alert
              { "type": "error"
              , message: "The username \"" <> username' <> "\" is already taken."
              }
    ]


