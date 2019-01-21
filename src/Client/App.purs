module Client.App where

import Prelude

import Client.AntDesign.Alert (alert)
import Client.AntDesign.Button (button)
import Client.AntDesign.Input (input)
import Client.AntDesign.Modal (modal)
import Client.Messages (Connect(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.String.NonEmpty (fromString, toString)
import Effect.Class (liftEffect)
import React.Basic (Component, JSX, ReactComponentInstance, Self, StateUpdate(..), capture, capture_, createComponent, make)
import React.Basic as React
import React.Basic.DOM (text)
import React.Basic.DOM.Events (targetValue)
import Server.Messages as Server
import Socket.Client (Socket, runClient)
import Socket.Client as Client
import Types (Room, User(..))


data State
  = EnterUsername String (Maybe UserError)
  | TryingToJoin String
  | Joined User Room (Set Room) (Array Server.Chat)

data Action
  = UpdateUsername String
  | JoinChat
  | SetUserError UserError
  | SetUser User
  | SetRooms (Array Room)
  | AddMessage Server.Chat

component :: Component { socket :: Socket }
component = createComponent "App"

app :: { socket :: Socket } -> JSX
app = make component { initialState, didMount, update, render }
  where
  initialState = EnterUsername "" Nothing

  didMount self = do
    let  send_ = liftEffect <<< React.send self
    runClient self.props.socket do
      Client.receive case _ of
        Server.UserAlreadyExists user ->
          send_ $ SetUserError $ UsernameTaken $ toString $ unwrap user
        Server.Connected user rooms -> do
          send_ $ SetUser user
          send_ $ SetRooms rooms
      Client.receive \msg ->
        send_ $ AddMessage msg
          

  update self@{ state, props } action = case state, action of
    EnterUsername _ error, UpdateUsername username ->
      Update $ EnterUsername username error

    EnterUsername username _, JoinChat ->
      UpdateAndSideEffects (TryingToJoin username) \self' -> do
        runClient props.socket do
          fromString username
            # maybe (pure unit) (Client.send <<< Connect <<< User)

    _, _ -> NoUpdate

  render self@{ state } =
    case state of
      EnterUsername username error ->
        enterUsernameScreen self { username, error, disableForm: false }
      TryingToJoin username ->
        enterUsernameScreen self { username, error: Nothing, disableForm: true }
      _ -> mempty

data UserError = UsernameTaken String

enterUsernameScreen
  :: forall props state
   . Self props state ReactComponentInstance
  -> { username :: String
     , error :: Maybe UserError
     , disableForm :: Boolean
     }
  -> JSX
enterUsernameScreen self { username, error, disableForm } =
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


