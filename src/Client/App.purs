module Client.App where

import Prelude hiding (div)
import Client.AntDesign.Alert as Alert
import Client.AntDesign.Button as Button
import Client.AntDesign.Input as Input
import Client.AntDesign.Layout as Layout
import Client.AntDesign.Menu as Menu
import Client.AntDesign.Modal as Modal
import Client.Messages as Client
import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (Component, JSX, ReactComponentInstance, Self, StateUpdate(..), capture, capture_, createComponent, make)
import React.Basic as React
import React.Basic.DOM (css, div, div_, h2, h3, text)
import React.Basic.DOM.Events (targetValue)
import Server.Messages (Chat(..))
import Server.Messages as Server
import Socket.Client (Socket)
import Socket.Client as Socket
import Types (MessageText(..), Room(..), User(..), getCurrentTime, unwrapToString)
import Unsafe.Coerce (unsafeCoerce)


data State
  = EnterUsername String (Maybe UserError)
  | TryingToJoin String
  | Joined
    { user :: User
    , currentRoom :: Room
    , rooms :: Set Room
    , newRoomName :: String
    , newMessageText :: String
    , messages :: Array Server.Chat
    }

data Action
  = NoOp
  | UpdateUsername String
  | AttemptToConnect
  | JoinChat User Room (Set Room)
  | SetUserError UserError
  | AddRoom Room
  | CreateRoom String
  | AddMessage Server.Chat
  | SelectRoom Room
  | UpdateNewRoomName String
  | UpdateNewMessageText String
  | SendMessage String

component :: Component { socket :: Socket }
component = createComponent "App"

app :: { socket :: Socket } -> JSX
app = make component { initialState, didMount, update, render }
  where
  initialState = EnterUsername "" Nothing

  didMount self = do
    let action = liftEffect <<< React.send self
    
    Socket.run self.props.socket do
      Socket.listen case _ of
        Server.UserAlreadyExists user ->
          action $ SetUserError $ UsernameTaken $ unwrapToString user
        Server.Connected user rooms -> do
          for_ (Array.head rooms) \room -> do
            action $ JoinChat user room (Set.fromFoldable rooms)
      Socket.listen \msg -> do
        action $ AddMessage msg
        case msg of
          Server.RoomCreated _ room _ ->
            action $ AddRoom room
          _ ->
            pure unit

  -- This function is basically the state transition logic for the app.
  -- i.e. If this action is encountered in this state, perform this transition.
  update { state } action = case action, state of
    NoOp, _ ->
      NoUpdate
    
    UpdateUsername username, EnterUsername _ error ->
      let newState = EnterUsername username error
      in Update newState

    AttemptToConnect, EnterUsername username _ ->
      let maybeUser = User <$> NonEmptyString.fromString username
          newState = TryingToJoin username
          effects user self = Socket.run self.props.socket do
            Socket.send $ Client.Connect user
      in maybe NoUpdate (\user -> UpdateAndSideEffects newState (effects user)) maybeUser
    
    SetUserError error, TryingToJoin username ->
      let newState = EnterUsername username (Just error)
      in Update newState
    
    JoinChat user currentRoom rooms, TryingToJoin _ ->
      let newState = Joined { user, currentRoom, rooms, newRoomName: "", newMessageText: "", messages: [] }
      in Update newState
    
    CreateRoom roomName, Joined joinedState  ->
      let maybeRoom = Room <$> NonEmptyString.fromString roomName
          newState room = Joined $ joinedState { currentRoom = room, rooms = Set.insert room joinedState.rooms, newRoomName = "" }
          effects room self = Socket.run self.props.socket do
            Socket.send $ Client.CreateRoom room
      in maybe NoUpdate (\room -> UpdateAndSideEffects (newState room) (effects room)) maybeRoom
    
    AddRoom room, Joined joinedState  ->
      let newState = Joined $ joinedState { rooms = Set.insert room joinedState.rooms }
      in Update newState
    
    AddMessage msg, Joined joinedState ->
      let newState = Joined $ joinedState { messages = Array.snoc joinedState.messages msg }
      in Update newState
    
    SelectRoom room, Joined joinedState ->
      let newState = Joined $ joinedState { currentRoom = room }
      in Update newState
    
    UpdateNewRoomName room, Joined joinedState ->
      let newState = Joined $ joinedState { newRoomName = room }
      in Update newState
    
    UpdateNewMessageText messageText, Joined joinedState ->
      let newState = Joined $ joinedState { newMessageText = messageText }
      in Update newState
    
    SendMessage message, Joined joinedState  ->
      let maybeMessageText = MessageText <$> NonEmptyString.fromString message
          newState = Joined $ joinedState { newMessageText = "" }
          effects messageText self = Socket.run self.props.socket do
            Socket.send $ Client.SendMessage joinedState.currentRoom messageText
            liftEffect do
              time <- getCurrentTime
              let newMessage = Server.Message joinedState.user joinedState.currentRoom messageText time
              React.send self $ AddMessage newMessage
      in maybe NoUpdate (\messageText -> UpdateAndSideEffects newState (effects messageText)) maybeMessageText

    _, _ ->
      SideEffects \_ -> do
        let unsafeLog :: forall a. a -> Effect Unit
            unsafeLog = unsafeCoerce Console.log
        Console.log "Unhandled action"
        Console.log "Action:"
        unsafeLog action
        Console.log "State:"
        unsafeLog state

  render self =
    case self.state of
      EnterUsername username error ->
        enterUsernameScreen self { username, error, disableForm: false }
      TryingToJoin username ->
        enterUsernameScreen self { username, error: Nothing, disableForm: true }
      Joined joinedState  ->
        chatScreen self joinedState

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
  Modal.modal
    { visible: true
    , closable: false
    , title: "Enter a username"
    , footer:
      [ Button.button
        { key: "join"
        , "type": "primary"
        , disabled: username == "" || disableForm
        , onClick: capture_ self AttemptToConnect
        }
        [ text "Join"
        ]
      ]
    }
    [ Input.input
      { key: "username"
      , value: username
      , disabled: disableForm
      , onChange: capture self targetValue \value ->
          UpdateUsername $ maybe "" identity value
      }
      , error
        # maybe mempty case _ of
          UsernameTaken username' ->
            Alert.alert
              { "type": "error"
              , message: "The username \"" <> username' <> "\" is already taken."
              }
    ]

chatScreen
  :: forall props state
   . Self props state ReactComponentInstance
  -> { user :: User
     , currentRoom :: Room
     , rooms :: Set Room
     , messages :: Array Server.Chat
     , newMessageText :: String
     , newRoomName :: String
     }
  -> JSX
chatScreen self { currentRoom, rooms, newRoomName, messages, newMessageText } =
  let
    roomTitle = append "# " $ unwrapToString currentRoom
  in
    Layout.layout { style: css { height: "100vh" } }
      [ Layout.sider
        { theme: "light" }
        [ Input.input
          { placeholder: "Create a new room"
          , style: css { margin: "1em 1em", width: "auto" }
          , value: newRoomName
          , onChange: capture self targetValue \value ->
              UpdateNewRoomName $ maybe "" identity value
          , onPressEnter: capture self targetValue \value ->
              CreateRoom $ maybe "" identity value
          }
        , h3
          { style: css { marginLeft: "1em" }
          , children: [ text "Rooms" ]
          }
        , Menu.menu
          { selectedKeys: [ unwrapToString currentRoom ]
          , onClick: \{ key } -> unsafePerformEffect do
              -- AntDesign event handler, so non-standard.
              NonEmptyString.fromString key
                # map Room
                # traverse_ \room -> React.send self (SelectRoom room)
          }
          $ Array.fromFoldable rooms
            <#> \room ->
              Menu.item
                { key: unwrapToString room
                }
                [ text $ "# " <> unwrapToString room ]
        ]
      , Layout.layout {}
        [ Layout.header {}
          [ h2
            { style: css { color: "white" }
            , children: [ text roomTitle ]
            }
          ]
        , Layout.content {} $ renderMessages currentRoom messages
        , Layout.footer {}
          [ Input.input
            { "type": "textarea"
            , placeholder: "Enter your message"
            , size: "large"
            , value: newMessageText
            , onChange: capture self targetValue \value ->
                UpdateNewMessageText $ maybe "" identity value 
            , onPressEnter: capture self targetValue \value ->
                SendMessage $ maybe "" identity value
            }
          ]
        ]
      ]

renderMessages :: Room -> Array Server.Chat -> Array JSX
renderMessages currentRoom messages =
  let
    filteredMessages = messages # Array.filter case _ of
      Message _ room _ _ -> room == currentRoom
      _ -> true

    formatTime time =
      -- this is fragile, but convenient for this project
      (\time' -> "[" <> time' <> "] ") $ String.take 8 $ JSDate.toTimeString $ unwrap time
  in
    filteredMessages
      <#> case _ of
        UserJoined user time ->
          div_ [ text $ formatTime time <> unwrapToString user <> " has joined" ]
        UserLeft user time ->
          div_ [ text $ formatTime time <> unwrapToString user <> " has left" ]
        RoomCreated user room time ->
          div_ [ text $ formatTime time <> unwrapToString user <> " has created the room " <> unwrapToString room ]
        Message user room messageText time ->
          div_ [ text $ formatTime time <> unwrapToString user <> ": " <> unwrapToString messageText ]
      <#> \child -> div { style: css { margin: "1em" }, children: [ child ] }