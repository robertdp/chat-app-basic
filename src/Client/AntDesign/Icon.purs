module Client.AntDesign.Icon where

import React.Basic (JSX, ReactComponent, element)


-- | Totally unsafe, but convenient. Ant Design has a pretty poor API to try and model.
foreign import _icon :: forall props. ReactComponent { | props }

icon :: forall props. { | props } -> JSX
icon = element _icon