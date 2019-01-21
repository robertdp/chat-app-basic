module Client.AntDesign.Alert where

import React.Basic (JSX, ReactComponent, element)


-- | Totally unsafe, but convenient. Ant Design has a pretty poor API to try and model.
foreign import _alert :: forall props. ReactComponent { | props }

alert :: forall props. { | props } -> JSX
alert = element _alert