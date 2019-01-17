module Client.Components.Input where

import React.Basic (JSX, ReactComponent, element)


-- | Totally unsafe, but convenient. Ant Design has a pretty poor API to try and model.
foreign import _input :: forall props. ReactComponent { | props }

input :: forall props. { | props } -> JSX
input = element _input