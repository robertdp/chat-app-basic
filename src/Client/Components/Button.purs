module Client.Components.Button where

import Prelude
import React.Basic (JSX, ReactComponent, element)
import Record.Unsafe.Union (unsafeUnion)


-- | Totally unsafe, but convenient. Ant Design has a pretty poor API to try and model.
foreign import _button :: forall props. ReactComponent { | props }

button :: forall props. { | props } -> Array JSX -> JSX
button props children = element _button $ unsafeUnion props { children }