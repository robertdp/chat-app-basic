module Client.AntDesign.Modal where

import Prelude
import React.Basic (JSX, ReactComponent, element)
import Record.Unsafe.Union (unsafeUnion)


-- | Totally unsafe, but convenient. Ant Design has a pretty poor API to try and model.
foreign import _modal :: forall props. ReactComponent { | props }

modal :: forall props. { | props } -> Array JSX -> JSX
modal props children = element _modal $ unsafeUnion props { children }