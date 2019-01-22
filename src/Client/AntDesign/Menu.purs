module Client.AntDesign.Menu where

import Prelude
import React.Basic (JSX, ReactComponent, element)
import Record.Unsafe.Union (unsafeUnion)


-- | Totally unsafe, but convenient. Ant Design has a pretty poor API to try and model.
foreign import _menu :: forall props. ReactComponent { | props }
foreign import _item :: forall props. ReactComponent { | props }
foreign import _divider :: forall props. ReactComponent { | props }

menu :: forall props. { | props } -> Array JSX -> JSX
menu props children = element _menu $ unsafeUnion props { children }

item :: forall props. { | props } -> Array JSX -> JSX
item props children = element _item $ unsafeUnion props { children }

divider :: JSX
divider = element _divider {}