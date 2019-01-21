module Client.AntDesign.Layout where

import Prelude

import React.Basic (JSX, ReactComponent, element)
import Record.Unsafe.Union (unsafeUnion)


-- | Totally unsafe, but convenient. Ant Design has a pretty poor API to try and model.
foreign import _layout :: forall props. ReactComponent { | props }
foreign import _header :: forall props. ReactComponent { | props }
foreign import _sider :: forall props. ReactComponent { | props }
foreign import _content :: forall props. ReactComponent { | props }
foreign import _footer :: forall props. ReactComponent { | props }

layout :: forall props. { | props } -> Array JSX -> JSX
layout props children = element _layout $ unsafeUnion props { children }

header :: forall props. { | props } -> Array JSX -> JSX
header props children = element _header $ unsafeUnion props { children }

sider :: forall props. { | props } -> Array JSX -> JSX
sider props children = element _sider $ unsafeUnion props { children }

content :: forall props. { | props } -> Array JSX -> JSX
content props children = element _content $ unsafeUnion props { children }

footer :: forall props. { | props } -> Array JSX -> JSX
footer props children = element _footer $ unsafeUnion props { children }
