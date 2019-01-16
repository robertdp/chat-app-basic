module Types where


import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)
import Foreign (F, Foreign, ForeignError(..))
import Foreign.Class (class Decode, class Encode, decode, encode)


newtype User = User NonEmptyString
derive instance newtypeUser :: Newtype User _
derive instance eqUser :: Eq User
derive instance ordUser :: Ord User

instance encodeUser :: Encode User where
  encode = encodeNonEmptyString

instance decodeUser :: Decode User where
  decode = decodeNonEmptyString

newtype Room = Room NonEmptyString
derive instance newtypeRoom :: Newtype Room _
derive instance eqRoom :: Eq Room
derive instance ordRoom :: Ord Room

instance encodeRoom :: Encode Room where
  encode = encodeNonEmptyString

instance decodeRoom :: Decode Room where
  decode = decodeNonEmptyString

encodeNonEmptyString :: forall t. Newtype t NonEmptyString => t -> Foreign
encodeNonEmptyString = encode <<< toString <<< unwrap

decodeNonEmptyString :: forall t. Newtype t NonEmptyString => Foreign → F t
decodeNonEmptyString = decode >=> fromString >>> maybe (throwError $ pure $ ForeignError "String is empty") (pure <<< wrap)
