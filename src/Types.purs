module Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)
import Foreign (F, Foreign, ForeignError(..))
import Foreign.Class (class Decode, class Encode, decode, encode)


newtype Time = Time JSDate

derive instance eqTime :: Eq Time
derive instance ordTime :: Ord Time
derive newtype instance showTime :: Show Time
derive instance newtypeTime :: Newtype Time _

instance encodeTime :: Encode Time where
  encode = encode <<< JSDate.getTime <<< unwrap

instance decodeTime :: Decode Time where
  decode = decode >>> map (JSDate.fromTime >>> wrap)

-- epoch :: DateTime.DateTime
-- epoch = unsafePartial $ fromJust ado
--   year <- toEnum 1970
--   month <- toEnum 1
--   day <- toEnum 1
--   hour <- toEnum 0
--   minute <- toEnum 0
--   second <- toEnum 0
--   millisecond <- toEnum 0
--   in DateTime.DateTime (DateTime.canonicalDate year month day) (DateTime.Time hour minute second millisecond)

newtype User = User NonEmptyString

derive instance eqUser :: Eq User
derive instance ordUser :: Ord User
derive instance newtypeUser :: Newtype User _

instance encodeUser :: Encode User where
  encode = encodeNonEmptyString

instance decodeUser :: Decode User where
  decode = decodeNonEmptyString

newtype Room = Room NonEmptyString

derive instance eqRoom :: Eq Room
derive instance ordRoom :: Ord Room
derive instance newtypeRoom :: Newtype Room _

instance encodeRoom :: Encode Room where
  encode = encodeNonEmptyString

instance decodeRoom :: Decode Room where
  decode = decodeNonEmptyString

newtype MessageText = MessageText NonEmptyString

derive instance newtypeMessageText :: Newtype MessageText _

instance encodeMessageText :: Encode MessageText where
  encode = encodeNonEmptyString

instance decodeMessageText :: Decode MessageText where
  decode = decodeNonEmptyString

encodeNonEmptyString :: forall t. Newtype t NonEmptyString => t -> Foreign
encodeNonEmptyString = encode <<< toString <<< unwrap

decodeNonEmptyString :: forall t. Newtype t NonEmptyString => Foreign -> F t
decodeNonEmptyString = decode >=> fromString >>> maybe (throwError error) (wrap >>> pure)
  where error = pure $ ForeignError "String is empty"

unwrapToString :: forall a. Newtype a NonEmptyString => a -> String
unwrapToString = toString <<< unwrap