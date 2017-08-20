module Types where

import Prelude

import Data.Either (Either)
import Data.Foreign (Foreign, MultipleErrors, F)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Foreign.Generic.Types (SumEncoding)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax.Request (class Requestable)
import Unsafe.Coerce (unsafeCoerce)

type E a = Either MultipleErrors a

newtype Note = Note
  { property :: String
  , value :: String
  }

instance myNoteShow :: Show Note where
  show (Note r) = r.property <> " " <> r.value

options :: {
  unwrapSingleConstructors :: Boolean
, sumEncoding :: SumEncoding
, unwrapSingleArguments :: Boolean
}
options = defaultOptions {unwrapSingleConstructors = true}

genericDecode' :: forall a rep. Generic a rep => GenericDecode rep => Foreign -> F a
genericDecode' = genericDecode options

genericEncode' :: forall a rep. Generic a rep => GenericEncode rep => a -> Foreign
genericEncode' = genericEncode options

derive instance genericNote :: Generic Note _

instance myNoteDecode :: Decode Note where
  decode = genericDecode'


instance myNoteEncode :: Encode Note where
  encode = genericEncode'

instance requestableNote :: Encode Note => Requestable Note where
  toRequest json = Tuple (Just applicationJSON) (unsafeCoerce $ unsafeStringify (encode json))
