{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.Factorio
    ( -- * main interface
      imageToBlueprint
      -- * helper functions
    , imageToJson
    , jsonToBlueprint
    , blueprintToJson
      -- * TileSet class
    , TileSet(..)
      -- * TileSet instances
    , Vanilla(..)
    , VanillaTile(..)
      -- * Errors
    , DecodeError(..)
    ) where

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.Zlib.Internal (DecompressError)
import Codec.Factorio.Helpers (decompress)
import Codec.Picture (Image, PixelRGB16)
import Control.Arrow ((>>>))
import Control.Exception (Exception, throw)
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json.Pretty
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Base64 qualified as Base64
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encode
import qualified Data.Char as Char


data DecodeError
    = ZLibError DecompressError
    | Base64Error Text
    | JsonError Text
    deriving stock (Show, Eq)
    deriving anyclass Exception

-- | This typeclass allows flexibility in the tileset used for approximating
-- pixels - there are some tileset instances provided out-of-the-box, but
-- this can be extended as needed by library users.
class TileSet t where
    type Tile t
    -- colour ?
    -- nearest ?

data Vanilla = MkVanilla
data VanillaTile = Stone | Concrete | HazardConcrete | Refined | HazardRefined

instance TileSet Vanilla where
    type Tile Vanilla = VanillaTile

-- | Main wrapper function - turns a binary image into a blueprint
-- string suitable for using in-game.
imageToBlueprint :: TileSet t => t -> ByteString -> Text
imageToBlueprint = error "TODO"

-- | Convert an image into a JSON value. Each pixel will be assigned a
-- suitable tile from the given 'TileSet' by approximating its colour value.
imageToJson :: TileSet t => t -> Image PixelRGB16 -> Json.Value
imageToJson = error "TODO"

-- | Internal helper function - converts from JSON to a string, taking
-- a version number to include in the output.
--
-- Version number should be in the range 0-9 inclusive, otherwise this
-- function may throw an error.
jsonToBlueprint :: Int -> Json.Value -> Text
jsonToBlueprint version =
    Json.encode
        >>> ZLib.compressWith params
        >>> Base64.encodeBase64
        >>> Text.Lazy.toStrict
        >>> Text.cons (Char.intToDigit version)
  where
    params = ZLib.defaultCompressParams
        { ZLib.compressMethod = ZLib.deflateMethod
        , ZLib.compressLevel = ZLib.compressionLevel 9 }

-- | Internal helper function - converts from a string to JSON. Inverse
-- to 'jsonToBlueprint':
--
-- prop> \obj -> blueprintToJson (jsonToBlueprint 0 obj) == Right obj
blueprintToJson :: Text -> Either DecodeError Json.Value
blueprintToJson text = do
    let versioned = Text.Lazy.fromStrict text
    let lazy = Text.Lazy.drop 1 versioned
    let base64 = Text.Lazy.Encode.encodeUtf8 lazy
    zipped <- first Base64Error $ Base64.decodeBase64 base64
    bytes <- first ZLibError $ decompress zipped
    first (Text.pack >>> JsonError) $ Json.eitherDecode bytes

-- | Convenience function for testing or interactive use. Tries to decode
-- the given blueprint string. If it succeeds, it pretty-prints the resulting
-- JSON to stdout. Otherwise, it throws the 'DecodeError' encountered as
-- an exception.
printBlueprint :: Text -> IO ()
printBlueprint text = case blueprintToJson text of
    Left err -> throw err
    Right json -> json
        & Json.Pretty.encodePrettyToTextBuilder
        & Builder.toLazyText
        & Text.Lazy.toStrict
        & Text.IO.putStrLn
