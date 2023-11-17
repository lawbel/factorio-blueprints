{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Codec.Factorio
    ( -- * main interface
      imageToBlueprint
      -- * helper functions
    , imageToJson
    , jsonToBlueprint
    , blueprintToJson
      -- * Palette class
    , Palette(..)
    , Category(..)
      -- * Errors
    , DecodeError(..)
    ) where

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.Zlib.Internal (DecompressError)
import Codec.Factorio.Helpers (decompress)
import Codec.Picture (Image, PixelRGB8)
import Codec.Picture qualified as Picture
import Control.Arrow ((>>>))
import Control.Exception (Exception, throw)
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json.Pretty
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Base64 qualified as Base64
import Data.Char qualified as Char
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encode

data DecodeError
    = ZLibError DecompressError
    | Base64Error Text
    | JsonError Text
    deriving stock (Eq, Show)
    deriving anyclass Exception

data Category = Entity | Tile
    deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | This typeclass allows flexibility in the tileset used for approximating
-- pixels - there are some tileset instances provided out-of-the-box, but
-- this can be extended as needed by library users.
class Palette p where
    -- | This type family is injective, in order to not force the user into
    -- providing Proxy values.
    type Object p = r | r -> p
    name :: Object p -> Text
    colour :: Object p -> PixelRGB8
    toJson :: Object p -> Json.Object
    categorize :: Object p -> Category
    nearest :: PixelRGB8 -> Object p

-- | Main wrapper function - turns a binary image into a blueprint
-- string suitable for using in-game.
imageToBlueprint :: Palette p => p -> ByteString -> Text
imageToBlueprint = error "TODO"

-- | Convert an image into a JSON value. Each pixel will be assigned a
-- suitable tile from the given 'Palette' by approximating its colour value.
imageToJson :: Palette p => Proxy p -> Image PixelRGB8 -> Json.Object
imageToJson (Proxy @p) image =
    Json.Map.fromList
        [ "entities" .= do
            (num, (entity, pos)) <- zip [1..] entities
            let position = Json.Map.singleton "position" pos
            let number = Json.Map.singleton "entity_number" $ intToNumber num
            pure $ position <> number <> entity
        , "tiles" .= do
            (tile, pos) <- tiles
            let position = Json.Map.singleton "position" pos
            pure $ position <> tile ]
  where
    intToFloat = fromIntegral @Int @Float
    intToNumber = fromIntegral @Int >>> Json.Number
    (entities, tiles) = partitionEithers $ do
        x <- [0 .. Picture.imageWidth image - 1]
        y <- [0 .. Picture.imageHeight image - 1]
        let pixel = Picture.pixelAt image x y
        let (cat, json) = pixelToJson (Proxy @p) pixel
        let pos = Json.object ["x" .= intToFloat x, "y" .= intToFloat y]
        pure $ case cat of
            Entity -> Left (json, pos)
            Tile -> Right (json, pos)

pixelToJson :: Palette p => Proxy p -> PixelRGB8 -> (Category, Json.Object)
pixelToJson (Proxy @p) col = (categorize @p object, toJson object)
  where
    object = nearest col

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
