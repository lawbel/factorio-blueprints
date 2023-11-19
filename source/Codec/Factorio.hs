{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Codec.Factorio
    ( -- * main interface
      jsonToBlueprint
    , blueprintToJson
    , setPalette
    , quantize
    , dither
    , toJson
      -- * Palette class
    , Palette(..)
    , Category(..)
    , Figure(..)
      -- * Errors
    , DecodeError(..)
      -- * Utilities
    , printBlueprint
    ) where

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.Zlib.Internal (DecompressError)
import Codec.Factorio.Helpers (decompress, updatePixel)
import Codec.Picture (Image, PixelRGB8)
import Codec.Picture qualified as Picture
import Codec.Picture.Types qualified as Picture.Type
import Control.Arrow ((>>>))
import Control.Exception (Exception, throw)
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json.Pretty
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Base64 qualified as Base64
import Data.Char qualified as Char
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encode
import Data.Word (Word8)

data DecodeError
    = ZLibError DecompressError
    | Base64Error Text
    | JsonError Text
    deriving stock (Eq, Show)
    deriving anyclass Exception

data Category = Entity | Tile
    deriving stock (Bounded, Enum, Eq, Ord, Show)

newtype Figure p = MkFigure (Image PixelRGB8)
    deriving stock Eq

-- | This typeclass allows flexibility in the tileset used for approximating
-- pixels - there are some tileset instances provided out-of-the-box, but
-- this can be extended as needed by library users.
class Palette p where
    -- | This type family is injective, in order to not force the user into
    -- providing Proxy values.
    type Object p = r | r -> p
    name :: Object p -> Text
    colour :: Object p -> PixelRGB8
    asJson :: Object p -> Json.Object
    categorize :: Object p -> Category
    nearest :: PixelRGB8 -> Object p
    getName :: Text -> Maybe (Object p)
    getColour :: PixelRGB8 -> Maybe (Object p)

setPalette :: Palette p => Image PixelRGB8 -> Figure p
setPalette = MkFigure

quantize :: Palette p => Figure p -> Figure p
quantize (MkFigure @p image) = MkFigure $ Picture.pixelMap quant image
  where
    quant = nearest @p >>> colour

dither :: Palette p => Figure p -> Figure p
dither (MkFigure @p image) = MkFigure $ runST $ do
    canvas <- Picture.Type.thawImage image
    let update x y fn = when (inBounds x y) $ updatePixel canvas x y fn
    for_ [0 .. height - 1] $ \y -> do
        for_ [0 .. width - 1] $ \x -> do
            oldPixel <- Picture.Type.readPixel canvas x y
            let newPixel = colour $ nearest @p oldPixel
            Picture.Type.writePixel canvas x y newPixel
            let err = difference oldPixel newPixel
            update (x + 1) y $ add $ scale (7/16) err
            update (x - 1) (y + 1) $ add $ scale (3/16) err
            update x (y + 1) $ add $ scale (5/16) err
            update (x + 1) (y + 1) $ add $ scale (1/16) err
    Picture.Type.unsafeFreezeImage canvas
  where
    width = Picture.imageWidth image
    height = Picture.imageHeight image
    inBounds x y = (0 <= x) && (x < width) && (0 <= y) && (y < height)

difference :: PixelRGB8 -> PixelRGB8 -> (Int, Int, Int)
difference (Picture.PixelRGB8 r1 g1 b1) (Picture.PixelRGB8 r2 g2 b2) =
    ( w2i r1 - w2i r2
    , w2i g1 - w2i g2
    , w2i b1 - w2i b2 )
  where
    w2i = fromIntegral @Word8 @Int

scale :: Float -> (Int, Int, Int) -> (Int, Int, Int)
scale factor (x, y, z) = (apply x, apply y, apply z)
  where
    apply = fromIntegral >>> (* factor) >>> round

add :: (Int, Int, Int) -> PixelRGB8 -> PixelRGB8
add (r1, g1, b1) (Picture.PixelRGB8 r2 g2 b2) =
    Picture.PixelRGB8
        (i2w $ r1 + w2i r2)
        (i2w $ g1 + w2i g2)
        (i2w $ b1 + w2i b2)
  where
    w2i = fromIntegral @Word8 @Int
    i2w = min max8 >>> max 0 >>> fromIntegral @Int @Word8
    max8 = w2i $ maxBound @Word8

toJson :: Palette p => Figure p -> Json.Value
toJson (MkFigure @p image) = Json.object
    [ "blueprint" .= Json.object
        [ "entities" .= do
            (num, (entity, pos)) <- zip [1..] entities
            let position = Json.Map.singleton "position" pos
            let number = Json.Map.singleton "entity_number" $ intToNumber num
            pure $ position <> number <> entity
        , "tiles" .= do
            (tile, pos) <- tiles
            let position = Json.Map.singleton "position" pos
            pure $ position <> tile
        , "item" .= Json.String "blueprint" ] ]
  where
    intToFloat = fromIntegral @Int @Float
    intToNumber = fromIntegral @Int >>> Json.Number
    (entities, tiles) = partitionEithers $ do
        x <- [0 .. Picture.imageWidth image - 1]
        y <- [0 .. Picture.imageHeight image - 1]
        let pixel = Picture.pixelAt image x y
        let (cat, json) = pixelInfo (Proxy @p) pixel
        let pos = Json.object ["x" .= intToFloat x, "y" .= intToFloat y]
        pure $ case cat of
            Entity -> Left (json, pos)
            Tile -> Right (json, pos)

pixelInfo :: Palette p => Proxy p -> PixelRGB8 -> (Category, Json.Object)
pixelInfo (Proxy @p) col = (categorize object, asJson object)
  where
    object = case getColour @p col of
        Just obj -> obj
        Nothing -> nearest col

-- | Converts from JSON to a string, taking a version number to include
-- in the output.
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

-- | Converts from a string to JSON. Inverse to 'jsonToBlueprint':
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
