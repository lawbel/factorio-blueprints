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
    , quantize
    , ditherFS
    , ditherMAE
    , ditherAtkinson
    , toJson
      -- * Palette class
    , Palette(..)
    , Category(..)
    , Figure(..)
    , setPalette
      -- * Errors
    , DecodeError(..)
      -- * Utilities
    , printBlueprint
    ) where

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.Zlib.Internal (DecompressError)
import Codec.Factorio.Helpers qualified as Help
import Codec.Picture (Image, PixelRGB8)
import Codec.Picture qualified as Picture
import Codec.Picture.Types qualified as Picture.Type
import Control.Arrow ((>>>))
import Control.Exception (Exception, throw)
import Control.Monad (when, unless)
import Control.Monad.ST (runST)
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json.Pretty
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Base64 qualified as Base64
import Data.Char qualified as Char
import Data.Either (partitionEithers)
import Data.Foldable (for_, asum)
import Data.Function ((&))
import Data.Matrix (Matrix)
import Data.Matrix qualified as Matrix
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encode
import Data.Word (Word8)

-- | All possible errors that can occur when decoding a blueprint string.
data DecodeError
    = ZLibError DecompressError
    -- ^ decompression error
    | Base64Error Text
    -- ^ base-64 decoding error
    | JsonError Text
    -- ^ json decoding error
    deriving stock (Eq, Show)
    deriving anyclass Exception

-- | The possible categories of objects that can be used as part of a
-- colour palette.
data Category = Entity | Tile
    deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | A newtype wrapper around an 'Image', that allows tracking a type
-- variable (usually a colour palette) together with the underlying image.
newtype Figure p = MkFigure (Image PixelRGB8)
    deriving stock Eq

-- | This typeclass allows flexibility in the tileset used for approximating
-- pixels - there are some tileset instances provided out-of-the-box, but
-- this can be extended as needed by library users.
class Palette obj where
    name :: obj -> Text
    colour :: obj -> PixelRGB8
    asJson :: obj -> Json.Object
    categorize :: obj -> Category
    nearest :: PixelRGB8 -> obj
    getName :: Text -> Maybe obj
    getColour :: PixelRGB8 -> Maybe obj

instance (Palette a, Palette b) => Palette (Either a b) where
    name = either name name
    colour = either colour colour
    asJson = either asJson asJson
    categorize = either categorize categorize
    nearest col =
        let left = Left $ nearest col
            right = Right $ nearest col
            dist = colour >>> Help.distance col
        in  if dist left < dist right then left else right
    getName str = asum
        [ Left <$> getName str
        , Right <$> getName str ]
    getColour col = asum
        [ Left <$> getColour col
        , Right <$> getColour col ]

-- | Wrap up an 'Image' into a 'Figure' - this is just a newtype wrapper
-- (so has no runtime impact), but it allows us to keep track of the colour
-- palette together with the source image.
setPalette :: Palette p => Image PixelRGB8 -> Figure p
setPalette = MkFigure

-- | Perform colour-quantization on the given image, using the given
-- colour 'Palette'.
quantize :: Palette p => Figure p -> Figure p
quantize (MkFigure @p image) = MkFigure $ Picture.pixelMap quant image
  where
    quant = nearest @p >>> colour

-- | Implements Floyd-Steinberg dithering on the image.
ditherFS :: Palette p => Figure p -> Figure p
ditherFS = ditherError (2, 1) $ fmap (/ 16) $ Matrix.fromLists
    [ [ 0, 0, 7 ]
    , [ 3, 5, 1 ] ]

-- | Implements Minimized Average Error dithering. Similar to 'ditherFS',
-- but works with a larger kernel. As such, it should be slower to run but
-- yield less visual artifacts.
ditherMAE :: Palette p => Figure p -> Figure p
ditherMAE = ditherError (3, 1) $ fmap (/ 48) $ Matrix.fromLists
    [ [ 0, 0, 0, 7, 5 ]
    , [ 3, 5, 7, 5, 3 ]
    , [ 1, 3, 5, 3, 1 ] ]

-- | Implements Atkinson dithering. Similar to 'ditherMAE', but only
-- diffuses 3/4 of the error. This improves locality of the dither, at cost
-- of worse performance near extreme black and extreme white.
ditherAtkinson :: Palette p => Figure p -> Figure p
ditherAtkinson = ditherError (3, 1) $ fmap (/ 8) $ Matrix.fromLists
    [ [ 0, 0, 0, 1, 1 ]
    , [ 0, 1, 1, 1, 0 ]
    , [ 0, 0, 1, 0, 0] ]

-- | Implements error-diffusion dithering, using the given matrix as a kernel
-- which has the given position as the pixel being processed.
--
-- Note: the position in the matrix is 1-based, so e.g. the top-left
-- would be (1, 1).
ditherError :: Palette p => (Int, Int) -> Matrix Float -> Figure p -> Figure p
ditherError center kernel (MkFigure @p image) = MkFigure $ runST $ do
    canvas <- Picture.Type.thawImage image
    let update (x, y) fn = when (valid x y) $ Help.updatePixel canvas x y fn
    for_ [0 .. height - 1] $ \y -> do
        for_ [0 .. width - 1] $ \x -> do
            old <- Picture.Type.readPixel canvas x y
            let new = colour $ nearest @p old
            let err = Help.diffPixel old new
            Picture.Type.writePixel canvas x y new
            for_ (Help.positionsAfter center kernel) $ \(col, row) -> do
                let frac = Matrix.getElem row col kernel
                let adjust = Help.scaleTriple frac err
                let offset = Help.diffPair (col, row) center
                let pos = Help.addPair (x, y) offset
                unless (frac == 0) $ update pos $ Help.addPixel adjust
    Picture.Type.unsafeFreezeImage canvas
  where
    width = Picture.imageWidth image
    height = Picture.imageHeight image
    valid x y = (0 <= x) && (x < width) && (0 <= y) && (y < height)

-- | Take an image (perhaps after various processing steps earlier) and output
-- it in the JSON format used for blueprints in-game.
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

-- | Helper function for use in 'toJson'.
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
    bytes <- first ZLibError $ Help.decompress zipped
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
