{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Factorio.Helpers
    ( decompress
    , distance
    , closestTo
    , updatePixel
    , forwards
    , backwards
    ) where

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.Zlib.Internal (DecompressError)
import Codec.Compression.Zlib.Internal qualified as ZLib.Internal
import Codec.Picture (PixelRGB8, Pixel)
import Codec.Picture qualified as Picture
import Codec.Picture.Types (MutableImage)
import Control.Arrow ((>>>))
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Word (Word8)

-- | Alternate version of 'ZLib.decompress' which explicitly returns
-- any errors.
decompress :: Lazy.ByteString -> Either DecompressError Lazy.ByteString
decompress bytes =
    fold combine atEnd raiseErr stream bytes
        <&> Builder.toLazyByteString
  where
    fold = ZLib.Internal.foldDecompressStreamWithInput
    combine chunk = \case
        Left err -> Left err
        Right builder -> Right (Builder.byteString chunk <> builder)
    atEnd _remaining = Right mempty
    raiseErr = Left
    stream = ZLib.Internal.decompressST
        ZLib.Internal.zlibFormat
        ZLib.defaultDecompressParams

distance :: PixelRGB8 -> PixelRGB8 -> Float
distance (Picture.PixelRGB8 r1 g1 b1) (Picture.PixelRGB8 r2 g2 b2) =
    sqrt $ sum
        [ square (w2f r2 - w2f r1)
        , square (w2f g2 - w2f g1)
        , square (w2f b2 - w2f b1) ]
  where
    square x = x * x
    -- Very important not to calculate with Word8 - if you do, you
    -- may easily overflow and get nonsense results. So our first step
    -- is to convert to Float.
    w2f = fromIntegral @Word8 @Float

closestTo :: (a -> PixelRGB8) -> [a] -> PixelRGB8 -> a
closestTo toCol options col =
    let measure = toCol >>> distance col
    in  List.minimumBy (comparing measure) options

updatePixel
    :: (PrimMonad m, Pixel a)
    => MutableImage (PrimState m) a
    -> Int -> Int
    -> (a -> a)
    -> m ()
updatePixel image x y fn = do
    val <- Picture.readPixel image x y
    Picture.writePixel image x y $ fn val

-- | Assuming the input map is complete (that is, it contains in it
-- every possible key - so a lookup will always succeed), this function
-- returns the value for a given key.
--
-- If this assumption does not hold, will throw an error.
forwards :: Ord k => Map k v -> k -> v
forwards map' key = fromJust $ Map.lookup key map'

-- | Attempts to find the (first) key associated with a given value.
backwards :: Ord v => Map k v -> v -> Maybe k
backwards map' val = Map.lookup val (invert map')

invert :: Ord v => Map k v -> Map v k
invert = Map.toList >>> fmap swap >>> Map.fromList
  where
    swap (x, y) = (y, x)
