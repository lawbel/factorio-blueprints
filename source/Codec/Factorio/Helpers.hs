{-# LANGUAGE RankNTypes #-}

module Codec.Factorio.Helpers
    ( decompress
    , distance
    , closestTo
    ) where

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.Zlib.Internal (DecompressError)
import Codec.Compression.Zlib.Internal qualified as ZLib.Internal
import Codec.Picture (PixelRGB8)
import Codec.Picture qualified as Picture
import Control.Arrow ((>>>))
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Functor ((<&>))
import Data.List qualified as List
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

distance :: PixelRGB8 -> PixelRGB8 -> Word8
distance (Picture.PixelRGB8 r1 g1 b1) (Picture.PixelRGB8 r2 g2 b2) =
    square (r2 - r1) + square (g2 - g1) + square (b2 - b1)
  where
    square x = x * x

closestTo :: (a -> PixelRGB8) -> [a] -> PixelRGB8 -> a
closestTo toCol options col =
    let measure = toCol >>> distance col
    in  List.minimumBy (comparing measure) options
