{-# LANGUAGE RankNTypes #-}

module Codec.Factorio.Helpers
    ( decompress
    ) where

import Codec.Compression.Zlib qualified as ZLib
import Codec.Compression.Zlib.Internal qualified as ZLib.Internal
import Codec.Compression.Zlib.Internal (DecompressError)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Functor ((<&>))


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
