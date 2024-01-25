{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Main
    ( main
    ) where

import Codec.Factorio (Figure, Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Base qualified as Base
import Codec.Factorio.Dectorio qualified as Dectorio
import Codec.Factorio.Krastorio qualified as Krastorio
import Codec.Picture (Image, PixelRGB8)
import Codec.Picture qualified as Picture
import Codec.Picture.Extra (scaleBilinear)
import Codec.Picture.Types qualified as Picture.Type
import Control.Applicative (asum)
import Control.Arrow ((>>>))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json.Pretty
import Data.ByteString (ByteString)
import Data.ByteString qualified as Bytes
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Proxy (Proxy(..))
import Data.String.Interpolate (i)
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt
import System.Exit (die)
import System.FilePath qualified as FilePath
import Text.Read (readEither)

data Format = Str | Json
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Set
    = TileBase  -- ^ base game, tiles only
    | AllBase   -- ^ base game, everything (tiles & entities)
    | TileKras  -- ^ krastorio, tiles only
    | AllKras   -- ^ krastorio, everything (tiles & entities)
    | TileDect  -- ^ dectorio, tiles
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Dither = FS | MAE | Atkinson | NoDither
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Resize = Width Int | Height Int | Scale Float
    deriving (Eq, Ord, Read, Show)

data Args = MkArgs
    { image :: FilePath
    , set :: Set
    , dither :: Dither
    , output :: Maybe Format
    , preview :: Maybe FilePath
    , resize :: Maybe Resize }
    deriving (Eq, Ord, Show)

parseArgs :: Parser Args
parseArgs = do
    image <- Opt.strOption $ mconcat
        [ Opt.long "image"
        , Opt.short 'i'
        , Opt.metavar "FILE"
        , Opt.help "the input image to use" ]
    preview <- Opt.option (Just <$> Opt.str) $ mconcat
        [ Opt.long "preview"
        , Opt.short 'p'
        , Opt.metavar "FILE"
        , Opt.help "output a preview of the blueprint to the given file"
        , Opt.value Nothing ]
    set <- Opt.option (Opt.eitherReader readSet) $ mconcat
        [ Opt.long "set"
        , Opt.metavar "SET"
        , Opt.help
            "the tileset/palette to use - should be one of \
            \{tile-base, tile-kras, tile-dect, all-base, all-kras}"
        , Opt.value AllBase ]
    let readFormat = readTitle >>> fmap Just
    output <- Opt.option (Opt.eitherReader readFormat) $ mconcat
        [ Opt.long "output"
        , Opt.short 'o'
        , Opt.metavar "FORMAT"
        , Opt.help
            "print the blueprint in the given format - one of {str, json}"
        , Opt.value Nothing ]
    dither <- Opt.option (Opt.eitherReader readDither) $ mconcat
        [ Opt.long "dither"
        , Opt.short 'd'
        , Opt.metavar "METHOD"
        , Opt.help
            "how (if at all) to dither the preview image - one of \
            \{fs, mae, atkin, none}"
        , Opt.value NoDither
        , Opt.showDefaultWith (const "none") ]
    resize <- asum
        [ fmap (Width >>> Just) $ Opt.option Opt.auto $ mconcat
            [ Opt.long "width"
            , Opt.metavar "INT"
            , Opt.help "target width (in pixels) to resize the image to" ]
        , fmap (Height >>> Just) $ Opt.option Opt.auto $ mconcat
            [ Opt.long "height"
            , Opt.metavar "INT"
            , Opt.help "target height (in pixels) to resize the image to" ]
        , fmap (Scale >>> Just) $ Opt.option Opt.auto $ mconcat
            [ Opt.long "scale"
            , Opt.metavar "FLOAT"
            , Opt.help
                "ratio to scale the image to - scale=1 means preserve \
                \size, scale=0.5 means half scale, etc." ]
        , pure Nothing ]
    pure $ MkArgs{image, set, output, preview, dither, resize}

readSet :: String -> Either String Set
readSet = \case
    "tile-base" -> Right TileBase
    "tile-dect" -> Right TileDect
    "tile-kras" -> Right TileKras
    "all-base"  -> Right AllBase
    "all-kras"  -> Right AllKras
    txt         -> Left [i|'#{txt}' is not a valid set|]

readDither :: String -> Either String Dither
readDither = \case
    "fs"    -> Right FS
    "mae"   -> Right MAE
    "atkin" -> Right Atkinson
    "none"  -> Right NoDither
    txt     -> Left [i|'#{txt}' is not a valid dithering method|]

titleCase :: String -> String
titleCase = fmap Char.toLower >>> upperFirst
  where
    upperFirst = \case
        c:cs -> Char.toUpper c : cs
        [] -> []

readTitle :: Read a => String -> Either String a
readTitle = titleCase >>> readEither

main :: IO ()
main = Opt.execParser opts >>= run
  where
    opts = Opt.info (Opt.helper <*> parseArgs) $ mconcat
        [ Opt.fullDesc
        , Opt.progDesc "Generate a Factorio Blueprint from a given image" ]

run :: Args -> IO ()
run MkArgs{image, set, output, preview, dither, resize} = do
    bytes <- Bytes.readFile image
    case Picture.decodeImage bytes of
        Left err -> die err
        Right file -> withSet set $ \(Proxy @p) -> do
            let sized = maybe id applyResize resize $ Picture.convertRGB8 file
            let dithered = applyDither dither $ Factorio.setPalette @p sized
            let json = Factorio.toJson dithered
            for_ output $ printAs json
            for_ preview $ writePreview dithered

applyDither :: Palette p => Dither -> Figure p -> Figure p
applyDither = \case
    FS       -> Factorio.ditherFS
    MAE      -> Factorio.ditherMAE
    Atkinson -> Factorio.ditherAtkinson
    NoDither -> Factorio.quantize

applyResize :: Resize -> Image PixelRGB8 -> Image PixelRGB8
applyResize resize image = case resize of
    Width widthNew ->
        let scale = i2f widthNew / i2f widthOld
            heightNew = f2i (scale * i2f heightOld)
        in  scaleBilinear widthNew heightNew image
    Height heightNew ->
        let scale = i2f heightNew / i2f heightOld
            widthNew = f2i (scale * i2f widthOld)
        in  scaleBilinear widthNew heightNew image
    Scale scale ->
        let widthNew = f2i (scale * i2f widthOld)
            heightNew = f2i (scale * i2f heightOld)
        in  scaleBilinear widthNew heightNew image
  where
    widthOld = Picture.imageWidth image
    heightOld = Picture.imageHeight image
    i2f = fromIntegral @Int @Float
    f2i = round @Float @Int

withSet :: Set -> (forall p. Palette p => Proxy p -> a) -> a
withSet set cont = case set of
    AllBase  -> cont $ Proxy @Base.All
    AllKras  -> cont $ Proxy @Krastorio.All
    TileBase -> cont $ Proxy @Base.Tile
    TileKras -> cont $ Proxy @Krastorio.AllTile
    TileDect -> cont $ Proxy @Dectorio.AllTile

printAs :: Json.Value -> Format -> IO ()
printAs json = \case
    Str -> Text.IO.putStrLn $ Factorio.jsonToBlueprint 0 json
    Json -> prettyPrintJson json

prettyPrintJson :: Json.Value -> IO ()
prettyPrintJson =
    Json.Pretty.encodePrettyToTextBuilder
        >>> Builder.toLazyText
        >>> Text.Lazy.toStrict
        >>> Text.IO.putStrLn

-- | Can fail if the given 'FilePath' has an unrecognised extension.
writePreview :: Figure p -> FilePath -> IO ()
writePreview (Factorio.MkFigure image) path = do
    let extension = FilePath.takeExtension path
    let bytes = encodeExtension extension image
    Bytes.writeFile path bytes

encodeExtension :: String -> Image PixelRGB8 -> ByteString
encodeExtension = \case
    ".bmp"  -> Picture.encodeBitmap >>> Bytes.toStrict
    ".png"  -> Picture.encodePng >>> Bytes.toStrict
    ".jpg"  -> encodeJpeg >>> Bytes.toStrict
    ".jpeg" -> encodeJpeg >>> Bytes.toStrict
    ext     -> error [i|unsupported image type '#{ext}'|]
  where
    encodeJpeg = Picture.Type.convertImage >>> Picture.encodeJpeg
