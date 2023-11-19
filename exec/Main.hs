{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import Codec.Factorio (Figure, Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Vanilla qualified as Vanilla
import Codec.Picture (Image, PixelRGB8)
import Codec.Picture qualified as Picture
import Codec.Picture.Extra (scaleBilinear)
import Control.Applicative (asum)
import Control.Arrow ((>>>))
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json.Pretty
import Data.Bifunctor (first)
import Data.ByteString qualified as Bytes
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Proxy (Proxy(..))
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt
import System.Exit (die)
import System.File.OsPath qualified as File.OsPath
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import Text.Read (readEither)

data Format = Str | Json
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Set = Floor | All
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Resize = Width Int | Height Int | Scale Float
    deriving (Eq, Ord, Read, Show)

data Args = MkArgs
    { image :: OsPath
    , set :: Set
    , dither :: Bool
    , output :: Maybe Format
    , preview :: Maybe OsPath
    , resize :: Maybe Resize }
    deriving (Eq, Ord, Show)

parseArgs :: Parser Args
parseArgs = do
    image <- Opt.option (Opt.eitherReader encodeOsPath) $ mconcat
        [ Opt.long "image"
        , Opt.short 'i'
        , Opt.metavar "FILE"
        , Opt.help "the input image to use" ]
    let encodeJust = encodeOsPath >>> fmap Just
    preview <- Opt.option (Opt.eitherReader encodeJust) $ mconcat
        [ Opt.long "preview"
        , Opt.short 'p'
        , Opt.metavar "FILE"
        , Opt.help
            "output a preview of the blueprint to the given file, in \
            \PNG format"
        , Opt.value Nothing ]
    set <- Opt.option (Opt.eitherReader readTitle) $ mconcat
        [ Opt.long "set"
        , Opt.metavar "SET"
        , Opt.help
            "the tileset/palette to use - should be one of {flooring, all}"
        , Opt.value All ]
    let readJust = readTitle >>> fmap Just
    output <- Opt.option (Opt.eitherReader readJust) $ mconcat
        [ Opt.long "output"
        , Opt.short 'o'
        , Opt.metavar "FORMAT"
        , Opt.help
            "print the blueprint in the given format - one of {str, json}"
        , Opt.value Nothing ]
    dither <- asum
        [ Opt.flag' True $ mconcat
            [ Opt.long "dither"
            , Opt.help "enable dithering (the default)" ]
        , Opt.flag' False $ mconcat
            [ Opt.long "no-dither"
            , Opt.help "disable dithering" ]
        , pure True ]
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

encodeOsPath :: String -> Either String OsPath
encodeOsPath = OsPath.encodeUtf >>> first show

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
    bytes <- File.OsPath.readFile' image
    case Picture.decodeImage bytes of
        Left err -> die err
        Right file -> withSet set $ \(Proxy @p) -> do
            let sized = maybe id applyResize resize $ Picture.convertRGB8 file
            let process = if dither then Factorio.dither else Factorio.quantize
            let processed = process $ Factorio.setPalette @p sized
            let json = Factorio.toJson processed
            for_ output $ printAs json
            for_ preview $ writePreview processed

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
    Floor -> cont $ Proxy @Vanilla.Flooring
    All -> cont $ Proxy @Vanilla.All

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

writePreview :: Figure p -> OsPath -> IO ()
writePreview (Factorio.MkFigure image) path = do
    let bytes = Bytes.toStrict $ Picture.encodePng image
    File.OsPath.writeFile' path bytes
