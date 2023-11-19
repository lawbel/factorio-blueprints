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
import Codec.Picture qualified as Picture
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
import Control.Applicative (asum)

data Format = Str | Json
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Set = Flooring | All
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Args = MkArgs
    { image :: OsPath
    , set :: Set
    , dither :: Bool
    , output :: Maybe Format
    , preview :: Maybe OsPath }
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
        , Opt.short 's'
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
    pure $ MkArgs{image, set, output, preview, dither}

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
run MkArgs{image, set, output, preview, dither} = do
    bytes <- File.OsPath.readFile' image
    case Picture.decodeImage bytes of
        Left err -> die err
        Right file -> withSet set $ \(Proxy @p) -> do
            let rgb = Picture.convertRGB8 file
            let process = if dither then Factorio.dither else Factorio.quantize
            let processed = process $ Factorio.setPalette @p rgb
            let json = Factorio.toJson processed
            for_ output $ printAs json
            for_ preview $ writePreview processed

withSet :: Set -> (forall p. Palette p => Proxy p -> a) -> a
withSet set cont = case set of
    Flooring -> cont $ Proxy @Vanilla.Flooring
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
