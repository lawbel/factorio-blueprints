{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import Codec.Factorio (Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Vanilla qualified as Vanilla
import Codec.Picture (Image, PixelRGB8)
import Codec.Picture qualified as Picture
import Control.Arrow ((>>>))
import Data.Bifunctor (first)
import Data.Proxy (Proxy(..))
import Data.Text.IO qualified as Text.IO
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt
import System.Exit (die)
import System.File.OsPath qualified as File.OsPath
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

data Action = PrintBlueprintStr | OutputImagePreview
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data Choice = Flooring | All
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data Args = MkArgs
    { image :: OsPath
    , choice :: Choice
    , action :: Action }
    deriving (Eq, Ord, Show)

withChoice :: Choice -> (forall p. Palette p => Proxy p -> a) -> a
withChoice choice cont = case choice of
    Flooring -> cont $ Proxy @Vanilla.Flooring
    All -> cont $ Proxy @Vanilla.All

encodeOsPath :: String -> Either String OsPath
encodeOsPath = OsPath.encodeUtf >>> first show

parseArgs :: Parser Args
parseArgs = do
    image <- Opt.option (Opt.eitherReader encodeOsPath) $ mconcat
        [ Opt.long "image"
        , Opt.short 'i'
        , Opt.metavar "FILE" ]
    pure $ MkArgs{image}

main :: IO ()
main = Opt.execParser opts >>= run
  where
    opts = Opt.info (Opt.helper <*> parseArgs) $ mconcat
        [ Opt.fullDesc
        , Opt.progDesc "Generate a Factorio Blueprint from a given image" ]

run :: Args -> IO ()
run MkArgs{image, action, choice} = do
    bytes <- File.OsPath.readFile' image
    case Picture.decodeImage bytes of
        Left err -> die err
        Right file -> do
            let rgb = Picture.convertRGB8 file
            withChoice choice $ \proxy -> runAction proxy rgb action

runAction :: Palette p => Proxy p -> Image PixelRGB8 -> Action -> IO ()
runAction proxy image = \case
    PrintBlueprintStr -> do
        let blueprint = Factorio.imageToBlueprint proxy image
        Text.IO.putStrLn blueprint
    OutputImagePreview -> do
        putStrLn "TODO"
