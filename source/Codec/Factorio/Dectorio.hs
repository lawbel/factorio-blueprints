{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Codec.Factorio.Dectorio
    ( -- * All tiles
      AllTile(..)
      -- * New tiles
    , NewTile(..)
    , tileNames
    , tileColours
    ) where

import Codec.Factorio (Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Base qualified as Base
import Codec.Factorio.Helpers (EitherIsBounded, EitherIsEnum)
import Codec.Factorio.Helpers qualified as Help
import Codec.Picture (PixelRGB8)
import Data.Aeson ((.=))
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

$( do
    info <- Help.loadDectInfo
    declType <- Help.declDectType "NewTile" info
    declNameFn <- Help.declDectFn "newTileName" Help.identifier info
    declColourFn <- Help.declDectFn "newTileColour" Help.rgb info
    pure [declType, declNameFn, declColourFn] )

newtype AllTile = MkTile (Either Base.Tile (Either Base.Hazard NewTile))
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype Palette
    deriving Bounded via
        EitherIsBounded Base.Tile (EitherIsBounded Base.Hazard NewTile)
    deriving Enum via
        EitherIsEnum Base.Tile (EitherIsEnum Base.Hazard NewTile)

instance Palette NewTile where
    name = Help.forwards tileNames
    getName = Help.backwards tileNames
    colour = Help.forwards tileColours
    getColour = Help.backwards tileColours
    asJson object = Json.Map.fromList ["name" .= Factorio.name object]
    categorize _ = Factorio.Tile
    nearest = Help.closestTo Factorio.colour [minBound .. maxBound]

tileNames :: Map NewTile Text
tileNames = Map.fromList $ do
    key <- [minBound .. maxBound]
    pure (key, newTileName key)

tileColours :: Map NewTile PixelRGB8
tileColours = Map.fromList $ do
    key <- [minBound .. maxBound]
    pure (key, newTileColour key)
