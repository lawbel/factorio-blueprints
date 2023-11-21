{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Factorio.Vanilla
    ( -- * Tiles
      Tile(..)
    , tileNames
    , tileColours
      -- * Entities
    , Entity(..)
    , entityName
    , entityColour
      -- * Everything
    , All(..)
    ) where

import Codec.Factorio (Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Helpers (closestTo, forwards, backwards)
import Codec.Picture (PixelRGB8)
import Codec.Picture qualified as Picture
import Control.Arrow ((&&&))
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

data Tile = Stone | Concrete | Refined
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

data Entity = Wall | Gate
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

newtype All = MkAll (Either Tile Entity)
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype Palette

instance Palette Tile where
    name = forwards tileNames
    getName = backwards tileNames
    colour = forwards tileColours
    getColour = backwards tileColours
    asJson object = Json.Map.fromList ["name" .= Factorio.name object]
    categorize _ = Factorio.Tile
    nearest = closestTo Factorio.colour [minBound .. maxBound]

tileNames :: Map Tile Text
tileNames = Map.fromList $ fmap (id &&& name) [minBound .. maxBound]
  where
    name = \case
        Stone    -> "stone-path"
        Concrete -> "concrete"
        Refined  -> "refined-concrete"

tileColours :: Map Tile PixelRGB8
tileColours = Map.fromList $ fmap (id &&& colour) [minBound .. maxBound]
  where
    colour = \case
        Stone    -> Picture.PixelRGB8 0x52 0x51 0x4A
        Concrete -> Picture.PixelRGB8 0x3A 0x3D 0x3A
        Refined  -> Picture.PixelRGB8 0x31 0x31 0x29

instance Palette Entity where
    name = forwards entityName
    getName = backwards entityName
    colour = forwards entityColour
    getColour = backwards entityColour
    asJson = \case
        Wall -> Json.Map.fromList
            [ "name" .= Factorio.name Wall ]
        Gate -> Json.Map.fromList
            [ "name" .= Factorio.name Gate
            , "direction" .= Json.Number 1 ]
    categorize _ = Factorio.Entity
    nearest = closestTo Factorio.colour [minBound .. maxBound]

entityName :: Map Entity Text
entityName = Map.fromList $ fmap (id &&& name) [minBound .. maxBound]
  where
    name = \case
        Wall -> "stone-wall"
        Gate -> "gate"

entityColour :: Map Entity PixelRGB8
entityColour = Map.fromList $ fmap (id &&& colour) [minBound .. maxBound]
  where
    colour = \case
        Wall -> Picture.PixelRGB8 0xCE 0xDB 0xCE
        Gate -> Picture.PixelRGB8 0x7B 0x7D 0x7B
