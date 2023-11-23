{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Factorio.Base
    ( -- * Tiles
      Tile(..)
    , tileNames
    , tileColours
      -- * Hazard concrete
    , Hazard(..)
    , hazardNames
    , hazardColours
      -- * Entities
    , Entity(..)
    , entityNames
    , entityColours
      -- * Everything
    , All(..)
    ) where

import Codec.Factorio (Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Internal (EitherBounded, EitherEnum)
import Codec.Factorio.Internal qualified as Help
import Codec.Picture (PixelRGB8)
import Codec.Picture qualified as Picture
import Control.Arrow ((&&&))
import Data.Aeson ((.=))
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

data Tile = Stone | Concrete | Refined
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

data Hazard = HazardConcrete | HazardRefined
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

data Entity = Wall | Gate
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

newtype All = MkAll (Either Tile Entity)
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype Palette
    deriving Bounded via EitherBounded Tile Entity
    deriving Enum via EitherEnum Tile Entity

instance Palette Tile where
    name = Help.forwards tileNames
    getName = Help.backwards tileNames
    colour = Help.forwards tileColours
    getColour = Help.backwards tileColours
    asJson object = Json.Map.fromList ["name" .= Factorio.name object]
    categorize _ = Factorio.Tile
    nearest = Help.closestTo Factorio.colour [minBound .. maxBound]

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

instance Palette Hazard where
    name = Help.forwards hazardNames
    getName = Help.backwards hazardNames
    colour = Help.forwards hazardColours
    getColour = Help.backwards hazardColours
    asJson object = Json.Map.fromList ["name" .= Factorio.name object]
    categorize _ = Factorio.Tile
    nearest = Help.closestTo Factorio.colour [minBound .. maxBound]

hazardNames :: Map Hazard Text
hazardNames = Map.fromList $ fmap (id &&& name) [minBound .. maxBound]
  where
    name = \case
        HazardConcrete -> "hazard-concrete-left"
        HazardRefined  -> "refined-hazard-concrete-left"

hazardColours :: Map Hazard PixelRGB8
hazardColours = Map.fromList $ fmap (id &&& colour) [minBound .. maxBound]
  where
    colour = \case
        HazardConcrete -> Picture.PixelRGB8 0xB5 0x8E 0x21
        HazardRefined  -> Picture.PixelRGB8 0x73 0x5D 0x19

instance Palette Entity where
    name = Help.forwards entityNames
    getName = Help.backwards entityNames
    colour = Help.forwards entityColours
    getColour = Help.backwards entityColours
    asJson object = Json.Map.fromList ["name" .= Factorio.name object]
    categorize _ = Factorio.Entity
    nearest = Help.closestTo Factorio.colour [minBound .. maxBound]

entityNames :: Map Entity Text
entityNames = Map.fromList $ fmap (id &&& name) [minBound .. maxBound]
  where
    name = \case
        Wall -> "stone-wall"
        Gate -> "gate"

entityColours :: Map Entity PixelRGB8
entityColours = Map.fromList $ fmap (id &&& colour) [minBound .. maxBound]
  where
    colour = \case
        Wall -> Picture.PixelRGB8 0xCE 0xDB 0xCE
        Gate -> Picture.PixelRGB8 0x7B 0x7D 0x7B
