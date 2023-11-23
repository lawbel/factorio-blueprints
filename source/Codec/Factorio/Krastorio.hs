{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Factorio.Krastorio
    ( -- * New Tiles
      NewTile(..)
    , tileNames
    , tileColours
      -- * All Tiles
    , AllTile(..)
      -- * Everything
    , All(..)
    ) where

import Codec.Factorio (Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Base qualified as Base
import Codec.Factorio.Internal (EitherEnum, EitherBounded)
import Codec.Factorio.Internal qualified as Help
import Codec.Picture (PixelRGB8)
import Codec.Picture qualified as Picture
import Control.Arrow ((&&&))
import Data.Aeson ((.=))
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

data NewTile = BlackReinforced | WhiteReinforced
    deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

newtype AllTile = MkAllTile (Either Base.Tile NewTile)
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype Palette
    deriving Bounded via EitherBounded Base.Tile NewTile
    deriving Enum via EitherEnum Base.Tile NewTile

newtype All = MkAll (Either AllTile Base.Entity)
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype Palette
    deriving Bounded via EitherBounded AllTile Base.Entity
    deriving Enum via EitherEnum AllTile Base.Entity

instance Palette NewTile where
    name = Help.forwards tileNames
    getName = Help.backwards tileNames
    colour = Help.forwards tileColours
    getColour = Help.backwards tileColours
    asJson object = Json.Map.fromList ["name" .= Factorio.name object]
    categorize _ = Factorio.Tile
    nearest = Help.closestTo Factorio.colour [minBound .. maxBound]

tileNames :: Map NewTile Text
tileNames = Map.fromList $ fmap (id &&& name) [minBound .. maxBound]
  where
    name = \case
        BlackReinforced -> "kr-black-reinforced-plate"
        WhiteReinforced -> "kr-white-reinforced-plate"

tileColours :: Map NewTile PixelRGB8
tileColours = Map.fromList $ fmap (id &&& name) [minBound .. maxBound]
  where
    name = \case
        BlackReinforced -> Picture.PixelRGB8 0x29 0x28 0x29
        WhiteReinforced -> Picture.PixelRGB8 0x6B 0x6D 0x6B
