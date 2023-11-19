{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.Factorio.Vanilla
    ( Flooring
    , Floor(..)
    , All
    , Each(..)
    ) where

import Codec.Factorio (Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Helpers (closestTo, forwards, backwards)
import Codec.Picture (PixelRGB8)
import Codec.Picture qualified as Picture
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

data Flooring
data Floor = Stone | Concrete | Refined
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data All
data Each = MkFloor Floor | Wall | Gate
    deriving (Eq, Ord, Read, Show)

instance Palette Flooring where
    type Object Flooring = Floor
    name = forwards floorNames
    getName = backwards floorNames
    colour = forwards floorColours
    getColour = backwards floorColours
    asJson object = Json.Map.fromList ["name" .= Factorio.name object]
    categorize _ = Factorio.Tile
    nearest = closestTo Factorio.colour [minBound .. maxBound]

floorNames :: Map Floor Text
floorNames = Map.fromList
    [ (Stone, "stone-path")
    , (Concrete, "concrete")
    , (Refined, "refined-concrete") ]

floorColours :: Map Floor PixelRGB8
floorColours = Map.fromList
    [ (Stone, Picture.PixelRGB8 0x52 0x51 0x4A)
    , (Concrete, Picture.PixelRGB8 0x3A 0x3D 0x3A)
    , (Refined, Picture.PixelRGB8 0x31 0x31 0x29) ]

instance Palette All where
    type Object All = Each
    name = forwards allNames
    getName = backwards allNames
    colour = forwards allColours
    getColour = backwards allColours
    asJson =
        let wall = Json.Map.fromList ["name" .= Factorio.name Wall]
            gate = Json.Map.fromList
                [ "name" .= Factorio.name Gate
                , "direction" .= Json.Number 1 ]
        in  each Factorio.asJson wall gate
    categorize = each Factorio.categorize Factorio.Entity Factorio.Entity
    nearest = closestTo Factorio.colour $
        Wall : Gate : do
            flooring <- [minBound .. maxBound]
            pure $ MkFloor flooring

allNames :: Map Each Text
allNames = Map.fromList (wall : gate : assocs)
  where
    wall = (Wall, "stone-wall")
    gate = (Gate, "gate")
    assocs = first MkFloor <$> Map.toList floorNames

allColours :: Map Each PixelRGB8
allColours = Map.fromList (wall : gate : assocs)
  where
    wall = (Wall, Picture.PixelRGB8 0xCE 0xDB 0xCE)
    gate = (Gate, Picture.PixelRGB8 0x7B 0x7D 0x7B)
    assocs = first MkFloor <$> Map.toList floorColours

each :: (Floor -> a) -> a -> a -> Each -> a
each withFloor wall gate = \case
    MkFloor flooring -> withFloor flooring
    Wall -> wall
    Gate -> gate
