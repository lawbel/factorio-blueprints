{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Factorio.Vanilla
    ( Flooring
    , Floor(..)
    , All
    , Each(..)
    ) where

import Codec.Factorio (Palette)
import Codec.Factorio qualified as Factorio
import Codec.Factorio.Helpers (closestTo)
import Codec.Picture qualified as Picture
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.Aeson.KeyMap qualified as Json.Map

data Flooring
data Floor = Stone | Concrete | Refined
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data All
data Each = MkFloor Floor | Wall | Gate
    deriving (Eq, Ord, Read, Show)

instance Palette Flooring where
    type Object Flooring = Floor
    name = \case
        Stone -> "stone-path"
        Concrete -> "concrete"
        Refined -> "refined-concrete"
    search = \case
        "stone-path" -> Just Stone
        "concrete" -> Just Concrete
        "refined-concrete" -> Just Refined
        _ -> Nothing
    colour = \case
        Stone -> Picture.PixelRGB8 0x52 0x51 0x4A
        Concrete -> Picture.PixelRGB8 0x3A 0x3D 0x3A
        Refined -> Picture.PixelRGB8 0x31 0x31 0x29
    toJson object = Json.Map.fromList ["name" .= Factorio.name object]
    categorize _ = Factorio.Tile
    nearest = closestTo Factorio.colour [minBound .. maxBound]

each :: (Floor -> a) -> a -> a -> Each -> a
each withFloor wall gate = \case
    MkFloor flooring -> withFloor flooring
    Wall -> wall
    Gate -> gate

instance Palette All where
    type Object All = Each
    name = each Factorio.name "stone-wall" "gate"
    search = \case
        "stone-wall" -> Just Wall
        "gate" -> Just Gate
        txt -> MkFloor <$> Factorio.search @Flooring txt
    colour =
        let wall = Picture.PixelRGB8 0xCE 0xDB 0xCE
            gate = Picture.PixelRGB8 0x7B 0x7D 0x7B
        in  each Factorio.colour wall gate
    toJson =
        let wall = Json.Map.fromList ["name" .= Factorio.name Wall]
            gate = Json.Map.fromList
                [ "name" .= Factorio.name Gate
                , "direction" .= Json.Number 1 ]
        in  each Factorio.toJson wall gate
    categorize = each Factorio.categorize Factorio.Entity Factorio.Entity
    nearest = closestTo Factorio.colour $
        Wall : Gate : do
            flooring <- [minBound .. maxBound]
            pure $ MkFloor flooring
