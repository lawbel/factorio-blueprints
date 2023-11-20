{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.Factorio.Krastorio
    ( -- * Flooring
      Flooring
    , Floor(..)
    , floorNames
    , floorColours
      -- * Everything
    , All
    , Each(..)
    , eachName
    , eachColour
    ) where

import Codec.Factorio (Palette)
import Codec.Factorio.Vanilla qualified as Vanilla
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
data Floor = VanillaFloor Vanilla.Floor | BlackReinforced | WhiteReinforced
    deriving (Eq, Ord, Read, Show)

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
    nearest = closestTo Factorio.colour $
        BlackReinforced : WhiteReinforced : do
            vanilla <- [minBound .. maxBound]
            pure $ VanillaFloor vanilla

floorNames :: Map Floor Text
floorNames = vanilla <> krastorio
  where
    krastorio = Map.fromList
        [ (BlackReinforced, "kr-black-reinforced-plate")
        , (WhiteReinforced, "kr-white-reinforced-plate") ]
    vanilla = Map.mapKeys VanillaFloor Vanilla.floorNames

floorColours :: Map Floor PixelRGB8
floorColours = vanilla <> krastorio
  where
    krastorio = Map.fromList
        [ (BlackReinforced, Picture.PixelRGB8 0x29 0x28 0x29)
        , (WhiteReinforced, Picture.PixelRGB8 0x6B 0x6D 0x6B) ]
    vanilla = Map.mapKeys VanillaFloor Vanilla.floorColours

instance Palette All where
    type Object All = Each
    name = forwards eachName
    getName = backwards eachName
    colour = forwards eachColour
    getColour = backwards eachColour
    asJson =
        let wall = Json.Map.fromList ["name" .= Factorio.name Wall]
            gate = Json.Map.fromList
                [ "name" .= Factorio.name Gate
                , "direction" .= Json.Number 1 ]
        in  \case
                MkFloor flooring -> Factorio.asJson flooring
                Wall -> wall
                Gate -> gate
    categorize = \case
        MkFloor flooring -> Factorio.categorize flooring
        Wall -> Factorio.Entity
        Gate -> Factorio.Entity
    nearest = closestTo Factorio.colour $
        Wall : Gate : MkFloor BlackReinforced : MkFloor WhiteReinforced : do
            flooring <- [minBound .. maxBound]
            pure $ MkFloor $ VanillaFloor flooring

eachName :: Map Each Text
eachName = Map.fromList (wall : gate : assocs)
  where
    wall = (Wall, "stone-wall")
    gate = (Gate, "gate")
    assocs = first MkFloor <$> Map.toList floorNames

eachColour :: Map Each PixelRGB8
eachColour = Map.fromList (wall : gate : assocs)
  where
    wall = (Wall, Picture.PixelRGB8 0xCE 0xDB 0xCE)
    gate = (Gate, Picture.PixelRGB8 0x7B 0x7D 0x7B)
    assocs = first MkFloor <$> Map.toList floorColours
