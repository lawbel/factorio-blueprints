{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
import Codec.Picture qualified as Picture
import Data.Aeson ((.=))
import Data.Aeson.KeyMap qualified as Json.Map
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

newtype AllTile = MkTile (Either Base.Tile (Either Base.Hazard NewTile))
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype Palette
    deriving Bounded via
        EitherIsBounded Base.Tile (EitherIsBounded Base.Hazard NewTile)
    deriving Enum via
        EitherIsEnum Base.Tile (EitherIsEnum Base.Hazard NewTile)

-- | We omit some of the possible tiles from this data type, as they have
-- the same colour as other tiles which are included.
data NewTile
      -- * gravel
    = CoalGravel
    | CopperOreGravel
    | IronOreGravel
    | StoneGravel

      -- * wood
    | WoodFloor

      -- * snow
    | FrozenSnow0
    | FrozenSnow1
    | FrozenSnow2
    | FrozenSnow3
    | FrozenSnow4
    | FrozenSnow5
    | FrozenSnow6
    | FrozenSnow7
    | FrozenSnow8
    | FrozenSnow9

      -- * mineral aubergine
    | MinAubergineDirt1
    | MinAubergineDirt2
    | MinAubergineDirt3
    | MinAubergineDirt4
   -- MinAubergineDirt5
   -- MinAubergineDirt6
    | MinAubergineSand1
    | MinAubergineSand2
   -- MinAubergineSand3

      -- * mineral beige
    | MinBeigeDirt1
    | MinBeigeDirt2
    | MinBeigeDirt3
    | MinBeigeDirt4
    | MinBeigeDirt5
    | MinBeigeDirt6
    | MinBeigeSand1
   -- MinBeigeSand2
   -- MinBeigeSand3

      -- * mineral black
    | MinBlackDirt1
    | MinBlackDirt2
   -- MinBlackDirt3
   -- MinBlackDirt4
    | MinBlackDirt5
    | MinBlackDirt6
    | MinBlackSand1
    | MinBlackSand2
   -- MinBlackSand3

      -- * mineral black
    | MinBrownDirt1
    | MinBrownDirt2
    | MinBrownDirt3
    | MinBrownDirt4
   -- MinBrownDirt5
    | MinBrownDirt6
    | MinBrownSand1
    | MinBrownSand2
    | MinBrownSand3

      -- * mineral cream
    | MinCreamDirt1
    | MinCreamDirt2
    | MinCreamDirt3
    | MinCreamDirt4
    | MinCreamDirt5
    | MinCreamDirt6
    | MinCreamSand1
    | MinCreamSand2
    | MinCreamSand3

      -- * mineral dusty rose
    | MinDustyRoseDirt1
    | MinDustyRoseDirt2
    | MinDustyRoseDirt3
    | MinDustyRoseDirt4
   -- MinDustyRoseDirt5
    | MinDustyRoseDirt6
    | MinDustyRoseSand1
   -- MinDustyRoseSand2
    | MinDustyRoseSand3

      -- * mineral grey
    | MinGreyDirt1
    | MinGreyDirt2
    | MinGreyDirt3
   -- MinGreyDirt4
    | MinGreyDirt5
    | MinGreyDirt6
    | MinGreySand1
   -- MinGreySand2
   -- MinGreySand3

      -- * mineral purple
    | MinPurpleDirt1
    | MinPurpleDirt2
    | MinPurpleDirt3
    | MinPurpleDirt4
    | MinPurpleDirt5
    | MinPurpleDirt6
    | MinPurpleSand1
    | MinPurpleSand2
    | MinPurpleSand3

      -- * mineral red
    | MinRedDirt1
    | MinRedDirt2
    | MinRedDirt3
    | MinRedDirt4
    | MinRedDirt5
    | MinRedDirt6
    | MinRedSand1
    | MinRedSand2
    | MinRedSand3

      -- * mineral tan
    | MinTanDirt1
    | MinTanDirt2
    | MinTanDirt3
    | MinTanDirt4
    | MinTanDirt5
    | MinTanDirt6
    | MinTanSand1
    | MinTanSand2
    | MinTanSand3

      -- * mineral violet
    | MinVioletDirt1
    | MinVioletDirt2
    | MinVioletDirt3
    | MinVioletDirt4
    | MinVioletDirt5
    | MinVioletDirt6
    | MinVioletSand1
    | MinVioletSand2
    | MinVioletSand3

      -- * mineral white
    | MinWhiteDirt1
    | MinWhiteDirt2
   -- MinWhiteDirt3
   -- MinWhiteDirt4
    | MinWhiteDirt5
    | MinWhiteDirt6
    | MinWhiteSand1
    | MinWhiteSand2
    | MinWhiteSand3

      -- * vegetation grass
    | VegBlueGrass1
    | VegBlueGrass2
    | VegOrangeGrass1
    | VegOrangeGrass2
    | VegPurpleGrass1
    | VegPurpleGrass2
    | VegGreenGrass1
    | VegGreenGrass2
    | VegGreenGrass3
    | VegGreenGrass4
    | VegMauveGrass1
    | VegMauveGrass2
    | VegRedGrass1
   -- VegRedGrass2
    | VegOliveGrass1
    | VegOliveGrass2
    | VegTurquoiseGrass1
    | VegTurquoiseGrass2
    | VegVioletGrass1
    | VegVioletGrass2
    | VegYellowGrass1
    | VegYellowGrass2

      -- * volcanic
    | VolBlueHeat1
   -- VolBlueHeat2
    | VolBlueHeat3
    | VolBlueHeat4
    | VolOrangeHeat1
   -- VolOrangeHeat2
    | VolOrangeHeat3
    | VolOrangeHeat4
    | VolGreenHeat1
   -- VolGreenHeat2
    | VolGreenHeat3
    | VolGreenHeat4
    | VolPurpleHeat1
   -- VolPurpleHeat2
    | VolPurpleHeat3
    | VolPurpleHeat4

      -- * hazard concrete
    | PaintDanger
    | PaintEmergency
    | PaintCaution
    | PaintRadiation
    | PaintDefect
    | PaintOperations
    | PaintSafety
   -- RefinedDanger
   -- RefinedEmergency
   -- RefinedCaution
   -- RefinedRadiation
   -- RefinedDefect
   -- RefinedOperations
   -- RefinedSafety

      -- * plain concrete
    | AcidRefined
    | BlackRefined
    | BlueRefined
    | BrownRefined
    | CyanRefined
    | GreenRefined
    | OrangeRefined
    | PinkRefined
    | PurpleRefined
    | RedRefined
    | YellowRefined
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

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
    pure (key, name key)
  where
    -- we implement the map like this, so the compiler can help us
    -- catch any missed cases
    name = \case
        CoalGravel         -> "dect-coal-gravel"
        CopperOreGravel    -> "dect-copper-ore-gravel"
        IronOreGravel      -> "dect-iron-ore-gravel"
        StoneGravel        -> "dect-stone-gravel"

        WoodFloor          -> "dect-wood-floor"

        FrozenSnow0        -> "frozen-snow-0"
        FrozenSnow1        -> "frozen-snow-1"
        FrozenSnow2        -> "frozen-snow-2"
        FrozenSnow3        -> "frozen-snow-3"
        FrozenSnow4        -> "frozen-snow-4"
        FrozenSnow5        -> "frozen-snow-5"
        FrozenSnow6        -> "frozen-snow-6"
        FrozenSnow7        -> "frozen-snow-7"
        FrozenSnow8        -> "frozen-snow-8"
        FrozenSnow9        -> "frozen-snow-9"

        MinAubergineDirt1  -> "mineral-aubergine-dirt-1"
        MinAubergineDirt2  -> "mineral-aubergine-dirt-2"
        MinAubergineDirt3  -> "mineral-aubergine-dirt-3"
        MinAubergineDirt4  -> "mineral-aubergine-dirt-4"
     -- MinAubergineDirt5  -> "mineral-aubergine-dirt-5"
     -- MinAubergineDirt6  -> "mineral-aubergine-dirt-6"
        MinAubergineSand1  -> "mineral-aubergine-sand-1"
        MinAubergineSand2  -> "mineral-aubergine-sand-2"
     -- MinAubergineSand3  -> "mineral-aubergine-sand-3"

        MinBeigeDirt1      -> "mineral-beige-dirt-1"
        MinBeigeDirt2      -> "mineral-beige-dirt-2"
        MinBeigeDirt3      -> "mineral-beige-dirt-3"
        MinBeigeDirt4      -> "mineral-beige-dirt-4"
        MinBeigeDirt5      -> "mineral-beige-dirt-5"
        MinBeigeDirt6      -> "mineral-beige-dirt-6"
        MinBeigeSand1      -> "mineral-beige-sand-1"
     -- MinBeigeSand2      -> "mineral-beige-sand-2"
     -- MinBeigeSand3      -> "mineral-beige-sand-3"

        MinBlackDirt1      -> "mineral-black-dirt-1"
        MinBlackDirt2      -> "mineral-black-dirt-2"
     -- MinBlackDirt3      -> "mineral-black-dirt-3"
     -- MinBlackDirt4      -> "mineral-black-dirt-4"
        MinBlackDirt5      -> "mineral-black-dirt-5"
        MinBlackDirt6      -> "mineral-black-dirt-6"
        MinBlackSand1      -> "mineral-black-sand-1"
        MinBlackSand2      -> "mineral-black-sand-2"
     -- MinBlackSand3      -> "mineral-black-sand-3"

        MinBrownDirt1      -> "mineral-brown-dirt-1"
        MinBrownDirt2      -> "mineral-brown-dirt-2"
        MinBrownDirt3      -> "mineral-brown-dirt-3"
        MinBrownDirt4      -> "mineral-brown-dirt-4"
     -- MinBrownDirt5      -> "mineral-brown-dirt-5"
        MinBrownDirt6      -> "mineral-brown-dirt-6"
        MinBrownSand1      -> "mineral-brown-sand-1"
        MinBrownSand2      -> "mineral-brown-sand-2"
        MinBrownSand3      -> "mineral-brown-sand-3"

        MinCreamDirt1      -> "mineral-cream-dirt-1"
        MinCreamDirt2      -> "mineral-cream-dirt-2"
        MinCreamDirt3      -> "mineral-cream-dirt-3"
        MinCreamDirt4      -> "mineral-cream-dirt-4"
        MinCreamDirt5      -> "mineral-cream-dirt-5"
        MinCreamDirt6      -> "mineral-cream-dirt-6"
        MinCreamSand1      -> "mineral-cream-sand-1"
        MinCreamSand2      -> "mineral-cream-sand-2"
        MinCreamSand3      -> "mineral-cream-sand-3"

        MinDustyRoseDirt1  -> "mineral-dustyrose-dirt-1"
        MinDustyRoseDirt2  -> "mineral-dustyrose-dirt-2"
        MinDustyRoseDirt3  -> "mineral-dustyrose-dirt-3"
        MinDustyRoseDirt4  -> "mineral-dustyrose-dirt-4"
     -- MinDustyRoseDirt5  -> "mineral-dustyrose-dirt-5"
        MinDustyRoseDirt6  -> "mineral-dustyrose-dirt-6"
        MinDustyRoseSand1  -> "mineral-dustyrose-sand-1"
     -- MinDustyRoseSand2  -> "mineral-dustyrose-sand-2"
        MinDustyRoseSand3  -> "mineral-dustyrose-sand-3"

        MinGreyDirt1       -> "mineral-grey-dirt-1"
        MinGreyDirt2       -> "mineral-grey-dirt-2"
        MinGreyDirt3       -> "mineral-grey-dirt-3"
     -- MinGreyDirt4       -> "mineral-grey-dirt-4"
        MinGreyDirt5       -> "mineral-grey-dirt-5"
        MinGreyDirt6       -> "mineral-grey-dirt-6"
        MinGreySand1       -> "mineral-grey-sand-1"
     -- MinGreySand2       -> "mineral-grey-sand-2"
     -- MinGreySand3       -> "mineral-grey-sand-3"

        MinPurpleDirt1     -> "mineral-purple-dirt-1"
        MinPurpleDirt2     -> "mineral-purple-dirt-2"
        MinPurpleDirt3     -> "mineral-purple-dirt-3"
        MinPurpleDirt4     -> "mineral-purple-dirt-4"
        MinPurpleDirt5     -> "mineral-purple-dirt-5"
        MinPurpleDirt6     -> "mineral-purple-dirt-6"
        MinPurpleSand1     -> "mineral-purple-sand-1"
        MinPurpleSand2     -> "mineral-purple-sand-2"
        MinPurpleSand3     -> "mineral-purple-sand-3"

        MinRedDirt1        -> "mineral-red-dirt-1"
        MinRedDirt2        -> "mineral-red-dirt-2"
        MinRedDirt3        -> "mineral-red-dirt-3"
        MinRedDirt4        -> "mineral-red-dirt-4"
        MinRedDirt5        -> "mineral-red-dirt-5"
        MinRedDirt6        -> "mineral-red-dirt-6"
        MinRedSand1        -> "mineral-red-sand-1"
        MinRedSand2        -> "mineral-red-sand-2"
        MinRedSand3        -> "mineral-red-sand-3"

        MinTanDirt1        -> "mineral-tan-dirt-1"
        MinTanDirt2        -> "mineral-tan-dirt-2"
        MinTanDirt3        -> "mineral-tan-dirt-3"
        MinTanDirt4        -> "mineral-tan-dirt-4"
        MinTanDirt5        -> "mineral-tan-dirt-5"
        MinTanDirt6        -> "mineral-tan-dirt-6"
        MinTanSand1        -> "mineral-tan-sand-1"
        MinTanSand2        -> "mineral-tan-sand-2"
        MinTanSand3        -> "mineral-tan-sand-3"

        MinVioletDirt1     -> "mineral-violet-dirt-1"
        MinVioletDirt2     -> "mineral-violet-dirt-2"
        MinVioletDirt3     -> "mineral-violet-dirt-3"
        MinVioletDirt4     -> "mineral-violet-dirt-4"
        MinVioletDirt5     -> "mineral-violet-dirt-5"
        MinVioletDirt6     -> "mineral-violet-dirt-6"
        MinVioletSand1     -> "mineral-violet-sand-1"
        MinVioletSand2     -> "mineral-violet-sand-2"
        MinVioletSand3     -> "mineral-violet-sand-3"

        MinWhiteDirt1      -> "mineral-white-dirt-1"
        MinWhiteDirt2      -> "mineral-white-dirt-2"
     -- MinWhiteDirt3      -> "mineral-white-dirt-3"
     -- MinWhiteDirt4      -> "mineral-white-dirt-4"
        MinWhiteDirt5      -> "mineral-white-dirt-5"
        MinWhiteDirt6      -> "mineral-white-dirt-6"
        MinWhiteSand1      -> "mineral-white-sand-1"
        MinWhiteSand2      -> "mineral-white-sand-2"
        MinWhiteSand3      -> "mineral-white-sand-3"

        VegBlueGrass1      -> "vegetation-blue-grass-1"
        VegBlueGrass2      -> "vegetation-blue-grass-2"
        VegOrangeGrass1    -> "vegetation-orange-grass-1"
        VegOrangeGrass2    -> "vegetation-orange-grass-2"
        VegPurpleGrass1    -> "vegetation-purple-grass-1"
        VegPurpleGrass2    -> "vegetation-purple-grass-2"
        VegGreenGrass1     -> "vegetation-green-grass-1"
        VegGreenGrass2     -> "vegetation-green-grass-2"
        VegGreenGrass3     -> "vegetation-green-grass-3"
        VegGreenGrass4     -> "vegetation-green-grass-4"
        VegMauveGrass1     -> "vegetation-mauve-grass-1"
        VegMauveGrass2     -> "vegetation-mauve-grass-2"
        VegRedGrass1       -> "vegetation-red-grass-1"
     -- VegRedGrass2       -> "vegetation-red-grass-2"
        VegOliveGrass1     -> "vegetation-olive-grass-1"
        VegOliveGrass2     -> "vegetation-olive-grass-2"
        VegTurquoiseGrass1 -> "vegetation-turquoise-grass-1"
        VegTurquoiseGrass2 -> "vegetation-turquoise-grass-2"
        VegVioletGrass1    -> "vegetation-violet-grass-1"
        VegVioletGrass2    -> "vegetation-violet-grass-2"
        VegYellowGrass1    -> "vegetation-yellow-grass-1"
        VegYellowGrass2    -> "vegetation-yellow-grass-2"

        VolBlueHeat1       -> "volcanic-blue-heat-1"
     -- VolBlueHeat2       -> "volcanic-blue-heat-2"
        VolBlueHeat3       -> "volcanic-blue-heat-3"
        VolBlueHeat4       -> "volcanic-blue-heat-4"
        VolOrangeHeat1     -> "volcanic-orange-heat-1"
     -- VolOrangeHeat2     -> "volcanic-orange-heat-2"
        VolOrangeHeat3     -> "volcanic-orange-heat-3"
        VolOrangeHeat4     -> "volcanic-orange-heat-4"
        VolGreenHeat1      -> "volcanic-green-heat-1"
     -- VolGreenHeat2      -> "volcanic-green-heat-2"
        VolGreenHeat3      -> "volcanic-green-heat-3"
        VolGreenHeat4      -> "volcanic-green-heat-4"
        VolPurpleHeat1     -> "volcanic-purple-heat-1"
     -- VolPurpleHeat2     -> "volcanic-purple-heat-2"
        VolPurpleHeat3     -> "volcanic-purple-heat-3"
        VolPurpleHeat4     -> "volcanic-purple-heat-4"

        PaintDanger        -> "dect-paint-danger-left"
        PaintEmergency     -> "dect-paint-emergency-left"
        PaintCaution       -> "dect-paint-caution-left"
        PaintRadiation     -> "dect-paint-radiation-left"
        PaintDefect        -> "dect-paint-defect-left"
        PaintOperations    -> "dect-paint-operations-left"
        PaintSafety        -> "dect-paint-safety-left"
     -- RefinedDanger      -> "dect-paint-refined-danger-left"
     -- RefinedEmergency   -> "dect-paint-refined-emergency-left"
     -- RefinedCaution     -> "dect-paint-refined-caution-left"
     -- RefinedRadiation   -> "dect-paint-refined-radiation-left"
     -- RefinedDefect      -> "dect-paint-refined-defect-left"
     -- RefinedOperations  -> "dect-paint-refined-operations-left"
     -- RefinedSafety      -> "dect-paint-refined-safety-left"

        AcidRefined        -> "acid-refined-concrete"
        BlackRefined       -> "black-refined-concrete"
        BlueRefined        -> "blue-refined-concrete"
        BrownRefined       -> "brown-refined-concrete"
        CyanRefined        -> "cyan-refined-concrete"
        GreenRefined       -> "green-refined-concrete"
        OrangeRefined      -> "orange-refined-concrete"
        PinkRefined        -> "pink-refined-concrete"
        PurpleRefined      -> "purple-refined-concrete"
        RedRefined         -> "red-refined-concrete"
        YellowRefined      -> "yellow-refined-concrete"

tileColours :: Map NewTile PixelRGB8
tileColours = Map.fromList $ do
    key <- [minBound .. maxBound]
    pure (key, colour key)
  where
    -- we implement the map like this, so the compiler can help us
    -- catch any missed cases
    colour = \case
        CoalGravel         -> Picture.PixelRGB8 0x52 0x55 0x52
        CopperOreGravel    -> Picture.PixelRGB8 0xA4 0x75 0x73
        IronOreGravel      -> Picture.PixelRGB8 0x73 0x8A 0xA4
        StoneGravel        -> Picture.PixelRGB8 0x94 0x92 0x94

        WoodFloor          -> Picture.PixelRGB8 0x8C 0x45 0x10

        FrozenSnow0        -> Picture.PixelRGB8 0xDE 0xDF 0xE6
        FrozenSnow1        -> Picture.PixelRGB8 0xD6 0xDB 0xE6
        FrozenSnow2        -> Picture.PixelRGB8 0xCE 0xCE 0xD6
        FrozenSnow3        -> Picture.PixelRGB8 0xD6 0xD2 0xDE
        FrozenSnow4        -> Picture.PixelRGB8 0xC5 0xC6 0xD6
        FrozenSnow5        -> Picture.PixelRGB8 0x9C 0xAA 0xB5
        FrozenSnow6        -> Picture.PixelRGB8 0x84 0x92 0xA4
        FrozenSnow7        -> Picture.PixelRGB8 0x8C 0x9E 0xAD
        FrozenSnow8        -> Picture.PixelRGB8 0xA4 0xB2 0xBD
        FrozenSnow9        -> Picture.PixelRGB8 0x9C 0xAE 0xBD

        MinAubergineDirt1  -> Picture.PixelRGB8 0x3A 0x39 0x4A
        MinAubergineDirt2  -> Picture.PixelRGB8 0x31 0x28 0x3A
        MinAubergineDirt3  -> Picture.PixelRGB8 0x29 0x28 0x31
        MinAubergineDirt4  -> Picture.PixelRGB8 0x31 0x2D 0x3A
     -- MinAubergineDirt5  -> Picture.PixelRGB8 0x31 0x2D 0x3A
     -- MinAubergineDirt6  -> Picture.PixelRGB8 0x29 0x28 0x31
        MinAubergineSand1  -> Picture.PixelRGB8 0x3A 0x35 0x4A
        MinAubergineSand2  -> Picture.PixelRGB8 0x3A 0x35 0x52
     -- MinAubergineSand3  -> Picture.PixelRGB8 0x3A 0x35 0x4A

        MinBeigeDirt1      -> Picture.PixelRGB8 0x84 0x75 0x5A
        MinBeigeDirt2      -> Picture.PixelRGB8 0x6B 0x61 0x4A
        MinBeigeDirt3      -> Picture.PixelRGB8 0x63 0x59 0x4A
        MinBeigeDirt4      -> Picture.PixelRGB8 0x5A 0x55 0x42
        MinBeigeDirt5      -> Picture.PixelRGB8 0x5A 0x51 0x42
        MinBeigeDirt6      -> Picture.PixelRGB8 0x52 0x4D 0x3A
        MinBeigeSand1      -> Picture.PixelRGB8 0x8C 0x7D 0x63
     -- MinBeigeSand2      -> Picture.PixelRGB8 0x8C 0x7D 0x63
     -- MinBeigeSand3      -> Picture.PixelRGB8 0x84 0x75 0x5A

        MinBlackDirt1      -> Picture.PixelRGB8 0x3A 0x39 0x31
        MinBlackDirt2      -> Picture.PixelRGB8 0x29 0x2D 0x29
     -- MinBlackDirt3      -> Picture.PixelRGB8 0x29 0x2D 0x29
     -- MinBlackDirt4      -> Picture.PixelRGB8 0x29 0x2D 0x29
        MinBlackDirt5      -> Picture.PixelRGB8 0x21 0x28 0x21
        MinBlackDirt6      -> Picture.PixelRGB8 0x21 0x24 0x21
        MinBlackSand1      -> Picture.PixelRGB8 0x3A 0x39 0x3A
        MinBlackSand2      -> Picture.PixelRGB8 0x31 0x39 0x3A
     -- MinBlackSand3      -> Picture.PixelRGB8 0x3A 0x39 0x31

        MinBrownDirt1      -> Picture.PixelRGB8 0x73 0x51 0x3A
        MinBrownDirt2      -> Picture.PixelRGB8 0x5A 0x3D 0x29
        MinBrownDirt3      -> Picture.PixelRGB8 0x4A 0x35 0x29
        MinBrownDirt4      -> Picture.PixelRGB8 0x42 0x31 0x21
     -- MinBrownDirt5      -> Picture.PixelRGB8 0x42 0x31 0x21
        MinBrownDirt6      -> Picture.PixelRGB8 0x3A 0x28 0x19
        MinBrownSand1      -> Picture.PixelRGB8 0x7B 0x55 0x42
        MinBrownSand2      -> Picture.PixelRGB8 0x84 0x59 0x42
        MinBrownSand3      -> Picture.PixelRGB8 0x73 0x4D 0x3A

        MinCreamDirt1      -> Picture.PixelRGB8 0xA4 0x9A 0x6B
        MinCreamDirt2      -> Picture.PixelRGB8 0x8C 0x81 0x52
        MinCreamDirt3      -> Picture.PixelRGB8 0x7B 0x71 0x52
        MinCreamDirt4      -> Picture.PixelRGB8 0x73 0x6D 0x52
        MinCreamDirt5      -> Picture.PixelRGB8 0x6B 0x69 0x4A
        MinCreamDirt6      -> Picture.PixelRGB8 0x6B 0x65 0x42
        MinCreamSand1      -> Picture.PixelRGB8 0xAD 0xAA 0x73
        MinCreamSand2      -> Picture.PixelRGB8 0xB5 0xAE 0x7B
        MinCreamSand3      -> Picture.PixelRGB8 0xA4 0x96 0x6B

        MinDustyRoseDirt1  -> Picture.PixelRGB8 0x63 0x51 0x4A
        MinDustyRoseDirt2  -> Picture.PixelRGB8 0x4A 0x3D 0x3A
        MinDustyRoseDirt3  -> Picture.PixelRGB8 0x4A 0x41 0x3A
        MinDustyRoseDirt4  -> Picture.PixelRGB8 0x42 0x3D 0x3A
     -- MinDustyRoseDirt5  -> Picture.PixelRGB8 0x42 0x3D 0x3A
        MinDustyRoseDirt6  -> Picture.PixelRGB8 0x42 0x35 0x31
        MinDustyRoseSand1  -> Picture.PixelRGB8 0x63 0x4D 0x4A
     -- MinDustyRoseSand2  -> Picture.PixelRGB8 0x63 0x4D 0x4A
        MinDustyRoseSand3  -> Picture.PixelRGB8 0x63 0x4D 0x42

        MinGreyDirt1       -> Picture.PixelRGB8 0x73 0x79 0x73
        MinGreyDirt2       -> Picture.PixelRGB8 0x63 0x61 0x5A
        MinGreyDirt3       -> Picture.PixelRGB8 0x63 0x65 0x63
     -- MinGreyDirt4       -> Picture.PixelRGB8 0x63 0x65 0x63
        MinGreyDirt5       -> Picture.PixelRGB8 0x5A 0x5D 0x5A
        MinGreyDirt6       -> Picture.PixelRGB8 0x5A 0x59 0x5A
        MinGreySand1       -> Picture.PixelRGB8 0x73 0x75 0x73
     -- MinGreySand2       -> Picture.PixelRGB8 0x73 0x75 0x73
     -- MinGreySand3       -> Picture.PixelRGB8 0x73 0x75 0x73

        MinPurpleDirt1     -> Picture.PixelRGB8 0x4A 0x49 0x73
        MinPurpleDirt2     -> Picture.PixelRGB8 0x3A 0x39 0x63
        MinPurpleDirt3     -> Picture.PixelRGB8 0x31 0x35 0x52
        MinPurpleDirt4     -> Picture.PixelRGB8 0x3A 0x3D 0x63
        MinPurpleDirt5     -> Picture.PixelRGB8 0x42 0x41 0x63
        MinPurpleDirt6     -> Picture.PixelRGB8 0x3A 0x39 0x5A
        MinPurpleSand1     -> Picture.PixelRGB8 0x42 0x45 0x84
        MinPurpleSand2     -> Picture.PixelRGB8 0x42 0x41 0x84
        MinPurpleSand3     -> Picture.PixelRGB8 0x42 0x41 0x7B

        MinRedDirt1        -> Picture.PixelRGB8 0x7B 0x41 0x31
        MinRedDirt2        -> Picture.PixelRGB8 0x63 0x31 0x21
        MinRedDirt3        -> Picture.PixelRGB8 0x5A 0x31 0x29
        MinRedDirt4        -> Picture.PixelRGB8 0x4A 0x24 0x19
        MinRedDirt5        -> Picture.PixelRGB8 0x5A 0x31 0x29
        MinRedDirt6        -> Picture.PixelRGB8 0x42 0x20 0x19
        MinRedSand1        -> Picture.PixelRGB8 0x84 0x35 0x29
        MinRedSand2        -> Picture.PixelRGB8 0x84 0x39 0x29
        MinRedSand3        -> Picture.PixelRGB8 0x7B 0x39 0x29

        MinTanDirt1        -> Picture.PixelRGB8 0x8C 0x69 0x42
        MinTanDirt2        -> Picture.PixelRGB8 0x73 0x55 0x31
        MinTanDirt3        -> Picture.PixelRGB8 0x5A 0x45 0x29
        MinTanDirt4        -> Picture.PixelRGB8 0x5A 0x41 0x29
        MinTanDirt5        -> Picture.PixelRGB8 0x52 0x3D 0x21
        MinTanDirt6        -> Picture.PixelRGB8 0x52 0x39 0x21
        MinTanSand1        -> Picture.PixelRGB8 0x9C 0x79 0x4A
        MinTanSand2        -> Picture.PixelRGB8 0x9C 0x7D 0x4A
        MinTanSand3        -> Picture.PixelRGB8 0x8C 0x69 0x42

        MinVioletDirt1     -> Picture.PixelRGB8 0x6B 0x45 0x63
        MinVioletDirt2     -> Picture.PixelRGB8 0x5A 0x35 0x52
        MinVioletDirt3     -> Picture.PixelRGB8 0x4A 0x31 0x4A
        MinVioletDirt4     -> Picture.PixelRGB8 0x52 0x35 0x4A
        MinVioletDirt5     -> Picture.PixelRGB8 0x5A 0x3D 0x52
        MinVioletDirt6     -> Picture.PixelRGB8 0x4A 0x31 0x4A
        MinVioletSand1     -> Picture.PixelRGB8 0x6B 0x3D 0x6B
        MinVioletSand2     -> Picture.PixelRGB8 0x73 0x3D 0x6B
        MinVioletSand3     -> Picture.PixelRGB8 0x6B 0x3D 0x63

        MinWhiteDirt1      -> Picture.PixelRGB8 0xA4 0xA6 0xA4
        MinWhiteDirt2      -> Picture.PixelRGB8 0x8C 0x8E 0x8C
     -- MinWhiteDirt3      -> Picture.PixelRGB8 0x8C 0x8E 0x8C
     -- MinWhiteDirt4      -> Picture.PixelRGB8 0x8C 0x8E 0x8C
        MinWhiteDirt5      -> Picture.PixelRGB8 0x84 0x85 0x84
        MinWhiteDirt6      -> Picture.PixelRGB8 0x84 0x81 0x84
        MinWhiteSand1      -> Picture.PixelRGB8 0xB5 0xB2 0xAD
        MinWhiteSand2      -> Picture.PixelRGB8 0xB5 0xB6 0xB5
        MinWhiteSand3      -> Picture.PixelRGB8 0xA4 0xA2 0x9C

        VegBlueGrass1      -> Picture.PixelRGB8 0x29 0x31 0x4A
        VegBlueGrass2      -> Picture.PixelRGB8 0x29 0x31 0x3A
        VegOrangeGrass1    -> Picture.PixelRGB8 0x63 0x39 0x10
        VegOrangeGrass2    -> Picture.PixelRGB8 0x63 0x35 0x10
        VegPurpleGrass1    -> Picture.PixelRGB8 0x42 0x20 0x52
        VegPurpleGrass2    -> Picture.PixelRGB8 0x31 0x18 0x4A
        VegGreenGrass1     -> Picture.PixelRGB8 0x42 0x3D 0x10
        VegGreenGrass2     -> Picture.PixelRGB8 0x4A 0x3D 0x19
        VegGreenGrass3     -> Picture.PixelRGB8 0x42 0x39 0x19
        VegGreenGrass4     -> Picture.PixelRGB8 0x42 0x35 0x19
        VegMauveGrass1     -> Picture.PixelRGB8 0x31 0x2D 0x4A
        VegMauveGrass2     -> Picture.PixelRGB8 0x31 0x28 0x42
        VegRedGrass1       -> Picture.PixelRGB8 0x5A 0x18 0x10
     -- VegRedGrass2       -> Picture.PixelRGB8 0x5A 0x18 0x10
        VegOliveGrass1     -> Picture.PixelRGB8 0x5A 0x49 0x10
        VegOliveGrass2     -> Picture.PixelRGB8 0x5A 0x45 0x19
        VegTurquoiseGrass1 -> Picture.PixelRGB8 0x31 0x39 0x29
        VegTurquoiseGrass2 -> Picture.PixelRGB8 0x3A 0x39 0x29
        VegVioletGrass1    -> Picture.PixelRGB8 0x4A 0x1C 0x31
        VegVioletGrass2    -> Picture.PixelRGB8 0x42 0x18 0x29
        VegYellowGrass1    -> Picture.PixelRGB8 0x73 0x59 0x08
        VegYellowGrass2    -> Picture.PixelRGB8 0x6B 0x51 0x10

        VolBlueHeat1       -> Picture.PixelRGB8 0x19 0x24 0x21
     -- VolBlueHeat2       -> Picture.PixelRGB8 0x19 0x24 0x21
        VolBlueHeat3       -> Picture.PixelRGB8 0x19 0x28 0x31
        VolBlueHeat4       -> Picture.PixelRGB8 0x10 0x45 0x7B
        VolOrangeHeat1     -> Picture.PixelRGB8 0x21 0x20 0x19
     -- VolOrangeHeat2     -> Picture.PixelRGB8 0x21 0x20 0x19
        VolOrangeHeat3     -> Picture.PixelRGB8 0x31 0x1C 0x19
        VolOrangeHeat4     -> Picture.PixelRGB8 0x7B 0x18 0x10
        VolGreenHeat1      -> Picture.PixelRGB8 0x21 0x24 0x19
     -- VolGreenHeat2      -> Picture.PixelRGB8 0x21 0x24 0x19
        VolGreenHeat3      -> Picture.PixelRGB8 0x19 0x35 0x19
        VolGreenHeat4      -> Picture.PixelRGB8 0x19 0x7D 0x10
        VolPurpleHeat1     -> Picture.PixelRGB8 0x21 0x1C 0x21
     -- VolPurpleHeat2     -> Picture.PixelRGB8 0x21 0x1C 0x21
        VolPurpleHeat3     -> Picture.PixelRGB8 0x21 0x18 0x31
        VolPurpleHeat4     -> Picture.PixelRGB8 0x3A 0x10 0x7B

        PaintDanger        -> Picture.PixelRGB8 0xCE 0x4D 0x4A
        PaintEmergency     -> Picture.PixelRGB8 0xDE 0x59 0x63
        PaintCaution       -> Picture.PixelRGB8 0xDE 0x8E 0x42
        PaintRadiation     -> Picture.PixelRGB8 0xDE 0x8E 0xC5
        PaintDefect        -> Picture.PixelRGB8 0x73 0x7D 0xFF
        PaintOperations    -> Picture.PixelRGB8 0x5A 0x5D 0x5A
        PaintSafety        -> Picture.PixelRGB8 0x9C 0xC6 0x63
     -- RefinedDanger      -> Picture.PixelRGB8 0xCE 0x4D 0x4A
     -- RefinedEmergency   -> Picture.PixelRGB8 0xDE 0x59 0x63
     -- RefinedCaution     -> Picture.PixelRGB8 0xDE 0x8E 0x42
     -- RefinedRadiation   -> Picture.PixelRGB8 0xDE 0x8E 0xC5
     -- RefinedDefect      -> Picture.PixelRGB8 0x73 0x7D 0xFF
     -- RefinedOperations  -> Picture.PixelRGB8 0x5A 0x5D 0x5A
     -- RefinedSafety      -> Picture.PixelRGB8 0x9C 0xC6 0x63

        AcidRefined        -> Picture.PixelRGB8 0x8C 0xC2 0x29
        BlackRefined       -> Picture.PixelRGB8 0x19 0x18 0x19
        BlueRefined        -> Picture.PixelRGB8 0x21 0x8A 0xE6
        BrownRefined       -> Picture.PixelRGB8 0x4A 0x1C 0x00
        CyanRefined        -> Picture.PixelRGB8 0x42 0xC2 0xB5
        GreenRefined       -> Picture.PixelRGB8 0x10 0xC2 0x29
        OrangeRefined      -> Picture.PixelRGB8 0xDE 0x7D 0x21
        PinkRefined        -> Picture.PixelRGB8 0xEE 0x61 0x84
        PurpleRefined      -> Picture.PixelRGB8 0x7B 0x1C 0xAD
        RedRefined         -> Picture.PixelRGB8 0xCE 0x04 0x00
        YellowRefined      -> Picture.PixelRGB8 0xD6 0xAA 0x10
