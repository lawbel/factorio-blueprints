{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

import Codec.Factorio qualified as Factorio
import Codec.Factorio.Internal qualified as Help
import Codec.Factorio.Base qualified as Base
import Codec.Picture qualified as Picture
import Control.Arrow ((>>>))
import Data.String.Interpolate (i)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as Tasty.U
import Test.Tasty.QuickCheck ((===))
import Test.Tasty.QuickCheck qualified as Tasty.Q

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [properties, units]

properties :: TestTree
properties = Tasty.testGroup "property tests"
    [ Tasty.Q.testProperty "blueprintToJson inverts jsonToBlueprint" $
        let roundTrip = Factorio.jsonToBlueprint 0 >>> Factorio.blueprintToJson
        in  \json -> roundTrip json === Right json ]

units :: TestTree
units = Tasty.testGroup "unit tests" [reflexive, specialCases]

reflexive :: TestTree
reflexive = Tasty.testGroup "reflexivity" $ do
    option <- allPalette
    pure $ Tasty.U.testCase [i|#{option} colour closest to itself|] $
        Help.closestTo Factorio.colour allPalette (Factorio.colour option)
            @?= option

specialCases :: TestTree
specialCases = Tasty.testGroup "special cases"
    [ Tasty.U.testCase "solar colour closest to refined" $
        Help.closestTo Factorio.colour allPalette solar
            @?= Base.MkAll (Left Base.Refined) ]
  where
    solar = Picture.PixelRGB8 0x19 0x20 0x21

allPalette :: [Base.All]
allPalette = [minBound .. maxBound]
