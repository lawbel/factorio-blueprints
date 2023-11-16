import Codec.Factorio qualified as Factorio
import Control.Arrow ((>>>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as Tasty.U
import Test.Tasty.QuickCheck ((===))
import Test.Tasty.QuickCheck qualified as Tasty.Q


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [propTests, unitTests]

propTests :: TestTree
propTests = Tasty.testGroup "property tests"
    [ Tasty.Q.testProperty "blueprintToJson inverts jsonToBlueprint" $
        let roundTrip = Factorio.jsonToBlueprint 0 >>> Factorio.blueprintToJson
        in  \json -> roundTrip json === Right json ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" []
