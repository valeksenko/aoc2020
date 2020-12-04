import Test.Tasty
import qualified D01P1Spec
import qualified D01P2Spec
import qualified D02P1Spec
import qualified D02P2Spec
import qualified D03P1Spec
import qualified D03P2Spec
import qualified D04P1Spec
import qualified D04P2Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
      [
            D01P1Spec.tests
          , D01P2Spec.tests
          , D02P1Spec.tests
          , D02P2Spec.tests
          , D03P1Spec.tests
          , D03P2Spec.tests
          , D04P1Spec.tests
          , D04P2Spec.tests
      ]