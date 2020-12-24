import Test.Tasty
import qualified D01P1Spec
import qualified D01P2Spec
import qualified D02P1Spec
import qualified D02P2Spec
import qualified D03P1Spec
import qualified D03P2Spec
import qualified D04P1Spec
import qualified D04P2Spec
import qualified D05P1Spec
import qualified D05P2Spec
import qualified D06P1Spec
import qualified D06P2Spec
import qualified D07P1Spec
import qualified D07P2Spec
import qualified D08P1Spec
import qualified D08P2Spec
import qualified D09P1Spec
import qualified D09P2Spec
import qualified D10P1Spec
import qualified D10P2Spec
import qualified D11P1Spec
import qualified D11P2Spec
import qualified D12P1Spec
import qualified D12P2Spec
import qualified D13P1Spec
import qualified D13P2Spec
import qualified D14P1Spec
import qualified D14P2Spec
import qualified D15Spec
import qualified D16P1Spec
import qualified D17P1Spec
import qualified D17P2Spec
import qualified D18P1Spec
import qualified D18P2Spec
import qualified D19P1Spec
import qualified D23P1Spec
import qualified D24P1Spec
import qualified D24P2Spec

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
          , D05P1Spec.tests
          , D05P2Spec.tests
          , D06P1Spec.tests
          , D06P2Spec.tests
          , D07P1Spec.tests
          , D07P2Spec.tests
          , D08P1Spec.tests
          , D08P2Spec.tests
          , D09P1Spec.tests
          , D09P2Spec.tests
          , D10P1Spec.tests
          , D10P2Spec.tests
          , D11P1Spec.tests
          , D11P2Spec.tests
          , D12P1Spec.tests
          , D12P2Spec.tests
          , D13P1Spec.tests
          , D13P2Spec.tests
          , D14P1Spec.tests
          , D14P2Spec.tests
          , D15Spec.tests
          , D16P1Spec.tests
          , D17P1Spec.tests
          , D17P2Spec.tests
          , D18P1Spec.tests
          , D18P2Spec.tests
          , D19P1Spec.tests
          , D23P1Spec.tests
          , D24P1Spec.tests
          , D24P2Spec.tests
      ]