import qualified Control.Exception as Exception
import Data.Ratio ((%))
import Data.Text (Text)
import Test.Hspec

import Symkell.Symbolic
import Symkell.Symbolic.Limit
import qualified Symkell.SeriesSpec as SeriesSpec
import qualified Symkell.Symbolic.LimitSpec as LimitSpec

main :: IO ()
main = hspec $ do

  -- Add Limits tests
  describe "Symbolic Limits" $ do
    LimitSpec.spec

  -- Add Series tests
  describe "Series" $ do
    SeriesSpec.spec 