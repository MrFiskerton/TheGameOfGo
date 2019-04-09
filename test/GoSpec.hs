module GoSpec (spec) where

import qualified Board as B
import qualified Data.Map as Map
import Data.Ratio
import qualified Data.Set as Set
import Go
import Test.Hspec

spec :: Spec
spec =
    describe "Score" $ do
        context "gamescore" $
            it "only komi" $
                let k = 6 + 1 % 2
                    g = gosession 19 19 [B.Black, B.White] $ Map.singleton B.White k
                    expected = Map.fromList [ (B.Black, Score 0 0 0 0)
                                            , (B.White, Score 0 0 k k) ]
                in gamescore g `shouldBe` expected
        context "score" $
            it "only komi" $
                let k = 6 + 1 % 2
                    g = gosession 19 19 [B.Black, B.White] $ Map.singleton B.White k
                    expected = Score 0 0 k k
                in score g B.White `shouldBe` expected


