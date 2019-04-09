module GoSpec (spec) where

import qualified Board as B
import Control.Monad
import Data.Either
import qualified Data.Map as Map
import Data.Ratio
import qualified Data.Set as Set
import Go
import Test.Hspec

spec :: Spec
spec = do
    describe "Score" $
        context "gamescore" $ do
            it "example" $
                let k = 6 + 1 % 2
                    g = gosession 19 19 [B.Black, B.White] $ Map.singleton B.White k
                    expected = Map.fromList [ (B.Black, Score 0 0 0 0)
                                            , (B.White, Score 0 0 k k) ]
                in gamescore g `shouldBe` expected
            it "simple game" $
                let k = 3
                    g = return $ gosession 3 3 [B.Black, B.White] $ Map.singleton B.White k
                    g' = g >>= move (1, 1) >>= move (2, 1)
                        >>= move (3, 2) >>= move (1, 2)
                        >>= move (2, 3)
                    b = B.charboard B.charpiece [".x.",
                                                "o.x",
                                                ".o."]
                    expected = Map.fromList [ (B.Black, Score 1 0 0 1)
                                            , (B.White, Score 1 1 3 5) ]
                in (fmap board g', fmap gamescore g') `shouldBe` (Right b, Right expected)
    describe "Session" $ do
        context "gosession" $
            it "zero komi" $
                let g = gosession 19 19 [B.Black, B.White] Map.empty
                in  komi g `shouldBe` Map.fromList [(B.Black, 0), (B.White, 0)]
        context "move" $
            it "put pieces sequence" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (1, 2) >>= move (2, 2)
                           >>= move (1, 1) >>= move (2, 3)
                           >>= move (3, 2)
                    b = B.charboard B.charpiece [".o.",
                                                 "xox",
                                                 "x.."]
                in fmap board g' `shouldBe` Right b

