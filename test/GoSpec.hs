module GoSpec (spec) where

import qualified Board as B
import qualified Data.Map as Map
import Data.Ratio
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
        context "gosession" $ do
            it "zero komi" $
                let g = gosession 19 19 [B.Black, B.White] Map.empty
                in  komi g `shouldBe` Map.fromList [(B.Black, 0), (B.White, 0)]
            it "gameover if there isn't players" $
                isGameOver (gosession 19 19 [] Map.empty :: GoGameSession B.Piece) `shouldBe` True
        context "move" $ do
            it "put pieces sequence" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (1, 2) >>= move (2, 2)
                           >>= move (1, 1) >>= move (2, 3)
                           >>= move (3, 2)
                    b = B.charboard B.charpiece [".o.",
                                                 "xox",
                                                 "x.."]
                in fmap board g' `shouldBe` Right b
            it "capture" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 3) >>= move (3, 3)
                           >>= move (3, 2) >>= move (2, 2)
                    b = B.charboard B.charpiece [".x.",
                                                 ".ox",
                                                 "..."]
                in fmap board g' `shouldBe` Right b
            it "out of bounds" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 3) >>= move (2, 300)
                in g' `shouldBe` Left OutOfBounds
            it "suicide" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 3) >>= move (2, 2)
                           >>= move (3, 2) >>= move (3, 3)
                    -- b = B.charboard B.charpiece [".x.",
                    --                              ".ox",
                    --                              "..."]
                in g' `shouldBe` Left Suicide
            it "ko" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 1) >>= move (3, 1)
                           >>= move (1, 2) >>= move (2, 2)
                    b' = Right $ B.charboard B.charpiece ["...",
                                                          "xo.",
                                                          ".xo"]
                    g'' = g' >>= move (2, 3) >>= move (1, 1)
                    b'' = Right $ B.charboard B.charpiece [".x.",
                                                           "xo.",
                                                           "o.o"]
                    g''' = g'' >>= move (2, 1)
                    b''' = Left Ko
                in map (fmap board) [g', g'', g'''] `shouldBe` [b', b'', b''']
            it "occupied same" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 3) >>= move (2, 2)
                           >>= move (2, 3)
                    -- b = B.charboard B.charpiece [".x.",
                    --                              ".o.",
                    --                              "..."]
                in g' `shouldBe` Left Occupied
            it "occupied other" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 3) >>= move (2, 2)
                           >>= move (2, 2)
                    -- b = B.charboard B.charpiece [".x.",
                    --                              ".o.",
                    --                              "..."]
                in g' `shouldBe` Left Occupied
            it "game over: pass pass" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 3) >>= move (2, 2)
                           >>= pass >>= pass
                           >>= move (1, 1)
                in g' `shouldBe` Left GameOver
            it "game over: pass x 2 the same person" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 3) >>= move (2, 2)
                           >>= pass >>= move (1, 1)
                           >>= pass >>= move (3, 3)
                in fmap isGameOver g' `shouldBe` Right False
            it "game over: pass non-consecutively" $
                let g = return $ gosession 3 3 [B.Black, B.White] Map.empty
                    g' = g >>= move (2, 3) >>= pass
                           >>= move (3, 1) >>= move (1, 1)
                           >>= pass >>= move (3, 3)
                in fmap isGameOver g' `shouldBe` Right False
