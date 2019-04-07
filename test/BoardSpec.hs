module BoardSpec (spec) where

import Board
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
    describe "Peace form ASCII" $ do
        it "black x" $ charpiece 'x' `shouldBe` Just Black
        it "black X" $ charpiece 'X' `shouldBe` Just Black
        it "white o" $ charpiece 'o' `shouldBe` Just White
        it "white 0" $ charpiece '0' `shouldBe` Just White
       -- it "empty ." $ charpiece '.' `shouldBe` Just Empty
        it "otherwise char" $ charpiece '#' `shouldBe` Nothing
    describe "Board from ASCII" $ do
        it "empty" $ charboard charpiece [] `shouldBe` board (0, 0)
        it "blank[3x3]" $ charboard charpiece ["...", "...", "..."] `shouldBe` board (3, 3)
        it "complicated[3x3]" $
            let asciiboard = ["ox.",
                              "xox",
                              ".x."]
                expectations = [((1,2),Black),((1,3),White),((2,1),Black),((2,2),White),((2,3),Black),((3,2),Black)]
            in charboard charpiece asciiboard `shouldBe` Board {width = 3, height = 3, content = Map.fromList expectations}
    describe "Board" $ do
        context "neighbours" $ do
            it "zero point" $
                neighbours (0, 0) `shouldBe` [(-1,0),(0,-1),(0,1),(1,0)]
            it "middle point" $
                neighbours (3, 3) `shouldBe` [(2,3),(3,2),(3,4),(4,3)]
            it "extreme point" $
                neighbours (19, 19) `shouldBe` [(18,19),(19,18),(19,20),(20,19)]
        context "inBounds" $ do
            it "outside of empty board" $ inBounds (0, 0) (board (0, 0)) `shouldBe` False
            it "outside" $ inBounds (0, 0) (board (3, 3)) `shouldBe` False
            it "extreme outside" $ inBounds (4, 4) (board (3, 3)) `shouldBe` False
            it "inside" $ inBounds (1, 1) (board (3, 3)) `shouldBe` True
            it "extreme inside" $ inBounds (3, 3) (board (3, 3)) `shouldBe` True
    describe "Board operations" $ do
        context "set" $ do
            it "commutativity" $
                let b1 = set (5, 5) (Just Black) $ set (7, 8) (Just White) $ board (19, 19)
                    b2 = set (7, 8) (Just White) $ set (5, 5) (Just Black) $ board (19, 19)
                in  b1 `shouldBe` b2
            it "Nothing" $ set (7, 8) Nothing (board (19, 19):: Board ()) `shouldBe` board (19, 19)
            it "remove" $
                set (3, 3) Nothing (set (3, 3) (Just Black) $ board (19, 19)) `shouldBe` board (19, 19)
        context "put" $
            it "commutativity" $
                let b1 = put (5, 5) (Just Black) $ put (7, 8) (Just White) $ board (19, 19)
                    b2 = put (7, 8) (Just White) $ put (5, 5) (Just Black) $ board (19, 19)
                in  b1 `shouldBe` b2
        context "get" $ do
            it "set a piece" $
                get (3, 3) (set (3, 3) (Just Black) (board (19, 19))) `shouldBe` Just Black
            it "set and removed a piece" $
                get (3, 3) (set (3, 3) Nothing $
                            set (3, 3) (Just Black) $ board (19, 19)) `shouldBe` Nothing
            it "unused piece" $
                get (3, 3) (set (8, 8) (Just Black) (board (19, 19))) `shouldBe` Nothing
