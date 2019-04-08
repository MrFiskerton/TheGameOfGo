module BoardSpec (spec) where

import Board
import qualified Data.Map as Map
import qualified Data.Set as Set
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
    describe "Board groups" $ do
        context "neighbours" $ do
            it "zero point" $
                neighbours (0, 0) `shouldBe` [(-1,0),(0,-1),(0,1),(1,0)]
            it "middle point" $
                neighbours (3, 3) `shouldBe` [(2,3),(3,2),(3,4),(4,3)]
            it "extreme point" $
                neighbours (19, 19) `shouldBe` [(18,19),(19,18),(19,20),(20,19)]
        context "boundedNeighbours" $ do
            it "zero point" $
                boundedNeighbours (board (19, 19)) (0, 0) `shouldBe` []
            it "middle point" $
                boundedNeighbours (board (19, 19)) (3, 3) `shouldBe` [(2,3),(3,2),(3,4),(4,3)]
            it "extreme point" $
                boundedNeighbours (board (19, 19)) (19, 19) `shouldBe` [(18,19),(19,18)]
        context "connected" $ do
            it "sigle" $
                let b = charboard charpiece ["...", "x..", "..."]
                in connected (1, 2) b `shouldBe` Set.singleton (1, 2)
            it "group" $
                let b = charboard charpiece ["x.x",
                                             "xxx",
                                             "..x"]
                in connected (3, 3) b `shouldBe` Set.fromList [(1,2),(1,3),(2,2),(3,1),(3,2),(3,3)]
            it "complex group" $
                let b = charboard charpiece ["..x",
                                             "x.x",
                                             "..x"]
                in connected (3, 3) b `shouldBe` Set.fromList [(3,1),(3,2),(3,3)]
        context "liberties" $ do
            it "dead" $
                let b = charboard charpiece [".xx",
                                             "xox",
                                             ".x."]
                in liberties (2, 2) b `shouldBe` Set.empty
            it "complex" $
                let b = charboard charpiece ["xxx",
                                             "xx.",
                                             ".x."]
                in liberties (2, 2) b `shouldBe` Set.fromList [(1,1),(3,1),(3,2)]
        context "kill" $ do
            it "simple" $
                let b = charboard charpiece [".xx",
                                             "xox",
                                             ".x."]
                    b' = set (2, 2) Nothing b
                in kill (2, 2) b `shouldBe` b'
            it "complex" $
                let b = charboard charpiece [".xo",
                                             "xoo",
                                             ".xx"]
                    b' = set (3, 2) Nothing $
                         set (2, 2) Nothing $
                         set (3, 3) Nothing b
                in kill (2, 2) b `shouldBe` b'
        context "capture" $ do
            it "simple" $
                let b = charboard charpiece [".xx",
                                             "xox",
                                             ".x."]
                    b' = set (2, 2) Nothing b
                in capture (2, 2) b `shouldBe` (1, b')
            it "false positive" $
                let b = charboard charpiece [".xx",
                                             "xoo",
                                             ".x."]
                in capture (2, 2) b `shouldBe` (0, b)
            it "complex" $
                let b = charboard charpiece [".xo",
                                             "xoo",
                                             ".xx"]
                    b' = set (3, 2) Nothing $
                         set (2, 2) Nothing $
                         set (3, 3) Nothing b
                in capture (2, 2) b `shouldBe` (3, b')
    describe "Board check" $ do
        context "inBounds" $ do
            it "outside of empty board" $ inBounds (board (0, 0)) (0, 0)`shouldBe` False
            it "outside" $ inBounds (board (3, 3)) (0, 0) `shouldBe` False
            it "extreme outside" $ inBounds (board (3, 3)) (4, 4) `shouldBe` False
            it "inside" $ inBounds (board (3, 3)) (1, 1) `shouldBe` True
            it "extreme inside" $ inBounds (board (3, 3)) (3, 3) `shouldBe` True
        context "isDead" $ do
            it "dead" $
                let b = charboard charpiece [".xx",
                                             "xox",
                                             ".x."]
                in isDead b (2, 2) `shouldBe` True
            it "alive" $
                let b = charboard charpiece [".xx",
                                             "xo.",
                                             ".x."]
                in isDead b (2, 2) `shouldBe` False
        context "isConnected" $ do
            it "positive" $
                let b = charboard charpiece [".xx",
                                             "xox",
                                             ".x."]
                in isConnected b (2, 3) (3, 2) `shouldBe` True
            it "negative" $
                let b = charboard charpiece [".xx",
                                             "xox",
                                             ".x."]
                in isConnected b (1, 2) (3, 3) `shouldBe` False
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
        context "remove" $  do
            it "non empty" $
                remove (3, 3) (set (3, 3) (Just Black) $ board (19, 19)) `shouldBe` board (19, 19)
            it "empty" $
                remove (3, 3) (set (2, 2) (Just Black) $ board (19, 19)) `shouldBe` Board {width = 19, height = 19, content = Map.fromList [((2,2),Black)]}
        context "move" $ do
            it "capture" $
                let b = charboard charpiece [".xo..",
                                             "xoox.",
                                             ".xx.."]
                    b' = set (4, 3) (Just Black) $
                         kill (2, 2) b
                in move (4, 3) Black b `shouldBe` (3, b')
            it "put" $
                let b = charboard charpiece ["xxxo.",
                                             ".oox.",
                                             "xo..."]
                    b' = put (3, 1) Black b
                in move (3, 1) Black b `shouldBe` (0, b')
            it "double capture" $
                let b = charboard charpiece ["xxxo.",
                                             ".oo..",
                                             "xo..."]
                    b' = put (1, 2) White $
                         kill (1, 1) $
                         kill (1, 3) b
                in move (1, 2) White b `shouldBe` (4, b')
            it "suicide" $
                let b = charboard charpiece ["xxxo.",
                                             ".oo..",
                                             "oo..."]
                in move (1, 2) Black b `shouldBe` (0, b)
            it "double suicide" $
                let b = charboard charpiece ["xxxo.",
                                             ".oo..",
                                             "xo..."]
                in move (1, 2) Black b `shouldBe` (0, b)
