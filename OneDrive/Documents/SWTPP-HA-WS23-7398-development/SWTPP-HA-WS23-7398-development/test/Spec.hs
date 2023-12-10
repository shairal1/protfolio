
-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################
import Test.Hspec

import Board



main :: IO ()
main = hspec $ do
    tvalidateFEN
    testBuildBoard
    testpath
    {-testplayerWon-}
    tPos
    tCell
    tPlayer
    tDir 
   


tvalidateFEN :: Spec
tvalidateFEN = describe "Board.validateFEN" $ do
        it "" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` True
        it "" $ do
            validateFEN "ri,rr,rr,rr,rr,rr/,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False
        it "" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False
        it "" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False
        it "" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,5,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False
        it "" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,r,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` True
        it "" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,r,,/bb,bb,bb,bb,bb,bb" `shouldBe` True
        
        
testBuildBoard :: Spec
testBuildBoard = describe "Board.BuildBoard" $ do
        it "makes Board" $ do
             buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
        it "build empty board" $ do
            (buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,") `shouldBe` ([[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]] :: Board)
testpath:: Spec
testpath= describe "Board.path" $ do
        it "gives Path" $ do
             path  Pos {col = 'a', row = 1}  North  9 `shouldBe` [Pos {col = 'a', row = 1},Pos {col = 'a', row = 2},Pos {col = 'a', row = 3},Pos {col = 'a', row = 4},Pos {col = 'a', row = 5},Pos {col = 'a', row = 6},Pos {col = 'a', row = 5},Pos {col = 'a', row = 4},Pos {col = 'a', row = 3},Pos {col = 'a', row = 2}]
        
        it "gives Path" $ do
             path  Pos {col = 'a', row = 1} NorthEast  9 `shouldBe` [Pos {col = 'a', row = 1},Pos {col = 'b', row = 2},Pos {col = 'c', row = 3},Pos {col = 'd', row = 4},Pos {col = 'e', row = 5},Pos {col = 'f', row = 6},Pos {col = 'e', row = 5},Pos {col = 'd', row = 4},Pos {col = 'c', row = 3},Pos {col = 'b', row = 2}]
        
        it "gives Path" $ do
             path Pos {col = 'a', row = 1} East  9 `shouldBe` [Pos {col = 'a', row = 1},Pos {col = 'b', row = 1},Pos {col = 'c', row = 1},Pos {col = 'd', row = 1},Pos {col = 'e', row = 1},Pos {col = 'f', row = 1},Pos {col = 'e', row = 1},Pos {col = 'd', row = 1},Pos {col = 'c', row = 1},Pos {col = 'b', row = 1}]
        it "west" $ do
            path  Pos {col = 'f', row = 6}   West 9 `shouldBe` [Pos {col = 'f', row = 6},Pos {col = 'e', row = 6},Pos {col = 'd', row = 6},Pos {col = 'c', row = 6},Pos {col = 'b', row = 6},Pos {col = 'a', row = 6},Pos {col = 'b', row = 6},Pos {col = 'c', row = 6},Pos {col = 'd', row = 6},Pos {col = 'e', row = 6}]
        it "South" $ do 
            path Pos {col = 'f', row = 6}  South  9 `shouldBe` [Pos {col = 'f', row = 6},Pos {col = 'f', row = 5},Pos {col = 'f', row = 4},Pos {col = 'f', row = 3},Pos {col = 'f', row = 2},Pos {col = 'f', row = 1},Pos {col = 'f', row = 2},Pos {col = 'f', row = 3},Pos {col = 'f', row = 4},Pos {col = 'f', row = 5}]   
        it "Southwest" $ do 
            path Pos {col = 'f', row = 6}  SouthWest  9 `shouldBe` [Pos {col = 'f', row = 6},Pos {col = 'e', row = 5},Pos {col = 'd', row = 4},Pos {col = 'c', row = 3},Pos {col = 'b', row = 2},Pos {col = 'a', row = 1},Pos {col = 'b', row = 2},Pos {col = 'c', row = 3},Pos {col = 'd', row = 4},Pos {col = 'e', row = 5}]
        it "Southeast" $ do 
            path Pos {col = 'a', row = 6}  SouthEast  9 `shouldBe` [Pos {col = 'a', row = 6},Pos {col = 'b', row = 5},Pos {col = 'c', row = 4},Pos {col = 'd', row = 3},Pos {col = 'e', row = 2},Pos {col = 'f', row = 1},Pos {col = 'e', row = 2},Pos {col = 'd', row = 3},Pos {col = 'c', row = 4},Pos {col = 'b', row = 5}]
        it "northwest" $ do 
            path Pos {col = 'f', row = 1}  NorthWest  9 `shouldBe` [Pos {col = 'f', row = 1},Pos {col = 'e', row = 2},Pos {col = 'd', row = 3},Pos {col = 'c', row = 4},Pos {col = 'b', row = 5},Pos {col = 'a', row = 6},Pos {col = 'b', row = 5},Pos {col = 'c', row = 4},Pos {col = 'd', row = 3},Pos {col = 'e', row = 2}]


{-testplayerWon :: Spec
testplayerWon =
  describe "Board.playerWon" $ do
    it "No winner, all stacks have the correct player on top" $
      playerWon
        [ [Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Red, Red]]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Stack [Blue, Blue], Stack [Blue, Blue], Stack [Blue, Blue], Stack [Blue, Blue], Stack [Blue, Blue], Stack [Blue, Blue]]
        ] `shouldBe` Nothing

    it "Blue wins with all Blue stacks" $
      playerWon
        [ [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Stack [Blue, Blue], Stack [Blue, Blue], Stack [Blue, Blue], Stack [Blue, Blue], Stack [Blue, Blue], Stack [Blue, Blue]]
        ] `shouldBe` Just Blue

    it "Red wins with all Red stacks" $
      playerWon
        [ [Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Red, Red]]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        ] `shouldBe` Just Red

    it "No winner, one stack has different player on top" $
      playerWon
        [ [Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Red, Red], Stack [Blue, Red]]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        , [Empty, Empty, Empty, Empty, Empty, Empty]
        ] `shouldBe` Nothing
    {-it "start" $ do 
        playerWon  [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]] `shouldBe` Nothing  
    it "Just Blue" $ do 
        playerWon   [[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]] `shouldBe` Just Blue 
   {- it "one Red top"$ do 
        palyerWon   [[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Red,Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]  `shouldBe` Nothing -}
    it "just Red" $ do  playerWon  [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]] `shouldBe` Just Red-}
-}
---------------------------------------------------------------------------------------------------------------
--Instance test
tPos :: Spec 
tPos = describe "Eq Pos" $ do
    it "should consider Pos 'a' 1 equal to Pos 'a' 1" $ 
        Pos 'a' 1 `shouldBe` Pos 'a' 1

    it "should consider Pos 'a' 1 not equal to Pos 'b' 1" $ 
        Pos 'a' 1 `shouldNotBe` Pos 'b' 1

    it "should consider Pos 'a' 1 not equal to Pos 'a' 2" $ 
        Pos 'a' 1 `shouldNotBe` Pos 'a' 2

    
tCell :: Spec
tCell = describe "Eq Cell" $ do
    it "should consider Empty equal to Empty" $ do
      Empty `shouldBe` Empty

    it "should consider Stack [Blue] equal to Stack [Blue]" $ 
      Stack [Blue] `shouldBe` Stack [Blue]

    it "should consider Stack [Blue] not equal to Stack [Red]" $ 
      Stack [Blue] `shouldNotBe` Stack [Red]

    it "should consider Stack [Blue, Red] not equal to Stack [Blue]" $
      Stack [Blue, Red] `shouldNotBe` Stack [Blue]

    it "should consider Stack [Blue, Red] not equal to Empty" $
      Stack [Blue, Red] `shouldNotBe` Empty

    it "should consider Empty not equal to Stack [Blue]" $
      Empty `shouldNotBe` Stack [Blue]

    it "should consider Empty not equal to Stack []" $
      Empty `shouldNotBe` Stack []

    it "should consider Stack [] equal to Stack []" $
      Stack [] `shouldBe` Stack []

    it "should consider Stack [Blue, Red] not equal to Stack [Red, Blue]" $
      Stack [Blue, Red] `shouldNotBe` Stack [Red, Blue]
tPlayer ::Spec
tPlayer = do
  describe "Eq Player" $ do
    it "should consider Blue equal to Blue" $
      Blue `shouldBe` Blue

    it "should consider Red equal to Red" $
      Red `shouldBe` Red

    it "should consider Blue and Red not equal" $
      Blue `shouldNotBe` Red
tDir::Spec
tDir=describe "Dir" $ do
    it "shows each direction" $ do
      show North `shouldBe` "North"
      show NorthEast `shouldBe` "NorthEast"
      show East `shouldBe` "East"
      show SouthEast `shouldBe` "SouthEast"
      show South `shouldBe` "South"
      show SouthWest `shouldBe` "SouthWest"
      show West `shouldBe` "West"
      show NorthWest `shouldBe` "NorthWest"

