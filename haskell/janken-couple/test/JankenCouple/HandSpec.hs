module JankenCouple.HandSpec where

import JankenCouple.Hand
import Test.Hspec
import Control.Monad

testCases :: [(Hand, Hand, Result)]
testCases = [ (Stone, Stone, Draw),
              (Stone, Scissors, Win),
              (Stone, Paper, Lose),
              (Scissors, Stone, Lose),
              (Scissors, Scissors, Draw),
              (Scissors, Paper, Win),
              (Paper, Stone, Win),
              (Paper, Scissors, Lose),
              (Paper, Paper, Draw) ]

spec :: Spec
spec = do
  describe "Hand" $ do
    forM_ testCases $ \(h1, h2, expected) ->
      describe ((show h1) ++ " vs") $ do
        describe (show h2) $ do
          it ("should be " ++ (show expected)) $ do
            (h1 `battleWith` h2) `shouldBe` expected
