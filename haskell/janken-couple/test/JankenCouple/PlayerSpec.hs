module JankenCouple.PlayerSpec where

-- import Prelude hiding (id)
import JankenCouple.Player as PlayerMod
import JankenCouple.Strategy
import Test.Hspec
import Control.Monad

playerWithOnlyStone = player "p1" "p1" OnlyStone
playerWithOnlyScissors = player "p2" "p2" OnlyScissors

testCases :: [(Player, Player, (Int, Int))]
testCases = [ (playerWithOnlyStone,    playerWithOnlyScissors, (1, 0)),
              (playerWithOnlyScissors, playerWithOnlyStone,    (0, 1)),
              (playerWithOnlyStone,    playerWithOnlyStone,    (0, 0))]

spec :: Spec
spec = do
  describe "Player" $ do
    forM_ testCases $ \(p1, p2, expectedWinCount) -> do
      describe ((PlayerMod.id p1) ++ " vs " ++ (PlayerMod.id p2)) $ do
        describe "win count of each" $ do
          it ("should be " ++ (show expectedWinCount)) $ do
            let (p1After, p2After) = p1 `battleWith` p2
            let actual = ((winCount p1After), (winCount p2After))

            actual `shouldBe` expectedWinCount
