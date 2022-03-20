module JankenCouple.Player( Player,
                            player,
                            battleWith,
                            id,
                            name,
                            winCount) where

import Prelude hiding (id)
import qualified JankenCouple.Hand as Hand
import JankenCouple.Strategy

data Player = Player { id :: String, name :: String, strategy :: Strategy, winCount :: Int }
  deriving (Eq, Show)

player :: String -> String -> Strategy -> Player
player id name strategy = Player id name strategy 0

battleWith :: Player -> Player -> (Player, Player)
battleWith p1 p2 =
  battleWith' p1Next p2Next (Hand.battleWith p1Hand p2Hand)
    where
      (p1Hand, p1Next) = selectHand' p1
      (p2Hand, p2Next) = selectHand' p2

battleWith' :: Player -> Player -> Hand.Result -> (Player, Player)
battleWith' p1 p2 Hand.Win  = ((notifyWin' p1), p2)
battleWith' p1 p2 Hand.Lose = (p1, (notifyWin' p2))
battleWith' p1 p2 Hand.Draw = (p1, p2)

selectHand' :: Player -> (Hand.Hand, Player)
selectHand' p = ( select current,
                  withStrategy' p (next current))
  where
    current = strategy p

notifyWin' :: Player -> Player
notifyWin' p = withWinCount' p ((winCount p) + 1)

withWinCount' :: Player -> Int -> Player
withWinCount'
  (Player { id = i, name = n, strategy = s }) w = Player i n s w

withStrategy' :: Player -> Strategy -> Player
withStrategy'
  (Player { id = i, name = n, winCount = w }) s = Player i n s w
