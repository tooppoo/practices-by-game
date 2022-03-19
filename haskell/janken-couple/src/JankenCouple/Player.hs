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
battleWith p1 p2 = battleWith' p1 p2 (Hand.battleWith p1Hand p2Hand)
  where
    Player { strategy = p1Strategy } = p1
    Player { strategy = p2Strategy } = p2
    p1Hand = select p1Strategy
    p2Hand = select p2Strategy

battleWith' :: Player -> Player -> Hand.Result -> (Player, Player)
battleWith' p1 p2 Hand.Win = ((notifyWin' p1), p2)
battleWith' p1 p2 Hand.Lose =  (p1, (notifyWin' p2))
battleWith' p1 p2 Hand.Draw =  (p1, p2)

notifyWin' :: Player -> Player
notifyWin' p = Player (id p) (name p) (strategy p) ((winCount p) + 1)
