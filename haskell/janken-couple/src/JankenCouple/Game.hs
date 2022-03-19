module JankenCouple.Game where

import JankenCouple.Player

game :: (Player, Player) -> Int -> (Player, Player)
game players 0 = players
game (p1, p2) n =
  game (p1 `battleWith` p2) (n - 1)
