module Main where

import JankenCouple.Hand
import JankenCouple.Game
import JankenCouple.Player
import JankenCouple.Strategy

main :: IO ()
main = do
  let p1 = player "player-1" "John Doe" OnlyStone
  let p2 = player "player-2" "Una Nancy Owen" (orderedFrom Paper)

  let (p1After, p2After) = game (p1, p2) 7

  let r1 = (name p1After, winCount p1After)
  let r2 = (name p2After, winCount p2After)

  putStrLn $ show (r1, r2)
