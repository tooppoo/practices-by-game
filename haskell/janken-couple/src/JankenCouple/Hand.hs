module JankenCouple.Hand where

data Hand = Stone | Scissors | Paper
  deriving(Eq, Show)

data Result = Win | Lose | Draw
  deriving(Eq, Show)

battleWith :: Hand -> Hand -> Result
battleWith Stone Stone = Draw
battleWith Stone Scissors = Win
battleWith Stone Paper = Lose

battleWith Scissors Stone = Lose
battleWith Scissors Scissors = Draw
battleWith Scissors Paper = Win

battleWith Paper Stone = Win
battleWith Paper Scissors = Lose
battleWith Paper Paper = Draw
